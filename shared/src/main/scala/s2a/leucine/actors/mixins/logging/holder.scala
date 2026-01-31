package s2a.leucine.actors

/**
 * MIT License
 *
 * Copyright (c) 2023 Ruud Vlaming
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/


/**
 * Class that collects and temporary holds the log entries. Entries are collected in a list,
 * which is fast so it ensures quick return to the users code. However, it may not be suited
 * for large amounts of logs due to the scattered nature of memory use and missed cache hits
 * at garbage collection. The class also keeps a local copy of the active level and timing
 * settings. Any entry can be (runtime) checked prior to entry construction and storage.
 * For internal use only. The class is not thread safe by design, so synchronize your calls
 * or ensure that you are using this holder in one thread by other means. The latter is comes
 * natural when used as a thread local variable. */
private class LogHolder(val actorPath: String, val level: ActorLogger.Level, val timing: ActorLogger.Timing) :
  import ActorLogger.{Level, Entry}
  import Static.Kind
  import LogHolder.{Hold, ActorFilter, minStart, maxStart}

  /* Although we produce a Hold at the end of the LogHolders lifetime , we choose not to
   * do the work inside the Hold itself, for it would require construction and destruction
   * of a Hold instance for each log entry. The current is approach is less clean but
   * also uses less resources. */

  /* Variables to keep track of the lowest and highest stored indices in this instance. */
  private var min: Long = minStart
  private var max: Long = maxStart

  /** Managed container for all log entries. */
  private var entries: List[Entry] = Nil

  /** Check if the holder contains any entries */
  private[actors] def isEmpty: Boolean = entries.isEmpty

  /**
   * Construct an log entry based on the given data and add a timestamp, an index,  a thread
   * name, timing and actor path name (if available) */
  private[actors] def make(level: Level, sourceKind: Kind, sourcePath: String, message: String): Entry =
    Entry(actorPath,level,timing,sourceKind,sourcePath,message)

  /** Test if an log entry with the given level would pass for the current settings. */
  private[actors] def pass(level: Level, pathFilter: ActorFilter): Boolean =
    level <= this.level && pathFilter(level,actorPath)

  /** Add a log entry to the list. */
  private[actors] def add(entry: Entry): Unit =
    if min > entry.index then min = entry.index
    if max < entry.index then max = entry.index
    entries ::= entry

  /** Get a copy of the current log list. */
  private[actors] def get: Hold[Entry] = Hold(min,entries,max)

  /** Clear the current log list. */
  private[actors] def clear(): Unit =
    min = minStart
    max = maxStart
    entries = Nil



/**
 * Object that contains the public methods to add a log to the relevant log list.
 * For internal use only */
private object LogHolder :
  import ActorLogger.{Level, Entry}
  import Static.Kind

  /* Helper type to locally pass filters around */
  private[actors] type ActorFilter = (Level,String) => Boolean

  /* Universal start values to determine the highest and lowest index values */
  private inline val minStart = Long.MaxValue
  private inline val maxStart = 0

  /**
   * Holder class for multiple log entries. The values min and max indicate the lowest and highest
   * index values present within the entries in the container. The entries themselves may or may not
   * be ordered. Values of min and max are 0 (and thus invalid) if there are no entries. */
  case class Hold[E](val min: Long, val entries: List[E], val max: Long) :
    /**
     * Determine how many slots must be reserved if we want to store all entries in an array.
     * Note that there may be less entries present in the container. */
    def width: Int = if entries.isEmpty then 0 else (max - min).toInt + 1
    /**
     * Merge this container with a container of containers. If this entries list is empty, no empty
     * list will be added. */
    def + (hold: Hold[List[E]]): Hold[List[E]] = if entries.isEmpty then hold else
      val min = if (this.min < hold.min) then this.min else hold.min
      val max = if (this.max > hold.max) then this.max else hold.max
      Hold(min,this.entries :: hold.entries,max)

  object Hold :
    /* The empty holder with no contents. */
    def empty[E] = Hold[E](minStart,Nil,maxStart)


  /** Helper class to temporarily store log entries that may not yet be spooled. */
  private[actors] case class Store(val start: Long, val entries: IArray[Entry | Null])

  private[actors] object Store :
    /* The empty holder with no contents. */
    def empty = Store(0,IArray.empty)


  /**
   * Make a new log entry of the current thread, if that thread has an active local container. If not,
   * the entry is created by the global container. Returns the constructed entry for further processing.
   * If feed is true, the entry will be directly stored on the log queue. */
  private[actors] def entry(feed: Boolean, level: Level, actorFilter: ActorFilter, sourceKind: Kind, sourcePath: String, message: => String): Option[Entry] =
    /* Try to construct the entry on the thread local container */
    LogLocal.entry(feed,level,actorFilter,sourceKind,sourcePath,message) match
      /* This was a success, return the entry */
      case Right(entry) => Some(entry)
      /* The container was there but the entry could not be made due to insufficient log level. */
      case Left(true)   => None
      /* The container was not there, we must try the global container. */
      case Left(false)  => LogGlobal.entry(feed,level,actorFilter,sourceKind,sourcePath,message) match
        /* This was a success, return the entry */
        case Right(entry) => Some(entry)
        /* The entry could not be made due to insufficient log level or absent global logger. */
        case Left(_)      => None


  /** Collect all the logs that are present and empty the containers */
  private[actors] def retrieve(): Hold[List[Entry]] = synchronized :
    /* Although each separate call of retrieve is already synchronized on their owning objects,
     * we must also synchronized the sum calculation, otherwise this can lead to mixed results. */
    LogGlobal.retrieve() + LogLocal.retrieve()

