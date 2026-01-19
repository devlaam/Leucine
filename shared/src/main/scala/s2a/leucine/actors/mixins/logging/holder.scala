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

import java.util.concurrent.atomic.AtomicLong

/**
 * Class that collects and temporary holds the log entries. Entries are collected in a list,
 * which is fast so it ensures quick return to the users code. However, it may not be suited
 * for large amounts of logs due to the scattered nature of memory use and missed cache hits
 * at garbage collection. The class also keeps a local copy of the active level and timing
 * settings. Any entry can be (runtime) checked prior to entry construction and storage.
 * For internal use only. The class is not thread safe by design, so synchronize your calls
 * or ensure that you are using this holder in one thread by other means. The latter is comes
 * natural when used as a thread local variable. */
private class LogHolder(val path: String, val level: ActorLogger.Level, val timing: ActorLogger.Timing) :
  import ActorLogger.{Level, Entry}
  import LogHolder.{getIndex, getTimeStamp}

  /** Managed container for all log entries. */
  private var entries: List[Entry] = Nil

  /** Check if the holder contains any entries */
  private[actors] def isEmpty: Boolean = entries.isEmpty

  /**
   * Construct an log entry based on the given data and add a timestamp, an index,  a thread
   * name, timing and actor path name (if available) */
  private[actors] def make(level: Level, className: String, message: String): Entry =
    val timeStamp = getTimeStamp(timing)
    val index     = getIndex()
    val thread    = Thread.currentThread().getName()
    Entry(index,level,timing,timeStamp,thread,path,className,message)

  /** Test if an log entry with the given level would pass for the current settings. */
  private[actors] def pass(level: Level): Boolean = level <= this.level

  /** Add a log entry to the list. */
  private[actors] def add(entry: Entry): Unit = entries ::= entry

  /** Get a copy of the current log list. */
  private[actors] def get: List[Entry] = entries

  /** Clear the current log list. */
  private[actors] def clear(): Unit = entries = Nil



/**
 * Object that contains the public methods to add a log to the relevant log list.
 * For internal use only */
private object LogHolder :
  import ActorLogger.{Level, Timing, Entry}

  /* Fixed conversion factor to go from milliseconds to nanoseconds. */
  inline val millisToNanos = 1000000

  /* If we make use of logging we want to know the moment the application started. This
   * does not need to be at the nanosecond exact, but it must be some stable point around the start.
   * The start is obtained in both nanosecond and millisecond accuracy, so we can combine both timers
   * into one.  */
  private val startNanos: Long  = System.nanoTime
  private val startMillis: Long = System.currentTimeMillis() * millisToNanos

  /* Logs are indexed with strict order. This counter is used to t*/
  private val logCounter: AtomicLong = AtomicLong(1)

  /** Thread save manner to obtain a new index for a log entry */
  private[actors] def getIndex(): Long = logCounter.getAndIncrement()

  /**
   * Get a timestamp for the current time in nano seconds from the application start.
   * Note, since the application start cannot be determined with nano second accuracy, it
   * is build upon a time mark at global logger construction obtained with millisecond
   * accuracy. From there a reconstruction is made with the passed time in nano seconds. */
  private[actors] def getTimeStamp(timing: Timing): Long = timing match
    case Timing.Recent  => ActorGuard.getLastSpool * millisToNanos
    case Timing.Millis  => System.currentTimeMillis() * millisToNanos
    case Timing.Nanos   => startMillis + (System.nanoTime - startNanos)

  /**
   * Add a new log entry to the log of the current thread, if that thread has an active local container. If not,
   * the entry is directed to the global container. Returns the constructed entry for further processing.  */
  private[actors] def feed(level: Level, className: String, message: => String): Unit =
    if !LogLocal.tryFeed(level,className,message) then LogGlobal.feed(level,className,message)

  /** Collect all the logs that are present and empty the containers */
  private[actors] def retrieve(): List[List[Entry]] = LogGlobal.retrieve() :: LogLocal.retrieve()

