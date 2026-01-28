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
import scala.compiletime.constValue
import scala.annotation.nowarn

/**
 * Basic interface for each custom ActorLogger. You must at least implement the last
 * method of this interface and the FixLevel to get your logger running. See the
 * DefaultActorLogger object for an example. */
trait ActorLogger(using context: ActorContext) extends LogHandler :
  import ActorLogger.{Level, Timing, Entry}
  import LogHolder.{Hold, Store}

  /* Type to store Entries in Arrays. Arrays are initialized with null's. It is inefficient
   * to reinitialize them with a special Nil Entry since these stay all within Leucine.
   * So we allow for Null here and make sure these never reach the user of Leucine. */
  private type NEntry = Entry | Null

  /**
   * FixLevel defines the logging level used at compile time. Regular log statements with a higher
   * level will to removed from the code. Use this for example to eliminate info and debug log
   * messages by setting it to Level.Warn for a production release. */
  type FixLevel <: Level

  /**
   * Direct can be set to true if you want to directly receive the log entries without making
   * use of the per thread collectors. Sometimes this can be handy to zoom in on a critical bug
   * or if you have your own threaded log handler. However, the 'direct' call on the logger is
   * always direct, and the 'delayed' call on the logger is always delayed, both independent
   * of this setting. */
  type Direct <: Boolean

  /**
   * Develop can be set to true if you want to incorporate the logger statements 'direct' and
   * delayed in your code. When set to false, these are eliminated at compile time, independent
   * of the level used. Note that these statements are only compile time eliminated based on
   * the level on best effort basis. */
  type Develop <: Boolean

  /**
   * Select if you want full class paths in your logs or just class names.
   */
  type FullPath <: Boolean

  /* Keep an lower bound of the index since the last retrieve. Note that we must synchronize
   * since longs are not atomic with respect to read/write in Java (others are, except doubles) */
  private var lastIndex: Long = 0

  /* Contains the logHolder for the main and other threads without local logHolders. */
  private[actors] lazy val logHolder = new LogHolder("",level,timing)

  /** Test if we have enough new logs for spooling, entry should be the last or recently produced. */
  private[actors] def trySpool(entry: Entry): Unit =
    val size = synchronized { entry.index - lastIndex }
    if size > maxLogs then
      println("*** from trySpool => spool")
      context.deferred(spool(false))

  /**
   * Manually retrieve and clear the log entries collected since the last retrieve.
   * Make a new recent time stamp and record the last index. */
  final def retrieve(): Hold[List[Entry]] =
    /* Update the recent time stamp. Note this is done before retrieving, which may lead
     * to some generated log entries to have the new timestamp. Since their log
     * timestamps are in fact closer to this moment, that is no problem. */
    ActorLogger.updateRecent()
    /* Get an the current value of the log index. Note, immediately after the call the value
     * is already outdated, so if we base an estimation about the number of none processed
     * log entries on this number it will be to high. However, that is better than the opposite
     * since this number is used to call for intermediate spooling if the periodic calls from
     * the guard are insufficient.  */
    val currIndex = ActorLogger.getIndex
    synchronized { lastIndex = currIndex }
    /* Get the data, clear the collection, return the result. This call is synchronized
     * at the definition side. */
    LogHolder.retrieve()


  /**
   * Simple sorting routine to sort all entries in the holder upon index. Returns an
   * (immutable) IArray. Note that some index values may be missing, which is due to
   * the asynchronicity of the logging mechanism. Usually they are provided in the
   * next call. For efficiency reasons, these values are null instead of some custom
   * Nil variant of Entry. You may override this with your own implementation. */
  def sort(hold: Hold[List[Entry]]): IArray[NEntry] =
    println(s"*** hold = ${hold}")
    val slots: Array[NEntry] = new Array[NEntry](hold.width)
    val outer = hold.entries.iterator
    while outer.hasNext do
      val inner = outer.next().iterator
      while inner.hasNext do
        val entry = inner.next()
        val index = (entry.index - hold.min).toInt
        slots(index) = entry
    IArray.unsafeFromArray(slots)

  /**
   * This must be implemented with a method that spools the log entries to the process method.
   * The log entries can be obtained with a call to retrieve(). See the different examples for
   * possible implementations. Spool is called by Leucine on a regular basis to offload the log
   * entries from the actor framework to you logging framework. The parameter completed will be
   * true upon the very last call. */
  def spool(completed: Boolean): Unit


  /** Just spool all log entries unsorted. Use if your own logging framework takes care of this. */
  def simpleSpool(hold: Hold[List[Entry]]): Unit = hold.entries.foreach(_.foreach(process))

  /**
   * Collects all log entries from all different logHolders, orders them and simply spools them
   * one by one to the custom implemented process method. Note that some entries might be missing,
   * due to the asynchronicity of the logging mechanism. This method is periodically called by
   * the Actor Guard to transfer the log items to the user defined logging system. You may override
   * this method if you want to handle the entries in bulk yourself. Completed will be true on
   * the very last call to spool. The application will terminate soon after. You may want to use
   * this information when building your own sorted logger. */
  def sortedSpool(hold: Hold[List[Entry]]): Unit = sort(hold).withFilter(_ != null).foreach(process)


  /**
   * Spool method where we stitch new log entries before processing. Stitching means that if the
   * current spool session has missing entries, that part will be temporarily stored for reprocessing
   * in the next run of spool. That way we obtain strict ordering with respect of the log index, at
   * the cost of a slight delay of processing. When complete is true all remaining logs will be spooled. */
  def stichedSpool(hold: Hold[List[Entry]], store: Store, completed: Boolean): Store = synchronized :

    def arrayToString(array: IArray[Entry | Null]): String =
     def convert(x: Entry | Null): Char = if x==null then '-' else 'o'
     array.toSeq.map(convert).toString

    /* Sort the new entries into an array. */
    val currEntries = sort(hold)
    val lastEntries = store.entries

    println(s"*** lastEntries= |${arrayToString(lastEntries)}|")
    println(s"*** currEntries= |${arrayToString(currEntries)}|")

    /* Since we have no real idea how the arrays appear after each other, it is best to work with two phases,
     * where we walk through the arrays simultaneously, starting at the most early point from both of them.
     * We first have a send phase that sends the entries to the process method for as longs as the entries
     * are continuously available in either array. The first moment an entry is missing in both, we start the
     * merge phase. This array will therefor always start with an null entry or be empty. The index of that missing
     * entry will be our new start. We must further build in a safety measure that the stored array does not become
     * too long. At some time its better to disrupt the order than to blow op the memory. Also, when all actors
     * are finished, we spool anything that is left, since there will be no one around to produce new logs. */

    /* We will limit the resulting array to this size. If it gets bigger, log entries are spooled anyway. */
    val maxArraySize = 10 * maxLogs

    /* Visualization of the processing of the two entry arrays.
     *  |o-...| = array of entries.
     *   o      = entry present
     *    -     = entry absent
     *
     * At startup (run 1):
     *  ||                                                  <= lastEntries
     *  |ooooooooooooooooooooooooooo-----o-ooo-oo-o-ooo-o|  <= currentEntries
     *   <--------send-------------><------merge-------->   phases
     *                             |-----o-ooo-oo-o-ooo-o|  <= resulting array
     *
     * Next spool run 2:
     * |-----o-ooo-oo-o-ooo-o|                              <= lastEntries
     * |ooooo-o---o--o-o------oooooooooooooooooooooooo-o|   <= currentEntries
     *  <------send-------><----------merge------------>    phases
     *                    |-o-oooooooooooooooooooooooo-o|   <= resulting array
     *
     * Next spool run 3:
     * |-o-oooooooooooooooooooooooo-o|                      <= lastEntries
     * |o-o------------------------o-oooooooooooooo-o|      <= currentEntries
     *  <-----------send-------------------------->mm       phases
     *                                            |-o|      <= resulting array
     * or it could be
     *
     * Next spool run 3:
     * |-o-oooooooooooooooooooooooo-o|                      <= lastEntries
     *   |o------------------------o-oooooooooooooo-o|      <= currentEntries
     *  <-----------merge--------------------------->       phases
     * |-oooooooooooooooooooooooooooooooooooooooooo-o|      <= resulting array
     *
     * Etc ....
     */


    /* Verify if the construction assumptions about the array hold. We need those values to start processing. */
    assert(lastEntries.isEmpty || lastEntries(0) == null, s"Array lastEntries must start with a null or be empty.")
    assert(currEntries.isEmpty || currEntries(0) != null, s"Array currEntries may not start with a null.")

    /* Get for both arrays the real start index. */
    val lastEntryIndexStart = if lastEntries.isEmpty then 0 else store.start
    val currEntryIndexStart = if currEntries.isEmpty then 0 else currEntries(0).index

    /* See where we must start the processing, which is the minimum of both log-index-numbers. */
    val startIndex =
      if      lastEntries.isEmpty then currEntryIndexStart
      else if currEntries.isEmpty then lastEntryIndexStart
      else                             math.min(lastEntryIndexStart,currEntryIndexStart)

    /* We stop at the highest possible log-index-number. Note this one beyond the last usable value. */
    val endIndex   =
      if      lastEntries.isEmpty then currEntryIndexStart+currEntries.length
      else if currEntries.isEmpty then lastEntryIndexStart+lastEntries.length
      else                             math.max(lastEntryIndexStart+lastEntries.length,currEntryIndexStart+currEntries.length)

    /* Test if a log-index number has a valid array-index for each array */
    def hasLastIndex(index: Long) = (index >= lastEntryIndexStart) && (index - lastEntryIndexStart) < lastEntries.length
    def hasCurrIndex(index: Long) = (index >= currEntryIndexStart) && (index - currEntryIndexStart) < currEntries.length

    /* Get the entry based on log-index number for each array. Test first if the index may be used! */
    def getLastEntry(index: Long) = lastEntries((index - lastEntryIndexStart).toInt)
    def getCurrEntry(index: Long) = currEntries((index - currEntryIndexStart).toInt)

    /* See if an log-index has a value on the particular array. This returns false if the index
     * is out-of-bounds or if there is no entry on that particular location present. */
    def hasLastEntry(index: Long) = hasLastIndex(index) &&  getLastEntry(index) != null
    def hasCurrEntry(index: Long) = hasCurrIndex(index) &&  getCurrEntry(index) != null

    /* The send phase runs as long as we can process entries with continuously increasing indices without gaps.
     * We ignore gaps when we are terminating (completed is true, but then there should not be any gaps) or when
     * there the gaps are so early that we must store more then the maximum amount of entries. This breaks the
     * strict ordering, but is further not harmful. We also stop when we reached the end. */
    def sendPass(index: Long): Long = if index >= endIndex then endIndex else
      println(s"*** sendPass($index)")
      /* See which array has an entry */
      val hasLast = hasLastEntry(index)
      val hasCurr = hasCurrEntry(index)
      /* It cannot be so that both arrays have an entry on the same log-index. */
      assert(!hasLast || !hasCurr, s"Two log entries with the same index $index.")
      /* Process the available entry (or none if there isn't any. */
      if hasLast then process(getLastEntry(index))
      if hasCurr then process(getCurrEntry(index))
      /* Break if we have a gap but are not completed and the merged array will not become to large. */
      if !hasLast && !hasCurr && !completed && (endIndex - index < maxArraySize) then index else sendPass(index + 1)

    /* The remainder of the values must be merged into one array (or just copied if the other is shorter) */
    def mergePass(index: Long, offset: Long, result: Array[NEntry]): Array[NEntry] = if index >= endIndex then result else
      println(s"*** mergePass($index,$offset)")
      /* See which array has an entry */
      val hasLast = hasLastEntry(index)
      val hasCurr = hasCurrEntry(index)
      /* It cannot be so that both arrays have an entry on the same log-index. */
      assert(!hasLast || !hasCurr, s"Two log entries with the same index $index.")
      /* Store what is there. */
      if hasLast then result((index - offset).toInt) = getLastEntry(index)
      if hasCurr then result((index - offset).toInt) = getCurrEntry(index)
      /* Move to the next element. No break test here, that is done at the top, we must always parse to the end. */
      mergePass(index + 1, offset, result)

    /* Start the send phase. Returns the first index that does not fulfill the send phase criteria. */
    val sendIndex = sendPass(startIndex)
    /* Start the merge phase. Returns the first index that does not fulfill the merge phase criteria. */
    val result    = mergePass(sendIndex,sendIndex,new Array[NEntry]((endIndex-sendIndex).toInt))
    /* Return the merged array for the next spool run. */
    println(s"*** result(sendIndex=$sendIndex),array=${arrayToString(IArray.unsafeFromArray(result))}")
    Store(sendIndex,IArray.unsafeFromArray(result))

  /**
   * Set localSettings to true to allow for changes in logging level and timing within the actors.
   * Usually, this is only relevant while developing. In deployment you want these settings to be
   * equal throughout the whole application. Setting this to false makes that so, without have
   * to revisit all logging code lines. */
  def localSettings: Boolean

  /**
   * Define the max number of logs allowed before spooling must start. Do not make this value
   * to low, since it every time logs are spooled, they interfere if even ever so slightly,
   * with the actor processing. Realistic values depend on the number of log statements in
   * you code, but 20 to 100 should be considered as realistic lower bounds. Note that logs
   * are also periodically spooled. So this number is effective only if the number of logs
   * builds up quickly in between. */
  def maxLogs: Int

  /** Define the default active logging level (see ActorLogger.Level for documentation) */
  def level: Level

  /** Define the default active logging level (see ActorLogger.Level for documentation) */
  def timing: Timing

  /**
   * This method is called for every log entry when the entries are spooled.
   * Note that the implementation must be re-entrant and thread save. */
  def process(entry: Entry): Unit

  /**
   * Implement a handler for the event a fatal situation occurs.
   * Note that the implementation must be re-entrant and thread save. */
  def handleFatal(message: String): Unit

  /* As soon as this trait is instantiated, make sure the ActorGuard is aware. Note
   * that you should instantiate this trait only once, the second time will be ignored. */
  ActorGuard.logger = this


/**
 * Default logger you may use to simply send your logs to the console via the main thread.
 * It contains reasonable defaults for all obligatory definitions of the settings. */
trait DefaultLoggerSettings :
  import ActorLogger.{Level, Timing, Entry}
  import LogHolder.{Hold, Store}

  /** Set FixLevel to Level.Debug to ensure all logs pass during development. */
  type FixLevel = Level.Debug

  /** Set Direct to false to ensure all logs pass the thread local entry collectors */
  type Direct = false

  /** Set Develop to false to ensure development log calls are incorporated */
  type Develop = false

  /** Select if you want full class paths in your logs or just class names. */
  type FullPath = false


  /* Log entries that could not yet be processed. */
  private var store: Store = Store.empty

  /** Set the number of maxLogs to 25 */
  val maxLogs = 25

  /** Set timing to Millis to have a reasonable estimate about the moment the log was processed. */
  val timing: Timing = Timing.Millis

  /** Set logging level to debug to see all logs during development. */
  val level:  Level = Level.Debug

  /** Set local to true allow for changes in logging level and timing within the actors. */
  val localSettings: Boolean = true

  def retrieve(): Hold[List[Entry]]
  def simpleSpool(hold: Hold[List[Entry]]): Unit
  def sortedSpool(hold: Hold[List[Entry]]): Unit
  def stichedSpool(hold: Hold[List[Entry]], store: Store, completed: Boolean): Store
  def spool(completed: Boolean): Unit = store = stichedSpool(retrieve(),store,completed)
  //def spool(completed: Boolean): Unit = sortedSpool(retrieve())

  /** Simply say in the console that a fatal event occurred. */
  def handleFatal(message: String): Unit = println(s"THERE WAS A FATAL EVENT: $message")

  /** Pass all logs to the console. */
  def process(entry: Entry): Unit = println(entry)



/**
 * Object that holds the different levels of logging, settings for the timing, and the Entry
 * class that contains your log statement. */
object ActorLogger  :

  /* Fixed conversion factor to go from milliseconds to nanoseconds. */
  private inline val millisToNanos = 1000000

  /* If we make use of logging we want to know the moment the application started. This
   * does not need to be at the nanosecond exact, but it must be some stable point around the start.
   * The start is obtained in both nanosecond and millisecond accuracy, so we can combine both timers
   * into one.  */
  private val startNanos: Long  = System.nanoTime
  private val startMillis: Long = System.currentTimeMillis() * millisToNanos

  /* Logs are indexed with strict order. This counter is used to t*/
  private val logCounter: AtomicLong = AtomicLong(1)

  /** Keeps a timestamp of the last allTerminated poll in milliseconds */
  private val lastRecent: AtomicLong = AtomicLong(startMillis)

  /** Thread update the lastRecent timer with the current value */
  private[actors] def updateRecent(): Unit = lastRecent.set(System.currentTimeMillis() * millisToNanos)

  /** Thread save manner to obtain a the index for a log entries */
  private[actors] def getIndex: Long = logCounter.get

  /** Thread save manner to obtain a new index for a log entry */
  private def getAndIncIndex(): Long = logCounter.getAndIncrement()

  /**
   * Get a timestamp for the current time in nano seconds from the application start.
   * Note, since the application start cannot be determined with nano second accuracy, it
   * is build upon a time mark at global logger construction obtained with millisecond
   * accuracy. From there a reconstruction is made with the passed time in nano seconds. */
  private[actors] def getTimeStamp(timing: Timing): Long = timing match
    case Timing.Recent  => lastRecent.get
    case Timing.Millis  => System.currentTimeMillis() * millisToNanos
    case Timing.Nanos   => startMillis + (System.nanoTime - startNanos)

  /* Unfortunately it is not possible to properly inline Scala enums and eliminate code with that.
   * Therefore Level is not implemented as enum but as old fashioned sealed trait. That way we can
   * eliminate all debug and info log calls if the minimal logging level is set to Warn. See also
   * the forum discussion: https://users.scala-lang.org/t/how-to-inline-an-enum-properly/12181 */

  /**
   * Attribute to each log level an ordinal on the type level. This makes it possible to compare
   * level at compile time an eliminate unneeded code. */
  type Ordinal[L <: Level] <: Int = L match
    case Level.System => 0
    case Level.Fatal  => 1
    case Level.Error  => 2
    case Level.Warn   => 3
    case Level.Info   => 4
    case Level.Debug  => 5
    case Level.Trace  => 6

  /**
   * The different levels that are available for logging. Note that the level System is only there
   * as bottom level. Setting logging to this level effectively disables all logging, and should only
   * be used as temporary measure. Note the an actor may locally override this setting. This is what you
   * want, so you can zoom in on particular behavior. For the log level Fatal you can supply a special
   * handler and orderly shutdown hook if needed, which is handled before the log entry is processed.
   * The other levels are the common ones: Error, Warn, Info and Debug. */
  sealed trait Level extends EnumOrder[Level] :
    inline def ordinal: Int

  object Level :
    /** Type aliases to define the compile time fixed level syntactically equal to the runtime log level. */
    type System = System.type
    type Fatal  = Fatal.type
    type Error  = Error.type
    type Warn   = Warn.type
    type Info   = Info.type
    type Debug  = Debug.type
    type Trace  = Trace.type

    /**
     * Meaning: level to disable all logging (including fatal!).
     * Usage:   to temporarily silence (most) of the logging.
     * Action:  none, but do not use this level for production.
     * Example: to focus on the logging of one or some actors in a test. */
    case object System extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.System]]

    /**
     * Meaning: indicates that further processing is unreliable and shutdown is imminent.
     * Usage:   report messages directly (circumvent regular logging) and initiate last goodbyes.
     * Action:  immediate, will require root cause investigation and system restart.
     * Example: in case of "out of memory" or "null pointer exception". */
    case object Fatal extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Fatal]]

    /**
     * Meaning: severe disturbances in process handling, but system can continue with other tasks.
     * Usage:   to request attention from the operator and/or developer.
     * Action:  immediate, repair damage to data and investigation of the cause.
     * Example: in case of "disk full", unexpected absence of user profiles etc. */
    case object Error extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Error]]

    /**
     * Meaning: indication that something is out of the ordinary, but processing can continue.
     * Usage:   to request attention from the operator and/or developer
     * Action:  quickly investigate the cause
     * Example: any situation that you do not expect such as missing a data field etc. */
    case object Warn extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Warn]]

    /**
     * Meaning: to keep the developer/user informed about the systems whereabouts
     * Usage:   to enable a high level reconstruction of the systems actions
     * Action:  none
     * Example: see new users, written data, new network connection, etc. */
    case object Info extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Info]]

    /**
     * Meaning: to communicate internals of the system for diagnostic purposes.
     * Usage:   to enable a low level reconstruction of the systems actions
     * Action:  debug, refactor, code, drink coffee.
     * Example: any detail you need to know to understand possible problems */
    case object Debug extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Debug]]

    /**
     * Meaning: to follow the flow of the code for diagnostic purposes.
     * Usage:   supply each class and method definition with a trace
     * Action:  debug, refactor, code, drink coffee.
     * Example: any detail you need to know to understand possible problems */
    case object Trace extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Trace]]

  /**
   * The different timings that are available for logging. Nanos offers resolution at the nanosecond level,
   * although the granularity may be (significantly) higher. The cost is a System.nanoTime() call at each log
   * entry. The values are guaranteed to be strictly monotonic and are added to the start time of  the system.
   * Millis offer a millisecond resolution (and usually same granularity) by calling System.currentTimeMillis().
   * The latter can be 6 times more efficient than the former, but are not guaranteed to be monotonic. The
   * times can be used to order the logs, but the log entries also contain an index that can be used to that end.
   * If second level accuracy is sufficient, you may use Recent as setting, which simply uses the last available
   * call to System.currentTimeMillis() rounded to one second, which is the fastest option. The granularity
   * depends on the frequency of spooling the log entries to an external logging facility.
   * All times are passed as UTC times, daylight saving and local time zones are ignored. */
  enum Timing :
    /** Log entries will be not individually timed and are accurate up to the second at most. */
    case Recent
    /** Log entries will contain absolute times (from system clock) with millisecond accuracy.  */
    case Millis
    /** Log entries will contain relative times (from system start) with nanosecond accuracy. */
    case Nanos

  /**
   * The log entry itself.
   * index:      strictly monotonous and dense index for all log entries starting at 1.
   * level:      the log level of this entry, one of (Fatal,Error,Warn,Info,Debug)
   * timing:     the log timing granularity (second/millisecond/nanosecond)
   * timestamp:  number of nanoseconds starting from the Unix Epoch (granularity maybe less)
   * threadName: name of the thread the log was made in.
   * actorName:  name of the actor the log was made in (empty if outside the actor framework)
   * className:  full name of the enclosing class the log was made in
   * message:    the log message from the developer. */
  class Entry(val index: Long, val level: Level, val timing: Timing, val timestamp: Long, val threadName: String, val actorName: String, val className: String, val message: String) :

    /* We know that the date methods are deprecated. Ignore that for now. */
    /** Simple formatter to show the contents of a log entry. */
    @nowarn
    override def toString: String =
      import java.util.Date
      val nanos  = timestamp - ((timestamp / 1000000000) * 1000000000)
      val millis = nanos / 1000000
      val date   = Date(timestamp / 1000000)
      val indexStr  = s"%${2}s".format(index)
      val levelStr  = s"%${5}s".format(level)
      val yearStr   = f"${date.getYear() + 1900}%04d"
      val monthStr  = f"${date.getMonth() + 1}%02d"
      val datumStr  = f"${date.getDate()}%02d"
      val hoursStr  = f"${date.getHours()}%02d"
      val minsStr   = f"${date.getMinutes()}%02d"
      val secsStr   = f"${date.getSeconds()}%02d"
      val subsecStr = timing match
        case Timing.Recent => ""
        case Timing.Millis => f".$millis%03d"
        case Timing.Nanos  => f".$nanos%09d"
      val dtStr  = s"$yearStr-$monthStr-$datumStr $hoursStr:$minsStr:$secsStr$subsecStr"
      s"LOG($indexStr; $levelStr; $dtStr; thread($threadName); actor($actorName); class($className); $message)"

  object Entry :
    /**
     * Construct an log entry based on the given data and add a timestamp, an index,  a thread
     * name, timing and actor path name (if available) */
    def apply(path: String, level: Level, timing: Timing, className: String, message: String): Entry =
      val index      = getAndIncIndex()
      val timeStamp  = getTimeStamp(timing)
      val threadName = Thread.currentThread().getName()
      new Entry(index,level,timing,timeStamp,threadName,path,className,message)

    inline def system(message: String): Entry =
      val className  = StaticInfo.pathInfo(true)
      val timeStamp  = getTimeStamp(Timing.Millis)
      val threadName = Thread.currentThread().getName()
      new Entry(0,Level.System,Timing.Millis,timeStamp,threadName,"",className,message)
