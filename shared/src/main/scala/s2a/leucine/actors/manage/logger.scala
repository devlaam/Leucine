// package s2a.leucine.actors

// /**
//  * MIT License
//  *
//  * Copyright (c) 2023 Ruud Vlaming
//  *
//  * Permission is hereby granted, free of charge, to any person obtaining a copy
//  * of this software and associated documentation files (the "Software"), to deal
//  * in the Software without restriction, including without limitation the rights
//  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  * copies of the Software, and to permit persons to whom the Software is
//  * furnished to do so, subject to the following conditions:
//  *
//  * The above copyright notice and this permission notice shall be included in all
//  * copies or substantial portions of the Software.
//  *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  * SOFTWARE.
//  **/



// /** Basic interface for each ActorMonitor. */
// trait ActorLogger extends LogStore, LogHandle, Log:
//   import ActorLogger.Entry

//   private[actors] def thread: String = Thread.currentThread().getName()
//   private[actors] def actor: String  = ""
//   private[actors] def clazz: () => String  = () => getClass.getSimpleName()

//   /** Define if actors may be locally set the logLevel and logTiming. */
//   def local: Boolean

//   /** This method is called in the main thread for every log entry. */
//   def process(entry: Entry): Unit

//   /** Implement a handler for the event a fatal situation occurs */
//   def handleFatal(entry: Entry): Unit

//   private[actors] def spool(): Unit =
//     val allEntries = getAndClear :: LogAid.logRetrieve()
//     for
//       entries <- allEntries
//       entry   <- entries
//     do process(entry)


//   // We moeten hier nog iets verzinnen dat we ook kunnen loggen als we niet
//   // in een actor zijn. Bijvoorbeeld het Log object uit LogAid overnemen.



// // EXAMPLE USE:
// object DefaultActorLogger extends ActorLogger :
//   import ActorLogger.{Level, Timing, Entry}
//   /* Obligatory definitions of the settings. Each logger must define at least a default for
//    * the logging timing and level. */

//   /** Define the default active logging level (see ActorLogger.Level for documentation) */
//   val timing: Timing    = Timing.Millis

//   /** Define the default active logging level (see ActorLogger.Level for documentation) */
//   val level:  Level     = Level.Debug

//   /**
//    * Set local to allow for changes in logging level and timing within the actors. Usually, this
//    * is only so while developing. In deployment you usually want these settings to be the same
//    * throughout the whole application. Setting this to false makes that so, without have to revisit
//    * all logging code lines. */
//   val local: Boolean    = true


//   def process(entry: Entry): Unit = println(entry.show)
//   def handleFatal(entry: Entry): Unit = println(entry.show)



// /** Use this Object to directly start logging with default functionality. */
// object ActorLogger  :

//   /**
//    * The different levels that are available for logging. Note that the level Ignore is only there
//    * as bottom level. Setting logging to this level effectively disables all logging, and should only
//    * be used as temporary measure. Note the an actor may locally override this setting. This is wat you
//    * want, so you can zoom in on particular behavior. A call with log level Fatal is not handled via
//    * the regular channels, but you should supply a special handler and orderly shutdown hook for this.
//    * The other levels are the common ones: Error, Warn, Info and Debug. */
//   enum Level extends EnumOrder[Level] :
//     /**
//      * Meaning: level to disable all logging (including fatal!).
//      * Usage:   to temporarily silence (most) of the logging.
//      * Action:  none, but do not use this level for production.
//      * Example: to focus on the logging of one or some actors in a test. */
//     case Ignore
//     /**
//      * Meaning: indicates that further processing is unreliable and shutdown is imminent.
//      * Usage:   report messages directly (circumvent regular logging) and initiate last goodbyes.
//      * Action:  immediate, will require root cause investigation and system restart.
//      * Example: in case of "out of memory" or "null pointer exception". */
//     case Fatal
//     /**
//      * Meaning: severe disturbances in process handling, but system can continue with other tasks.
//      * Usage:   to request attention from the operator and/or developer.
//      * Action:  immediate, repair damage to data and investigation of the cause.
//      * Example: in case of "disk full", unexpected absence of user profiles etc. */
//     case Error
//     /**
//      * Meaning: indication that something is out of the ordinary, but processing can continue.
//      * Usage:   to request attention from the operator and/or developer
//      * Action:  quickly investigate the cause
//      * Example: any situation that you do not expect such as missing a data field etc. */
//     case Warn
//     /**
//      * Meaning: to keep the developer/user informed about the systems whereabouts
//      * Usage:   to enable a high level reconstruction of the systems actions
//      * Action:  none
//      * Example: new user, written data, new network connection, etc. */
//     case Info
//     /**
//      * Meaning: to communicate internals of the system for diagnostic purposes.
//      * Usage:   to enable a low level reconstruction of the systems actions
//      * Action:  debug, refactor, code, drink coffee.
//      * Example: any detail you need to know to understand possible problems */
//     case Debug

//   /**
//    * The different timings that are available for logging. Nanos offers resolution at the nanosecond level,
//    * although the granularity may be (significantly) higher. The cost is a System.nanoTime() call at each log
//    * entry. The values are guaranteed to be strictly monotonic and are added to the start time of  the system.
//    * Millis offer a millisecond resolution (and usually same granularity) by calling System.currentTimeMillis().
//    * The latter can be 6 times more efficient than the former, but are not guaranteed to be monotonic. The
//    * times can be used to order the logs, but the log entries also contain an index that can be used to that end.
//    * If second level accuracy is sufficient, you may use Recent as setting, which simply uses the last available
//    * call to System.currentTimeMillis() rounded to one second, which is the fastest option. The granularity
//    * depends on the frequency of spooling the log entries to an external logging facility.
//    * All times are displayed as UTC times, daylight saving and local time zones are ignored. */
//   enum Timing :
//     /** Log entries will be not individually timed and are accurate up to the second at most. */
//     case Recent
//     /** Log entries will contain absolute times (from system clock) with millisecond accuracy.  */
//     case Millis
//     /** Log entries will contain relative times (from system start) with nanosecond accuracy. */
//     case Nanos

//   /**
//    * The log entry itself.
//    * index:     strictly monotonous and dense index for all log entries starting at 1.
//    * level:     the log level of this entry, one of (Error,Warn,Info,Debug)
//    * timestamp: number of nanoseconds starting from the Unix Epoch
//    * message:   message from the developer.
//    */
//   // We willen ook de tread en de actor opnemen in de entry. Zie de oude logger.
//   class Entry(val index: Long, val level: Level, val timing: Timing, val timestamp: Long, val thread: String, val actor: String, val clazz: String, val message: String) :
//     import java.util.Date
//     @annotation.nowarn // We know that the date methods are deprecated Ignore that for now.
//     def show: String =
//       // Tijdelijke datum/tijdFormatter
//       val nanos  = timestamp - ((timestamp / 1000000000) * 1000000000)
//       val millis = nanos / 1000000
//       val date   = Date(timestamp / 1000000)
//       val indexStr  = s"%${2}s".format(index)
//       val levelStr  = s"%${5}s".format(level)
//       val yearStr   = f"${date.getYear() + 1900}%04d"
//       val monthStr  = f"${date.getMonth() + 1}%02d"
//       val datumStr  = f"${date.getDate()}%02d"
//       val hoursStr  = f"${date.getHours()}%02d"
//       val minsStr   = f"${date.getMinutes()}%02d"
//       val secsStr   = f"${date.getSeconds()}%02d"
//       val subsecStr = timing match
//         case Timing.Recent => ""
//         case Timing.Millis => f".$millis%03d"
//         case Timing.Nanos  => f".$nanos%09d"
//       val dtStr  = s"$yearStr-$monthStr-$datumStr $hoursStr:$minsStr:$secsStr$subsecStr"
//       s"LOG($indexStr; $levelStr; $dtStr; thread($thread); actor($actor); class($clazz); $message)"





