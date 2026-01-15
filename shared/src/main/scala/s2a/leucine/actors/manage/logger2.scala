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

import scala.compiletime.constValue

/** Basic interface for each ActorMonitor. */
trait ActorLogger extends LogHandler : //, LogInterface:
  import ActorLogger.{Level, Timing, Entry}

  /** Define if actors may be locally set the logLevel and logTiming. */
  def localSettings: Boolean

  /** This method is called in the main thread for every log entry. */
  def process(entry: Entry): Unit

  /** Implement a handler for the event a fatal situation occurs */
  def handleFatal(message: String): Unit

  /** Request the current active logging level */
  def level: Level

  /** Request the current active logging timing */
  def timing: Timing

  /* Contains the logHolder for the main and other threads without local logHolders. */
  private[actors] lazy val logHolder = new LogHolder("",level,timing)

  private[actors] def spool(): Unit =
    val allEntries = LogHolder.fullRetrieve()
    for
      entries <- allEntries
      entry   <- entries
    do process(entry)



// EXAMPLE USE:
object DefaultActorLogger extends ActorLogger :
  import ActorLogger.{Level, Timing, Entry}
  /* Obligatory definitions of the settings. Each logger must define at least a default for
   * the logging timing and level. */

  type FixLevel = Level.Debug

  /** Define the default active logging level (see ActorLogger.Level for documentation) */
  val timing: Timing    = Timing.Millis

  /** Define the default active logging level (see ActorLogger.Level for documentation) */
  val level:  Level     = Level.Debug


  /**
   * Set local to allow for changes in logging level and timing within the actors. Usually, this
   * is only so while developing. In deployment you usually want these settings to be the same
   * throughout the whole application. Setting this to false makes that so, without have to revisit
   * all logging code lines. */
  val localSettings: Boolean    = true

  def handleFatal(message: String): Unit = println(s"THERE WAS A FATAL EVENT: $message")

  def process(entry: Entry): Unit = println(entry.show)



/** Use this Object to directly start logging with default functionality. */
object ActorLogger  :

  /* Unfortunately it is not possible to properly inline Scala enums and eliminate code with that.
   * Therefore Level is not implemented as enum but as old fashioned sealed trait. That way we can
   * eliminate all debug and info log calls if the minimal logging level is set to Warn. See also
   * the forum discussion: https://users.scala-lang.org/t/how-to-inline-an-enum-properly/12181 */

  /**
   * Attribute to each log level an ordinal on the type level. This makes it possible to compare
   * level at compile time an eliminate unneeded code. */
  type Ordinal[L <: Level] <: Int = L match
    case Level.Ignore.type => 0
    case Level.Fatal.type  => 1
    case Level.Error.type  => 2
    case Level.Warn.type   => 3
    case Level.Info.type   => 4
    case Level.Debug.type  => 5

  /**
   * The different levels that are available for logging. Note that the level Ignore is only there
   * as bottom level. Setting logging to this level effectively disables all logging, and should only
   * be used as temporary measure. Note the an actor may locally override this setting. This is what you
   * want, so you can zoom in on particular behavior. A call with log level Fatal is not handled via
   * the regular channels, but you should supply a special handler and orderly shutdown hook for this.
   * The other levels are the common ones: Error, Warn, Info and Debug. */
  sealed trait Level extends EnumOrder[Level] :
    inline def ordinal: Int

  object Level :
    /** Type aliases to define the compile time fix level syntactically equal to the runtime log level. */
    type Ignore = Ignore.type
    type Fatal  = Fatal.type
    type Error  = Error.type
    type Warn   = Warn.type
    type Info   = Info.type
    type Debug  = Debug.type

    /**
     * Meaning: level to disable all logging (including fatal!).
     * Usage:   to temporarily silence (most) of the logging.
     * Action:  none, but do not use this level for production.
     * Example: to focus on the logging of one or some actors in a test. */
    case object Ignore extends Level :
      inline def ordinal: Int = constValue[Ordinal[Level.Ignore]]

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
     * Example: new user, written data, new network connection, etc. */
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
   * The different timings that are available for logging. Nanos offers resolution at the nanosecond level,
   * although the granularity may be (significantly) higher. The cost is a System.nanoTime() call at each log
   * entry. The values are guaranteed to be strictly monotonic and are added to the start time of  the system.
   * Millis offer a millisecond resolution (and usually same granularity) by calling System.currentTimeMillis().
   * The latter can be 6 times more efficient than the former, but are not guaranteed to be monotonic. The
   * times can be used to order the logs, but the log entries also contain an index that can be used to that end.
   * If second level accuracy is sufficient, you may use Recent as setting, which simply uses the last available
   * call to System.currentTimeMillis() rounded to one second, which is the fastest option. The granularity
   * depends on the frequency of spooling the log entries to an external logging facility.
   * All times are displayed as UTC times, daylight saving and local time zones are ignored. */
  enum Timing :
    /** Log entries will be not individually timed and are accurate up to the second at most. */
    case Recent
    /** Log entries will contain absolute times (from system clock) with millisecond accuracy.  */
    case Millis
    /** Log entries will contain relative times (from system start) with nanosecond accuracy. */
    case Nanos

  /**
   * The log entry itself.
   * index:     strictly monotonous and dense index for all log entries starting at 1.
   * level:     the log level of this entry, one of (Error,Warn,Info,Debug)
   * timestamp: number of nanoseconds starting from the Unix Epoch
   * message:   message from the developer.
   */
  // We willen ook de tread en de actor opnemen in de entry. Zie de oude logger.
  class Entry(val index: Long, val level: Level, val timing: Timing, val timestamp: Long, val thread: String, val actor: String, val clazz: String, val message: String) :
    import java.util.Date
    @annotation.nowarn // We know that the date methods are deprecated Ignore that for now.
    def show: String =
      // Tijdelijke datum/tijdFormatter
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
      s"LOG($indexStr; $levelStr; $dtStr; thread($thread); actor($actor); class($clazz); $message)"

  val noEntry = Entry(0,Level.Ignore,Timing.Recent,0,"","","","")





