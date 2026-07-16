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

import java.time.Instant
import java.util.concurrent.atomic.AtomicLong
import scala.compiletime.constValue
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.annotation.implicitNotFound
import scala.collection.mutable.ListBuffer

/**
 * Basic interface for each custom ActorLogger. You must at least implement the last
 * method of this interface and the FixLevel to get your logger running. See the
 * DefaultActorLogger object for an example. */
trait ActorLogger(using context: ActorContext) extends LogHandler, LogProcessConfig, Service :
  import ActorLogger.{Level, Entry, Capture}
  import LogHolder.Hold

  /* Keep an lower bound of the index since the last retrieve. Note that we must synchronize
   * since bare longs are not atomic with respect to read/write in Java (others are, except doubles)
   * The value is to so high that all first logs will be buffered.  */
  private val lastIndex: AtomicLong = AtomicLong(Int.MaxValue)

  /* A timer is used to schedule the regular log spool actions. This is defined lazy for the
   * spoolInterval will not yet be available at this part of the object construction. */
  private lazy val timer: Timer = Timer(spooling.time,() => action(false))

  /* Contains the logHolder for the main and other threads without local logHolders. (non top
   * level objects are lazy, so we have the correct values here, this works) */
  private object LogGlobal extends LogGlobal(LogHolder("",() => runLevel,incidentLevel, () => timing))

  /* Contains the logHolder for the actors with local logHolders. (non top level objects are lazy,
   * so we have the correct values here, this works) */
  private object LogLocal extends LogLocal()

  /** The local logger is required in the actor logging for manipulation. */
  private[actors] def local: LogLocal = LogLocal

  /**
   * Make a new log entry of the current thread, if that thread has an active local container. If not,
   * the entry is created by the global container. Returns the constructed entry for further processing.
   * If feed is true, the entry will be directly stored on the log queue. If not, the entry will only be
   * constructed on the holder, and you are responsible for further processing. */
  protected def entry(feed: Boolean, capture: Capture): Option[Entry] =
    /* Try to construct the entry on the thread local container */
    LogLocal.entry(feed,capture) match
      /* This was a success, return the entry */
      case Right(entry) => Some(entry)
      /* The container was there but the entry could not be made due to insufficient log level or blocking filter. */
      case Left(true)   => None
      /* The container was not there, we must try the global container. */
      case Left(false)  => LogGlobal.entry(feed,capture) match
        /* This was a success, return the entry */
        case Right(entry) => Some(entry)
        /* The entry could not be made due to insufficient log level, blocking filter or absent global logger. */
        case Left(_)      => None

  /** Enables/Disables buffering by setting the last lastIndex to a very high value or to zero. */
  private def buffer(active: Boolean): Unit = lastIndex.set(if active then Int.MaxValue else 0)

  /**
   * Test if we have enough new logs for spooling, entry should be the last or recently produced.
   * If we are directly spooling this method calls process directly and otherwise tests if we must
   * already spool. This can be the case if there are enough logs or if the log is important. If flush
   * is true, the spooler is ordered to process all lingering (if any) logs. */
  private[actors] def preprocess(entry: Entry): Unit = if spooling.direct then process(entry) else
    /* See how far we have come since the last spool. This is def, for it may not be needed to
     * determine its value if we have a flush. */
    def size = entry.index - lastIndex.get
    /* See if we have the situation that the level triggers the spooling. In that case we want the
     * the log queue to be flushed, otherwise possible gaps in the log statements would rather delay
     * the important log message than speed up. The consequence is that there index order may be broken. */
    val flush = entry.level <= spooling.level
    /* See if there are enough logs lingering or the log is important enough to perform a direct spool.
     * Note that this action must be quick, for we want return asap to the task that was calling this. */
    if flush || size > spooling.size then action(flush)

  /**
   * Manually retrieve and clear the log entries collected since the last retrieve. You must make sure
   * yourself that you are not calling this method concurrently. However, since this is solely called
   * within spool handling, and that method must also obey that restriction, we do not protect it here.
   * Note that if we are direct spooling there will be no entries available.  */
  final def retrieve(): Hold[List[Entry]] = if spooling.direct then Hold.empty else
    /* Store the current value of the log index. Note, immediately after the call the value
     * is already outdated, so if we base an estimation about the number of none processed
     * log entries on this number it will be to high. However, that is better than the opposite
     * since this number is used to call for intermediate spooling if the periodic calls from
     * the guard are insufficient.  */
    lastIndex.set(ActorLogger.Entry.getIndex)
    /* Get the data, clear the collection, return the result. */
    LogGlobal.retrieve() + LogLocal.retrieve()

  /**
   * Access point for the timer events and manual spool events. If flush is true, the spooler is ordered
   * to process all lingering (if any) logs. This may break the strict order if the spooler is capable
   * of keeping this.  */
  private[actors] protected def action(flush: Boolean): Unit =
    /* Update the recent time stamp. Note this is done before retrieving, which may lead to some
     * generated log entries to have the new timestamp. Since their log timestamps are in fact
     * closer to this moment, that is no problem. */
    ActorLogger.updateRecent()
    /* Since we want the timer thread to be free as soon as possible the actual task is pushed back
     * on the context. Even when called manually this is the right thing to do, since we do not know
     * the origin of this call. There is however no need if we are direct spooling since then there
     * will be no entries available. */
    if !spooling.direct then context.sequential(spool(flush))

  /**
   * Start the logger. You must call this at least once, the logger does not start spooling automatically.
   * If you log direct (no spooling) but make use of Timing.Recent you must still call this, for it ensures
   * periodic updates of the timer. It can be done for example at the end of the constructor in the derived
   * class or at the start of your application. Or you must register your logger to the ActorGuard, so it
   * can take care of its lifetime management. The latter is the preferred way, if you make use of
   * ActorGuard.watch. You still can stop and restart the logger by hand in between if needed. The parameter
   * hello should be true upon first start. If spooling is not direct, all logs are buffered until this call
   * is made, to ensure you had ample time to set up the logging pipeline. Note there is no limit on the buffer,
   * so make the start quickly after application start, or start out on a high pass level. */
  def start(hello: Boolean): Unit =
    /* Disable the buffering on logs. All logs in the buffer will be spooled quickly (by the next log entry),
     * if it is already full. */
    buffer(false)
    /* Report that logging has started. Note that this does not imply this is the first entry,
     * only that we started spooling of the logs on a regular basis. */
    ActorGuard.syslog(Level.Info, if hello then "Logger started." else "Logger restarted.")
    /* Start the logging spool timer. Most likely there are already some start up messages. In order to get
     * them into the open as quickly as possible we copy the value of hello. This is true at first start.
     * (guard makes it so) */
    timer.start(hello)

  /**
   * Stop the logger. You may stop and start the logger at will, which can be useful to catch a specific
   * part of the action and not get overwhelmed by the other log data. The parameter goodbye should be true
   * upon the final stop (for a final last spool). Also here, if the logger is registered at the ActorGuard,
   * it will take care of stopping it at application termination. Note that if you stop the logger by hand,
   * log entries will still be buffered. If this is not wanted, increase the log pass level to Ignore.*/
  def stop(goodbye: Boolean): Unit =
    /* Report that logging has stopped. Usually this does imply this is the last entry since all
     * actors are inactive now. */
    ActorGuard.syslog(Level.Info, if goodbye then "Logger stopped." else "Logger paused.")
    /* If we are not directly spooling we should spool any remaining entries. With direct spooling there
     * are no remaining entries to spool. */
    if !spooling.direct then context.sequential(spool(goodbye))
    /* Stop the spooling timer. Try to prohibit the last queue event if we are only pausing. */
    timer.stop(!goodbye)
    /* Enable buffering if we are not at the last take. */
    if !goodbye then buffer(true)


/**
 * Object that holds the different levels of logging, settings for the timing, and the Entry
 * class that contains your log statement. */
object ActorLogger  :
  import LogHolder.{Hold, Store}
  import Static.Kind

  /**
   * Base trait for defining your custom channels for debug/trace logging selection. Make then like this:
   *  case object MyFirstChan extends Channel
   *  case object MySecondChan extends Channel
   * etc. */
  trait Channel

  /** Object Channel contains all predefined channels. */
  object Channel :
    /** Special channel that always passes the check.  */
    case object Pass extends Channel

    /** Channel where all system logs of the levels Error,Warn and Info are published */
    case object SysPrd extends Channel

    /** Channel where all system logs of the levels Beta,Debug and Trace are published */
    case object SysDvl extends Channel

    /** Channel where all application logs of the levels Error,Warn and Info are published */
    case object AppPrd extends Channel

    /** Channel where all application logs of the levels Beta,Debug and Trace are published */
    case object AppDvl extends Channel


  /**
   * We want to define tuples of any arity in the same manner, but up to know there is no possibility
   * to do so. See the discussion on (with my contributions):
   * Scala Users Form: https://contributors.scala-lang.org/t/syntax-for-type-tuple-with-one-element/6974
   * Thus for now we define the generalized tuple this way so you can write (a,b) for tuples
   * with two elements, (a) for a tuple with one element, where the brackets are in fact ignored,
   * and () for a tuple without elements, which is in fact of type Unit. */
  type GTuple[G] = Tuple | G | Unit

  /** This defines a generalized tuple with homogeneous elements of type Base */
  type HTuple[Base, T <: GTuple[Base]] <: Boolean = T match
    case Unit       => true
    case Channel    => true
    case EmptyTuple => true
    case h *: t => h match
      case Base  => HTuple[Base,t]
      case _     => false

  /** This type helps to enforce homogeneous elements for our tuple, with a proper error notification. */
  @implicitNotFound("ShowChannels contains an element that is not a subtype of Channel in ${T} ")
  type ChannelTuple[T <: GTuple[Channel]] = HTuple[Channel,T] =:= true

  /**
   * Helper class so that you can define which channels are to be visible at logging. Use is described at the
   * showChannels method in the ActorLogger trait. Note that, since there is no proper syntax for a tuple
   * with zero or one element yet, we interpret the ShowChannels(()) as ShowChannels with a zero element
   * tuple as parameter and ShowChannels((MyChan)) as a ShowGroups(Tuple1(MyChan)). This enables consistent
   * definition of the used channels. */
  class ShowChannels[Channels <: GTuple[Channel] : ChannelTuple](channels: Channels):

    /* Local alias */
    private type Pass  = Channel.Pass.type

    /* Extract the type from the class parameter. */
    final private type ChannelMembers = Channels match
      /* The user meant an empty tuple, so we refuse any call by passing Nothing which always blocks. */
      case Unit    => Nothing
      /* For Pass we allow for all user logs, so we pass Channel, which always fits any call.*/
      case Pass    => Channel
      /* The user meant a tuple with one element, if it is the right one, we may pass this call. */
      case Channel => channels.type
      /* If this is a Tuple, we must test for the presence of a Pass by testing if it is a subtype of the Union */
      case Tuple   => Pass match
        /* If this fits, we must pass any call. */
        case Tuple.Union[Channels] => Channel
        /* Otherwise we return just the union for further testing. */
        case _                     => Tuple.Union[Channels]

    /* Compile time test to see if the channel is an element of the channels in this collection. */
    private[actors] transparent inline def contains[CH <: Channel](channel: CH): Boolean = inline channel match
      /* We always accept the Pass channel, even if there are no channels defined. */
      case _ : Pass           => true
      /* See if the channel is in the list, or is allowed for other reasons. */
      case _ : ChannelMembers => true
      /* Any other channel is rejected. */
      case _                  => false

    /** Do a runtime test on the presence of this channel. */
    private def member(channel: Channel): Boolean = channels match
      /* The user meant an empty tuple. */
      case _ : Unit       => false
      /* If there is only one element, test this one element. */
      case ch : Channel   => ch == channel
      /* If there is a tuple, test each element. */
      case tuple : Tuple  => tuple.toArray.contains(channel)
      /* Last possibility is null (which should not appear) is rejected. */
      case null           => false

    /* Predefined check on the presence of SysPrd to make this fast at runtime. */
    private[actors] val hasSysPrd = member(Channel.SysPrd)

    /* Predefined check on the presence of SysDvl to make this fast at runtime. */
    private[actors] val hasSysDvl = member(Channel.SysDvl)

  /* Type to store Entries in Arrays. Arrays are initialized with null's. It is inefficient
   * to reinitialize them with a special Nil Entry since these stay all within Leucine.
   * So we allow for Null here and make sure these never reach the end user of Leucine. */
  private type NEntry = Entry | Null

  /* If we make use of logging we want to know the moment the application started. This
   * does not need to be at the nanosecond exact, but it must be some stable point around the start.
   * The start is obtained in both nanosecond and millisecond accuracy, so we can combine both timers
   * into one.  */
  private val startNanos: Long  = System.nanoTime
  private val startMillis: Long = System.currentTimeMillis() * DateTime.millisToNanos

  /** Keeps a timestamp of the last allTerminated poll in milliseconds */
  private val lastRecent: AtomicLong = AtomicLong(startMillis)

  /** Thread update the lastRecent timer with the current value */
  private[actors] def updateRecent(): Unit = lastRecent.set(System.currentTimeMillis() * DateTime.millisToNanos)

  /**
   * Get a timestamp for the current time in nano seconds from the application start.
   * Note, since the application start cannot be determined with nano second accuracy, it
   * is build upon a time mark at global logger construction obtained with millisecond
   * accuracy. From there a reconstruction is made with the passed time in nano seconds. */
  private[actors] def getTimeStamp(timing: Timing): Long = timing match
    case Timing.Recent  => lastRecent.get
    case Timing.Millis  => System.currentTimeMillis() * DateTime.millisToNanos
    case Timing.Nanos   => startMillis + (System.nanoTime - startNanos)

  /* Unfortunately it is not possible to properly inline Scala enums and eliminate code with that.
   * Therefore Level is not implemented as enum but as old fashioned sealed trait. That way we can
   * eliminate all debug and info log calls if the minimal logging level is set to Warn. See also
   * the forum discussion: https://users.scala-lang.org/t/how-to-inline-an-enum-properly/12181 */

  /**
   * Attribute to each log level an ordinal on the type level. This makes it possible to compare
   * level at compile time an eliminate unneeded code. */
  type Ordinal[L <: Level] <: Int = L match
    case Level.Disable => 0
    case Level.Fatal   => 1
    case Level.Error   => 2
    case Level.Warn    => 3
    case Level.Info    => 4
    case Level.Beta    => 5
    case Level.Debug   => 6
    case Level.Trace   => 7

  /**
   * The different levels that are available for logging. Note that the level Disable is only there
   * as bottom level. Setting logging to this level effectively disables all logging, and should only
   * be used as temporary measure. Note that an actor may locally override this setting. This is what you
   * want, so you can zoom in on particular behavior. For the log level Fatal you can supply a special
   * handler and orderly shutdown hook if needed, which is handled before the log entry is processed.
   * Apart from the common levels: Error, Warn, Info and Debug we have the level Beta. This level is in
   * between Info and Debug. Use this level where you would normally use Debug in production. You should
   * in fact not use Debug in production, but we all have been there. We leave them in during beta testing.
   * Here the level Beta comes in handy, it provides the info you need, without having the include all
   * Debug stuff. */
  sealed trait Level extends EnumOrder[Level] :
    /* Each level is given a fixed ordinal number. The highest level (Disable) has the lowest number (0). */
    inline def ordinal: Int
    /* Names & Priorities used in other logging frameworks */
    def julName: String
    def l4jName: String
    def lgbName: String
    def julPrio: Int
    def l4jPrio: Int
    def lgbPrio: Int

    /* The use of each level is counted for informational purposes. */
    private val counter: AtomicLong = AtomicLong(0)
    /* Increment the counter by one in a thread safe manner */
    private[ActorLogger] def created(): Long = counter.incrementAndGet()
    /* Obtain the current counter value */
    private[ActorLogger] def creations: Long = counter.get


  /** Object Level contains all predefined levels. */
  object Level :
    /** Type aliases to define the compile time fixed level syntactically equal to the runtime log level. */
    type Disable = Disable.type
    type Fatal   = Fatal.type
    type Error   = Error.type
    type Warn    = Warn.type
    type Info    = Info.type
    type Beta    = Beta.type
    type Debug   = Debug.type
    type Trace   = Trace.type

    /**
     * Meaning: virtual level.
     * Usage:   level to (temporarily) disable all logging
     * Action:  none
     * Example: use if no logger spooling is available. */
    case object Disable extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Disable]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "OFF"
      def l4jName: String = "OFF"
      def lgbName: String = "OFF"
      def julPrio: Int = 1100
      def l4jPrio: Int = 0
      def lgbPrio: Int = Integer.MAX_VALUE

    /**
     * Meaning: indicates that further processing is unreliable and shutdown is imminent.
     * Usage:   report messages directly (circumvent regular logging) and initiate last goodbyes.
     * Action:  immediate, will require root cause investigation and system restart.
     * Example: in case of a caught "out of memory" or "null pointer exception". */
    case object Fatal extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Fatal]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "SEVERE"
      def l4jName: String = "FATAL"
      def lgbName: String = "ERROR"
      def julPrio: Int = 1000
      def l4jPrio: Int = 100
      def lgbPrio: Int = 40000

    /**
     * Meaning: severe disturbances in process handling, but system can continue with other tasks.
     * Usage:   to request attention from the operator and/or developer.
     * Action:  immediate, repair damage to data and investigation of the cause.
     * Example: in case of "disk full", unexpected absence of user profiles etc. */
    case object Error extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Error]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "SEVERE"
      def l4jName: String = "ERROR"
      def lgbName: String = "ERROR"
      def julPrio: Int = 1000
      def l4jPrio: Int = 200
      def lgbPrio: Int = 40000

    /**
     * Meaning: indication that something is out of the ordinary, but processing can continue.
     * Usage:   to request attention from the operator and/or developer
     * Action:  quickly investigate the cause
     * Example: any situation that you do not expect such as missing a data field etc. */
    case object Warn extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Warn]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "WARNING"
      def l4jName: String = "WARN"
      def lgbName: String = "WARN"
      def julPrio: Int = 900
      def l4jPrio: Int = 300
      def lgbPrio: Int = 30000

    /**
     * Meaning: to keep the user informed about the systems whereabouts
     * Usage:   to enable a high level reconstruction of the systems actions
     * Action:  none
     * Example: see new users, written data, new network connection, etc. */
    case object Info extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Info]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "INFO"
      def l4jName: String = "INFO"
      def lgbName: String = "INFO"
      def julPrio: Int = 800
      def l4jPrio: Int = 400
      def lgbPrio: Int = 20000

    /**
     * Meaning: to keep the developer informed about the systems whereabouts
     * Usage:   to monitor behavior for beta releases, same importance as Info.
     * Action:  none
     * Example: see new users, written data, new network connection, etc. */
    case object Beta extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Beta]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "FINE"
      def l4jName: String = "BETA"
      def lgbName: String = "DEBUG"
      def julPrio: Int = 500
      def l4jPrio: Int = 450
      def lgbPrio: Int = 10000

    /**
     * Meaning: to communicate internals of the system for diagnostic purposes.
     * Usage:   to enable a low level reconstruction of the systems actions
     * Action:  debug, refactor, code, drink coffee.
     * Example: any detail you need to know to understand possible problems */
    case object Debug extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Debug]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "FINER"
      def l4jName: String = "DEBUG"
      def lgbName: String = "DEBUG"
      def julPrio: Int = 400
      def l4jPrio: Int = 500
      def lgbPrio: Int = 10000

    /**
     * Meaning: to follow the flow of the code for diagnostic purposes.
     * Usage:   supply each class and method definition with a trace
     * Action:  debug, refactor, code, drink coffee.
     * Example: any detail you need to know to understand possible problems */
    case object Trace extends Level :
      /* Ordinal defined as inline since we need this for the compile time elimination of log entries. */
      inline def ordinal: Int = constValue[Ordinal[Level.Trace]]
      /* Names & Priorities used in other logging frameworks */
      def julName: String = "FINEST"
      def l4jName: String = "TRACE"
      def lgbName: String = "TRACE"
      def julPrio: Int = 300
      def l4jPrio: Int = 600
      def lgbPrio: Int = 5000

    /** This are all operational levels in one list. From high to low. */
    val allLevels = List(Fatal,Error,Warn,Info,Beta,Debug,Trace)

    /**
     * Take a sample from all level creations. Note, the samples are taken sequentially and
     * may not represent the number of creations at one single moment in time. */
    def sample: List[(Level,Long)] = allLevels.map(level => (level,level.creations))

    /**
     * Method to find the level given a string representation to a level. Searches for the
     * for the level ignoring the letter casing. */
    def fromString(name: String): Option[Level] = allLevels.find(_.toString.equalsIgnoreCase(name))


  /**
   * The spooling trait is for defining the way spooling is done with one parameter. There are basically
   * two modes. Or we directly process each log, or we collect them a while, and then process them. In the
   * latte case some kind of trigger is needed to initiate the spooling. This can either be that there are
   * too many logs, or it has taken too long since the last spool or a very important log passed by. */
  sealed trait Spooling :
    def direct: Boolean
    def size: Int
    def time: FiniteDuration
    def level: Level

  /* Companion object for Spooling holding the user settings. */
  object Spooling :
    /** Use this to bypass any log queuing and process the logs immediately. */
    case object Direct extends Spooling :
      final def direct = true
      final def size   = 0
      final def time   = Duration.Zero
      final def level  = Level.Disable
    /** Use this to periodically spool the logs when either of the parameters has been reached. */
    case class Periodic(size: Int, time: FiniteDuration, level: Level) extends Spooling :
      final def direct = false

  /**
   * Class to pack your log messages when they contain (extensive) calculations or may trigger exceptions
   * at evaluation. For regular log message just use standard strings. If you pack your string in this
   * class the evaluation will be postponed until all tests and filters are done. If the log does not make
   * it to the spooler, the string is never evaluated. If it does, it is evaluated at most one time, but this
   * may take place in an other thread. Make sure the string calculator is thread safe. */
  class Slow private (msg: () => String) :
    private lazy val _msg = try msg() catch case e: Exception => s"Exception in log statement: ${e.getMessage}"
    override def toString: String = _msg

  object Slow:
    /** Convenience packer for Slow. */
    def apply(msg: => String): Slow = new Slow(() => msg)

  /** Trait the user can implement to filter logs before they are passed to spooler.  */
  trait Filter :
    /** Filter method to filter on the log level, source location and enclosing kind of the log instruction. */
    def onSource(level: Level, kind: Kind, path: String): Boolean
    /** Filter method to filter on the log level and the name or full family path of the actor */
    def onActor(level: Level, path: String): Boolean
    /** Filter method to filter on the log level and contents of the message. */
    def onMessage(level: Level, msg: => String): Boolean

  /* Companion object of Filter trait. */
  object Filter :
    /** Filter object that allows all logs to pass. This is the default value. */
    object Pass extends Filter :
      final def onSource(level: Level, kind: Kind, path: String) = true
      final def onActor(level: Level, path: String)              = true
      final def onMessage(level: Level, msg: => String)          = true

  /** This class is used to capture data to be passed around in the framework. */
  final class Capture(
      /* All fields are private because this class is not for use outside the framework. Note the
       * class itself cannot be made private for it is inlined in user code. */
      private[actors] val level: Level,
      private[actors] val channel: Channel,
      private[actors] val filter: Filter,
      private[actors] val sourceKind: Kind,
      private[actors] val sourcePath: String,
      private[actors] val message: String | Slow,
      private[actors] val thrown: Option[Throwable] = None) :
    /* All methods are private because this class is not for use outside the framework. */
    private[actors] def passActor(actorPath: String): Boolean = filter.onActor(level,actorPath)
    private[actors] def passSource: Boolean  = filter.onSource(level,sourceKind,sourcePath)
    private[actors] def passMessage: Boolean = filter.onMessage(level,message.toString)
    private[actors] def getMessage: String   = message.toString


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
   * level:      the log level of this entry, one of (System,Fatal,Error,Warn,Info,Beta,Debug,Trace).
   * timing:     the log timing granularity (second/millisecond/nanosecond).
   * timestamp:  number of nanoseconds starting from the Unix Epoch (granularity maybe less)
   * channel:    name of the channel the log is published in.
   * threadName: name of the thread the log was made in.
   * actorName:  name of the actor the log was made in (empty if outside the actor framework).
   * className:  full name of the enclosing class the log was made in.
   * message:    the log message from the developer.
   * It is not possible to create instances directly from the class definition since they will not be
   * accounted for. Use the factory methods to that end. */
  class Entry private (
      val index:       Long,
      val level:       Level,
      val counter:     Long,
      val timing:      Timing,
      val timestamp:   Long,
      val channel:     Channel,
      val threadName:  String,
      val actorName:   String,
      val sourceKind:  Kind,
      val sourcePath:  String,
      val thrown:      Option[Throwable],
      val message:     String) :

    /** Return the timestamp as a java Instant. */
    def getInstant: Instant = DateTime.toInstant(timestamp)

    /** Return the string representation of the fields as Map for external loggers. */
    def getFields: Map[String,String] = Map(
      "index"      -> index.toString,
      "level"      -> level.toString,
      "counter"    -> counter.toString,
      "timing"     -> timing.toString,
      "timestamp"  -> timestamp.toString,
      "channel"    -> channel.toString,
      "thread"     -> threadName,
      "actor"      -> actorName,
      "kind"       -> sourceKind.toString,
      "path"       -> sourcePath,
      "thrown"     -> thrown.map(_.getMessage()).getOrElse(""),
      "message"    -> message)

    /**
     * Return a single line string representation of all fields for external loggers. With pretty set to
     * true it tries its best to make this look good when printed to the console or in a plain text file.
     * Absent fields are ignored, numbers are filled to three positions and level name to five positions.
     * This results in semi stable column widths. Since the thread and actor names can vary considerably
     * column widths are only fixed for the first few columns. When pretty is set to false, fields are
     * converted to bare strings and empty ones are not skipped. Use this to construct csv or html makeups. */
    def getList(pretty: Boolean): List[String] =
      val result = new ListBuffer[String]()
      def append(content: String, label: String = ""): Unit =
        if      !pretty           then result.append(content)
        else if content.isEmpty   then ()
        else if label.isEmpty     then result.append(content)
        else if label(0)=='%'     then append(label.format(content))
        else                           result.append(s"$label($content)")
      val datetime  = DateTime(timestamp)
      val subsecStr = timing match
        case Timing.Recent => ""
        case Timing.Millis => f".${datetime.milli}%03d"
        case Timing.Nanos  => f".${datetime.nano}%09d"
      append(index.toString,"%3s")
      append(level.toString,"%5s")
      append(s"#$counter","%4s")
      append(s"${datetime.dateStr("-")}T${datetime.timeStr(":")}$subsecStr")
      append(channel.toString,"Channel")
      append(threadName,"Thread")
      append(actorName,"Actor")
      if pretty then append(sourcePath,sourceKind.toString) else { append(sourceKind.toString); append(sourcePath) }
      thrown match
        case Some(throwable) => append(throwable.getMessage(),"Thrown")
        case None            => if !pretty then result.append("")
      append(message)
      result.toList

    /** Default (pretty) string representation of the entry. */
    override def toString: String = getList(true).mkString("Log(","; ",")")


  /** Object Entry contains the factory methods for Entry and the thread save counter.*/
  object Entry :

    /* Logs entries are indexed with strict order. This counter is used to order all entries when spooled.
     * In case of very long uptimes of an application it may happen that an Int is not enough to count for
     * entries. Therefore we make use of a long here. */
    private val index: AtomicLong = AtomicLong(1)

    /** Thread save manner to obtain a the index for a log entries */
    private[actors] def getIndex: Long = index.get

    /** Thread save manner to obtain a new index for a log entry */
    private def getAndIncIndex(): Long = index.getAndIncrement()

    /**
     * Construct an log entry based on the given data and add a timestamp, an index,  a thread name, timing and
     * actor path name (if available). Every entry that is created gets a new unique index number, and the level
     * at which it is created also increases a use counter. */
    def apply(actorName: String, timing: Timing, capture: Capture): Entry =
      val counter    = capture.level.created()
      val index      = getAndIncIndex()
      val timeStamp  = getTimeStamp(timing)
      val threadName = Thread.currentThread().getName()
      /* The actorName should not be null, but we are better save than sorry. */
      val safeName   = if actorName == null then "???" else actorName
      new Entry(index,capture.level,counter,timing,timeStamp,capture.channel,threadName,safeName,
                capture.sourceKind,capture.sourcePath,capture.thrown,capture.message.toString)


  /**
   * Simple sorting routine to sort all entries in the holder upon index. Returns an
   * (immutable) IArray. Note that some index values may be missing, which is due to
   * the asynchronicity of the logging mechanism. Usually they are provided in the
   * next call. For efficiency reasons, these values are null instead of some custom
   * Nil variant of Entry. You may override this with your own implementation. */
  def sort(hold: Hold[List[Entry]]): IArray[NEntry] =
    val slots: Array[NEntry] = new Array[NEntry](hold.width)
    val outer = hold.entries.iterator
    while outer.hasNext do
      val inner = outer.next().iterator
      while inner.hasNext do
        val entry = inner.next()
        val index = (entry.index - hold.min).toInt
        assert(index >= 0 && index < hold.width,s"Index $index out of bounds: hold = $hold")
        slots(index) = entry
    IArray.unsafeFromArray(slots)

  /** Just spool all log entries unsorted. Use if your own logging framework takes care of this. */
  def simpleSpool(hold: Hold[List[Entry]], process: Entry => Unit): Unit = hold.entries.foreach(_.foreach(process))

  /**
   * Collects all log entries from all different logHolders, orders them and simply spools them
   * one by one to the custom implemented process method. Note that some entries might be missing,
   * due to the asynchronicity of the logging mechanism. This method is periodically called by
   * the Actor Guard to transfer the log items to the user defined logging system. You may override
   * this method if you want to handle the entries in bulk yourself. Completed will be true on
   * the very last call to spool containing entries. The application will terminate soon after.
   * You may want to use this information when building your own sorted logger. */
  def sortedSpool(hold: Hold[List[Entry]], process: Entry => Unit): Unit = sort(hold).withFilter(_ != null).foreach(process)

  /**
   * Spool method where we stitch new log entries before processing. Stitching means that if the
   * current spool session has missing entries, that part will be temporarily stored for reprocessing
   * in the next run of spool. That way we obtain strict ordering with respect of the log index, at the
   * cost of a slight delay of processing. When flush is true all remaining logs will be spooled. We
   * will limit the temporary storage array to maxArraySize. If it gets bigger, log entries are spooled
   * anyway (and the strict order may be broken). */
  def stichedSpool(hold: Hold[List[Entry]], store: Store, maxArraySize: Int, flush: Boolean, process: Entry => Unit): Store =

    /** Way to make the contents of an array visible. For development of this method.*/
    def arrayToString(title: String, array: IArray[Entry | Null]): String =
      def convert(x: Entry | Null): String = if x==null then "---" else f"${x.index}%03d"
      array.map(convert).mkString(s"$title = |",",","|")

    /* Sort the new entries into an array. */
    val currEntries = sort(hold)
    val lastEntries = store.entries

    /* Since we have no real idea how the arrays appear after each other, it is best to work with two phases,
     * where we walk through the arrays simultaneously, starting at the most early point from both of them.
     * We first have a send phase that sends the entries to the process method for as longs as the entries
     * are continuously available in either array. The first moment an entry is missing in both, we start the
     * merge phase. This array will therefore always start with an null entry or be empty. The index of that missing
     * entry will be our new start. We must further build in a safety measure that the stored array does not become
     * too long. At some time its better to disrupt the order than to blow up the memory. Also, when all actors
     * are finished, we spool anything that is left, since there will be no one around to produce new logs. */

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

    /** If the assert below happens to fire, we might need to know the contents of both arrays. */
    def assertReport(message: String): String =
      s"""${message}:
         |  ${arrayToString("lastEntries",lastEntries)}
         |  ${arrayToString("currEntries",currEntries)}""".stripMargin

    /* Verify if the construction assumptions about the array hold. We need those values to start processing. */
    assert(lastEntries.isEmpty || lastEntries(0) == null, assertReport("Array lastEntries must start with a null or be empty"))
    assert(currEntries.isEmpty || currEntries(0) != null, assertReport("Array currEntries may not start with a null"))

    /* Get for both arrays the real start index. */
    val lastEntryIndexStart = if lastEntries.isEmpty then 0 else store.start
    val currEntryIndexStart = if currEntries.isEmpty then 0 else currEntries(0).index

    /* We start the processing where we left of last time. If this is the first time we enter,
     * we start at index 1, the lowest possible index value. It may happen that on the first run
     * the first log entry is not present.  */
    val startIndex = math.max(math.min(store.start,currEntryIndexStart),1L)

    /* We stop at the highest possible log-index-number. Note this is one beyond the last usable value. */
    val endIndex = math.max(lastEntryIndexStart+lastEntries.length,currEntryIndexStart+currEntries.length)

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
     * We ignore gaps when we are terminating (flush is true, but then there should not be any gaps) or when
     * there the gaps are so early that we must store more then the maximum amount of entries. This breaks the
     * strict ordering, but is further not harmful. We also stop when we reached the end. */
    def sendPass(index: Long): Long = if index >= endIndex then endIndex else
      /* See which array has an entry */
      val hasLast = hasLastEntry(index)
      val hasCurr = hasCurrEntry(index)
      /* It cannot be so that both arrays have an entry on the same log-index. */
      assert(!hasLast || !hasCurr, assertReport(s"Two log entries with the same index $index"))
      /* Process the available entry (or none if there isn't any). */
      if hasLast then process(getLastEntry(index))
      if hasCurr then process(getCurrEntry(index))
      /* Break if we have a gap but are not flushing and the merged array will not become to large. */
      if !hasLast && !hasCurr && !flush && (endIndex - index < maxArraySize) then index else sendPass(index + 1)

    /* The remainder of the values must be merged into one array (or just copied if the other is shorter) */
    def mergePass(index: Long, offset: Long, result: Array[NEntry]): Array[NEntry] = if index >= endIndex then result else
      /* See which array has an entry */
      val hasLast = hasLastEntry(index)
      val hasCurr = hasCurrEntry(index)
      /* It cannot be so that both arrays have an entry on the same log-index. */
      assert(!hasLast || !hasCurr, assertReport(s"Two log entries with the same index $index"))
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
    Store(sendIndex,IArray.unsafeFromArray(result))
