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
import java.lang.ThreadLocal

import scala.compiletime.constValue


class LogHolder(val path: String, val level: ActorLogger.Level, val timing: ActorLogger.Timing) :
  import ActorLogger.{Level, Entry}
  import LogAid.{getIndex, getTimeStamp}
  private var entries: List[Entry] = Nil
  private[actors] def isEmpty: Boolean = entries.isEmpty
  private[actors] def make(level: Level, className: String, message: String): Entry =
    val timeStamp = getTimeStamp(timing)
    val index     = getIndex()
    val thread    = Thread.currentThread().getName()
    Entry(index,level,timing,timeStamp,thread,path,className,message)
  private[actors] def pass(level: Level): Boolean = level <= this.level
  private[actors] def add(entry: Entry): Unit = entries ::= entry
  private[actors] def get: List[Entry] = entries
  private[actors] def clear(): Unit = entries = Nil


object LogHolder :
  import ActorLogger.{Level, Entry}

  /** Contains all of the logs collected from within actors (via ThreadLocal LogHolders) */
  private var accuEntries: List[List[Entry]] = Nil

  // De vraag is of we de synchronized moeten splitsen voor allEntries en mainLogHolder,
  // deze zitten elkaar meestal niet in de weg
  /** Add some entries to the full collection. */
  private def addToAccu(entries: List[Entry]): Unit = synchronized { accuEntries ::= entries }

  /**
   * Temporary contains all log entries that could not be logged via a threadLocal collection. Usually
   * these are logs in the main thread. Logs from other threads are allowed, but delay execution due
   * to the necessary synchronization. */
  private def globalLog: LogHolder = ActorGuard.logger.logHolder

  /** A per thread logHolder for logs that are produced by the actors. */
  private val localLog: ThreadLocal[LogHolder] = ThreadLocal[LogHolder]()

  /** Places a new container (LogHolder) for logs on this thread. */
  private[actors] def initLocal(logHolder: LogHolder): Unit = localLog.set(logHolder)

  /**
   * Add a new log entry to the log of the current thread, if that thread has an active local container. If not,
   * the entry is directed to the global container. Returns the constructed entry for further processing.  */
  private[actors] def feed(level: Level, className: String, message: String): Unit =
    /* Try to make a log entry on the given holder. Sync when needed. */
    def tryLog(holder: LogHolder, sync: Boolean): Unit = if holder.pass(level) then
      /* Construct the entry on the holder. */
      val entry = holder.make(level,className,message)
      /* Add the entry to the log queue. Synchronize this call if needed */
      if sync then synchronized { holder.add(entry) } else holder.add(entry)

    /* Try to obtain the local logHolder in this thread. */
    val localHolder: LogHolder = localLog.get()
    /* Since this is a Java call it may return null. If so, we do not have a container, and we must try the
     * globalHolder as fallback and synchronize the log addition. Otherwise we use the obtained holder for
     * thread local handling. Since we are in this thread, no synchronize needed. */
    if localHolder == null then tryLog(globalLog,true) else tryLog(localHolder,false)

  /**
   * Try to empty and remove the threadLocal logHolder. Afterwards, the holder is removed from the thread for it
   * may be reused for other purposes. This can be an actor, but also a future or other manual use. */
  private[actors] def emptyLocal(): Unit =
    /* Get the logHolder for this thread */
    val holder = localLog.get()
    /* If it is not there (null) there is nothing to do. If it is we ... */
    if holder != null then
      /* ... and it has some content then ... */
      if !holder.isEmpty then
        /* ... copy the entries to mainEntries if any ... */
        addToAccu(holder.get)
        /* .. remove the content from the holder for reuse. */
        holder.clear()
      /* and remove the holder to ensure it is not re- or misused by an other actor on the thread. */
      localLog.remove()

  /**
   * Collect all the logs that are present.
   */
  private[actors] def fullRetrieve(): List[List[Entry]] = synchronized :
    val sum = globalLog.get :: accuEntries
    globalLog.clear()
    accuEntries = Nil
    sum


trait LogHandler :
  import ActorLogger.*
  type FixLevel <: Level

  inline private[actors] def logEnqueue(inline level: Level, className: String, message: => String): Unit =
    /* See if the current fixed level surpassed the level of this entry, if not, we are done. */
    //inline if level.ordinal <= fixLevelOrdinal then
    inline if level.ordinal <= constValue[Ordinal[FixLevel]] then
      /* If we are dealing with a Fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      if level == Level.Fatal then handleFatal(message)
      /* Now, construct and feed the log entry on the log queue */
      LogHolder.feed(level,className,message)

  /** Implement a handler for the event a fatal situation occurs */
  def handleFatal(message: String): Unit
  /** Make log entry with level Fatal (see ActorLogger.Level for documentation) */
  inline def fatal(message: => String): Unit = logEnqueue(Level.Fatal,Caller.className,message)
  /** Make log entry with level Error (see ActorLogger.Level for documentation) */
  inline def error(message: => String): Unit = logEnqueue(Level.Error,Caller.className,message)
  /** Make log entry with level Warn (see ActorLogger.Level for documentation) */
  inline def warn(message: => String): Unit  = logEnqueue (Level.Warn,Caller.className,message)
  /** Make log entry with level Info (see ActorLogger.Level for documentation) */
  inline def info(message: => String): Unit  = logEnqueue(Level.Info,Caller.className,message)
  /** Make log entry with level Debug (see ActorLogger.Level for documentation) */
  inline def debug(message: => String): Unit = logEnqueue(Level.Debug,Caller.className,message)

/** Methods stub for when there is no logging mixin used. */
private[actors] trait LogDefs extends BareDefs :
  private[actors] def logInit(): Unit = ()
  private[actors] def logExit(): Unit = ()


/**
 * Mixin which enables fast local logging in the actor. The logging statements are collected and
 * combined at times which minimize delays the concurrent handling of actors. If you do not mixin
 * this trait, logging inside the actor and called instances and methods still work, but each log
 * call synchronizes separately. And you are unaware in which actor the log call was executed.  */
//trait LogAid extends LogClass, ActorInit, ActorDefs :
trait LogAid extends ActorInit, ActorDefs :
  this: BareActor =>

  import ActorLogger.{Level, Timing}


  private def fixedLevel  = ActorGuard.logger.logHolder.level
  private def fixedTiming = ActorGuard.logger.logHolder.timing
  private val fixedHolder = LogHolder(path,fixedLevel,fixedTiming)

  /** Keeps the actor local logHolder ready for use. */
  private def logHolder: LogHolder =
    if ActorGuard.logger.localSettings
    then LogHolder(path,logLevel,logTiming)
    else fixedHolder

  // Overriden kan dan in de actor.
  protected def logLevel: Level = fixedLevel
  protected def logTiming: Timing = fixedTiming


  private[actors] override def logInit(): Unit = LogHolder.initLocal(logHolder)
  private[actors] override def logExit(): Unit = LogHolder.emptyLocal()

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()



object LogAid :
  import ActorLogger.Timing

  private val startNanos: Long  = System.nanoTime
  private val startMillis: Long = System.currentTimeMillis()
  private val logCounter: AtomicLong = AtomicLong(1)

  /** Thread save manner to obtain a new index for a log entry */
  private[actors] def getIndex(): Long = logCounter.getAndIncrement()

  private[actors] def getTimeStamp(timing: Timing): Long = timing match
    case Timing.Recent  => ActorGuard.getLastSpool * 1000000
    case Timing.Millis  => System.currentTimeMillis() * 1000000
    case Timing.Nanos   => startMillis * 1000000 + (System.nanoTime - startNanos)
