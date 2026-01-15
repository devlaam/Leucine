package s2a.leucine.actors

import scala.quoted.*

object Caller:
  inline def className: String = ${ classNameImpl }

  private def classNameImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    def enclosingClass(sym: Symbol): Symbol =
      if sym == Symbol.noSymbol then sym
      else if sym.isClassDef then sym
      else enclosingClass(sym.owner)

    val cls = enclosingClass(Symbol.spliceOwner)
    val name =
      if cls == Symbol.noSymbol then "???"
      else cls.fullName // or .name / stripSuffix("$") etc.
    Expr(name)



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

// import java.util.concurrent.atomic.AtomicLong


// /** Generic interface for Log objects as presented to the user. */
// trait Log :
//     import ActorLogger.{Level, Timing}
//     /** Make log entry with level Fatal (see ActorLogger.Level for documentation) */
//     def fatal(message: => String): Unit
//     /** Make log entry with level Error (see ActorLogger.Level for documentation) */
//     def error(message: => String): Unit
//     /** Make log entry with level Warn (see ActorLogger.Level for documentation) */
//     def warn(message: => String): Unit
//     /** Make log entry with level Info (see ActorLogger.Level for documentation) */
//     def info(message: => String)(using logInfo: LogINFO): Unit
//     /** Make log entry with level Debug (see ActorLogger.Level for documentation) */
//     def debug(message: => String): Unit
//     /** Request the current active logging level (see ActorLogger.Level for documentation) */
//     def level: Level
//     /** Request the current active logging timing (see ActorLogger.Timing for documentation) */
//     def timing: Timing


// private class LogINFO(val klass: () => String)


// private object LogINFO :
//   /* In case such information is not available, empty is used. */
//   val empty = LogINFO(() => "???")


// trait LogClass :
//   private val klass: () => String = () => getClass.getSimpleName()
//   given LogINFO(klass)


// /** Trait that implements all handling of logging operations. */
// trait LogHandle :
//   import ActorLogger.Level
//   private[actors] def logFatal(message: => String): Unit
//   private[actors] def logEnqueue(level: Level, klass: String, message: => String): Unit
//   /** Make log entry with level Fatal (see ActorLogger.Level for documentation) */
//   def fatal(message: => String): Unit = logFatal(message)
//   /** Make log entry with level Error (see ActorLogger.Level for documentation) */
//   def error(message: => String): Unit = logEnqueue(Level.Error,"x",message)
//   /** Make log entry with level Warn (see ActorLogger.Level for documentation) */
//   def warn(message: => String): Unit  = logEnqueue (Level.Warn,"x",message)
//   /** Make log entry with level Info (see ActorLogger.Level for documentation) */
//   def info(message: => String)(using logInfo: LogINFO): Unit  = logEnqueue(Level.Info,logInfo.klass(),message)
//   /** Make log entry with level Debug (see ActorLogger.Level for documentation) */
//   def debug(message: => String): Unit = logEnqueue(Level.Debug,"x",message)


// /** Trait that holds and manages the log entries */
// trait LogStore :
//   import ActorLogger.{Level, Timing, Entry}
//   import LogAid.{getIndex, getTimeStamp}

//   private[actors] def level: Level
//   private[actors] def timing: Timing

//   private[actors] def thread: String
//   private[actors] def actor: String
//   private[actors] def clazz: () => String

//   /* Make a local copy of the active logger. Possible since the logger may not be changed during
//    * the lifetime of the application. */
//   protected val logger: ActorLogger = ActorGuard.logger

//   //Holds all log entries per actor, we might use a shared queue for this later,
//   //but a list is probably enough and less heavy, since every actor will have it.
//   private var entries: List[Entry] = Nil

//   /* And an entry to the log list if the current level allows for it. */
//   private[actors] def logEnqueue(level: Level, klass: String, message: => String): Unit = if level <= this.level then
//     val timeStamp = getTimeStamp(timing)
//     val index     = getIndex()
//     val entry     = Entry(index,level,timing,timeStamp,thread,actor,klass,message)
//     entries ::= entry

//   private[actors] def logFatal(message: => String): Unit = if Level.Fatal <= this.level then
//     val timeStamp = getTimeStamp(timing)
//     val index     = getIndex()
//     val entry     = Entry(index,Level.Fatal,timing,timeStamp,thread,actor,clazz(),message)
//     logger.handleFatal(entry)

//   private[actors] def isEmpty: Boolean = entries.isEmpty

//   private[actors] def getAndClear: List[Entry] =
//     val copy = entries
//     entries = Nil
//     copy



// /* Methods stub for when there is no timing mixin used. */
// private trait LogDefs extends BareDefs :
//   private[actors] def logSpool(): Unit = ()

// /**
//  * Mixin which enables fast local logging in the actor. The logging statements are collected and
//  * combined at times which minimize delays in the concurrent handling of actors. */
// trait LogAid extends LogStore, ActorInit, ActorDefs :
//   this: BareActor =>

//   import ActorLogger.{Level, Timing}
//   import LogAid.logCollect

//   val self: LogAid = this

//   private var _level: Level   = logger.level
//   private var _timing: Timing = logger.timing
//   private[actors] def level: Level    = if (logger.local) then _level else logger.level
//   private[actors] def timing: Timing  = if (logger.local) then _timing else logger.timing

//   private[actors] def thread: String = Thread.currentThread().getName()
//   private[actors] def actor: String  = path
//   private[actors] def clazz: () => String  =  () => getClass.getSimpleName()


//   private[actors] override def logSpool(): Unit = if !isEmpty then logCollect(getAndClear)


//   /** Interface to be used by the actor user to file and control log messages. */
//   protected object Log extends LogHandle, Log:

//     private[actors] def logFatal(message: => String): Unit = self.logFatal(message)
//     private[actors] def logEnqueue(level: Level, klass: String, message: => String): Unit = self.logEnqueue(level,klass,message)

//     /** Set a new active logging level for this actor. Fails when local changes are prohibited. */
//     def level_= (level: Level): Unit = _level = level
//     /** Request the current active logging level (see ActorLogger.Level for documentation) */
//     def level: Level = self.level

//     /** Set a new active logging timing for this actor. Fails when local changes are prohibited. */
//     def timing_= (timing: Timing): Unit =  _timing = timing
//     /** Request the current active logging timing (see ActorLogger.Timing for documentation) */
//     def timing: Timing = self.timing

//     /** Resets logging timing and level to their default values. */
//     def reset(): Unit =
//       _level  = logger.level
//       _timing = logger.timing

//   /* Called to count this trait */
//   private[actors] override def initCount: Int = super.initCount + 1

//   /* Signal that this trait is instantiated */
//   initReady()

//   given Log = Log
//   given LogINFO(() => getClass.getSimpleName())


// object LogAid :
//   import ActorLogger.{Timing, Entry}

//   private val startNanos: Long  = System.nanoTime
//   private val startMillis: Long = System.currentTimeMillis()
//   private val logCounter: AtomicLong = AtomicLong(1)
//   // Misschien moeten de allEntries verhuizen naar het ActorLogger object and actorEntriess gaan heten.
//   private var allEntries: List[List[Entry]] = Nil
//   private var lastIndex: Long = 1


//   /** Thread save manner to obtain a new index for a log entry */
//   private[actors] def getIndex(): Long = logCounter.getAndIncrement()

//   private[actors] def getTimeStamp(timing: Timing): Long = timing match
//     case Timing.Recent  => ActorGuard.getLastSpool * 1000000
//     case Timing.Millis  => System.currentTimeMillis() * 1000000
//     case Timing.Nanos   => startMillis * 1000000 + (System.nanoTime - startNanos)

//   private[actors] def logCollect(entries: List[Entry]): Unit = synchronized { allEntries ::= entries }
//   private[actors] def logRetrieve(): List[List[Entry]] = synchronized {
//     val copy = allEntries
//     allEntries = Nil
//     copy }


//   // Is dit nodig?
//   private def firstNull(array: Array[Entry]): Int =
//     var i = 0
//     val l = array.length
//     while (i<l && array(i)!=null) do i += 1
//     i


//   //Opletten, als we bij een ontbekende logentry blijven stoppen kan dat fout gaan.
//   //Op zich mogen er geen logs verloren gaan, maar als het toch gebeurd, is het beter
//   //dat we niet eindeloos wachten. Hoe lossen we dat op?

//   //Is dit nodig?
//   def logOrder(): Array[Entry] =
//     val allEntries = logRetrieve()
//     // Hier krijgen we een bovenwaarde van Index.
//     val maxEntries = getIndex()-lastIndex
//     val array: Array[Entry] = new Array[Entry](maxEntries.toInt)
//     for
//       entries <- allEntries
//       entry   <- entries
//     do array((entry.index - lastIndex).toInt) = entry

//     ???

//   def logSpool(entries: Iterable[Entry]): Unit =


//     ???





