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

/**
 * The LogHandler makes available all logging methods like log.warn(...) etc. The trait is used as
 * internal extension for the actor logger. Not for external use.
 */
private trait LogHandler :
  import ActorLogger.{Entry, Level, Ordinal}
  /* FixLevel is the level set by the user to make sure we can eliminate unreachable log statements
   * at compile time. */
  type FixLevel <: Level

  /* With direct you can force all log call to be made directly */
  type Direct <: Boolean

  /* With Develop you can incorporate the develop logger statements. */
  type Develop <: Boolean

  /* Select if you want full class paths in your logs or just class names. */
  type FullPath <: Boolean


  // TODO: why can't i refactor this using inline val fixLevel = constValue[Ordinal[FixLevel]]
  // here with fixLevel as substitute for the expression on the method feed?? This leads to an
  // inlining error

  /**
   * General method for feeding the logger with log statements. Due to inlining it is completely
   * stripped down to the statements that are relevant at the logging level of execution. */
  // TODO: why is it not possible to remove the [actors] from private[actors] here?? The method is
  // completely private, but it leads to an inlining error.
  inline private[actors] def feed(inline level: Level, inline className: String, inline direct: Boolean, inline message: String): Unit =
    /* See if the current fixed level surpassed the level of this entry, if not, we are done. If this
     * method is called from the fixed level methods or with a compile time constant in in the variable
     * log methods code will be eliminated when not reachable. We do not out 'inline if' here because
     * we want to allow for variable level use as well. */
    if level.ordinal <= constValue[Ordinal[FixLevel]] then
      /* If we are dealing with a Fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      if level.ordinal == Level.Fatal.ordinal then handleFatal(message)
      /* Now, construct and feed if needed the log entry directly to the process handler or to the log queue */
      inline if direct
        then LogHolder.entry(false,level,className,message).foreach(process)
        else LogHolder.entry(true,level,className,message).foreach(trySpool)

  /** This method is called for every log entry when the entries are spooled. */
  def process(entry: Entry): Unit

  /** Test if we have enough new logs for spooling */
  private[actors] def trySpool(entry: Entry): Unit

  /** Implement a handler for the event a fatal situation occurs */
  def handleFatal(message: String): Unit

  /**
   * Make lazy (delayed) log entry with level Fatal (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when FixLevel is set to Ignore. */
  inline def fatal(message: => String): Unit = feed(Level.Fatal,StaticInfo.pathInfo(constValue[FullPath]),constValue[Direct],message)

  /**
   * Make lazy (delayed) log entry with level Error (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when FixLevel is set to Ignore or Fatal. */
  inline def error(message: => String): Unit = feed(Level.Error,StaticInfo.pathInfo(constValue[FullPath]),constValue[Direct],message)

  /**
   * Make lazy (delayed) log entry with level Warn (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when FixLevel is set to Ignore, Fatal or Error */
  inline def warn(message: => String): Unit  = feed(Level.Warn,StaticInfo.pathInfo(constValue[FullPath]),constValue[Direct],message)

  /**
   * Make lazy (delayed) log entry with level Info (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when FixLevel is set to Ignore, Fatal, Error or Warn */
  inline def info(message: => String): Unit  = feed(Level.Info,StaticInfo.pathInfo(constValue[FullPath]),constValue[Direct],message)

  /**
   * Make lazy (delayed) log entry with level Debug (see ActorLogger.Level for documentation on the level.
   * This call is eliminated from the code when FixLevel is set to Ignore, Fatal, Error, Warn or Info. */
  // Opletten, we geven de Method niet door als we debuggen. Hmm, missschien toch maar wel?
  inline def debug(message: => String): Unit = feed(Level.Debug,StaticInfo.pathInfo(constValue[FullPath]),constValue[Direct],message)

  // Voor debug en trace twee pathFilters inbouwen, een voor de het sourcePath (object/class/method) en
  // een voor het actorPath. Het zijn string filters. Het sourcePath zou compiletime kunnen (mits inlined)
  // het actorPath niet, want die komt uit de logholder.
  // pathFilter: String => Boolean Deze filters worden in de actorLogger eenmaal ingesteld en elke debug/ trace
  // wordt er doorheen gehaald. Default is: alles doorlaten.
  //  voor trace ook een groepfilter inbouwen, het trace command krijgt een groepName parameter (optional).
  // Als die er is, wordt er voor getest.
  // WBT short/full classNames. dit gaat met een default instelling die je kan aanpassen, en per debug/trace
  // kan overridden (alleen voor debug/trace)
  inline def trace(): Unit = println("### "+StaticInfo.callInfo(constValue[FullPath],true))

  /**
   * Make a log entry with variable level which is directly spooled. This call is eliminated from the code
   * when FixLevel is set to a higher level than level in the call, on a best effort basis (for example when
   * called with a constant level value). This call is always eliminated when Develop is set to false. */
//  inline def direct(level: Level, message: => String): Unit =
//    inline if constValue[Develop] then feed(level,StaticInfo.className,true,message)


  /**
   * Make a lazy log entry with variable level (this is spooled later). This call is eliminated from the code
   * when FixLevel is set to a higher level than level in the call, on a best effort basis (for example when
   * called with a constant level value). This call is always eliminated when Develop is set to false. */
//  inline def delayed(level: Level, message: => String): Unit =
//    inline if constValue[Develop] then feed(level,StaticInfo.className,false,message)
