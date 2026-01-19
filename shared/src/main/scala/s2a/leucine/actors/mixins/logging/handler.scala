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
  import ActorLogger.{Level, Ordinal}
  /* FixLevel is the level set by the user to make sure we can eliminate unreachable log statements
   * at compile time. */
  type FixLevel <: Level

  // TODO: why can't i refactor this using inline val fixLevel = constValue[Ordinal[FixLevel]]
  // here with fixLevel as substitute for the expression on the method feed?? This leads to an
  // inlining error

  /**
   * General method for feeding the logger with log statements. Due to inlining it is completely
   * stripped down to the statements that are relevant at the logging level of execution. */
  // TODO: why is it not possible to remove the [actors] from private[actors] here?? The method is
  // completely private, but it leads to an inlining error.
  inline private[actors] def feed(inline level: Level, className: String, message: => String): Unit =
    /* See if the current fixed level surpassed the level of this entry, if not, we are done. */
    inline if level.ordinal <= constValue[Ordinal[FixLevel]] then
      /* If we are dealing with a Fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      inline if level.ordinal == Level.Fatal.ordinal then handleFatal(message)
      /* Now, construct and feed the log entry to the log queue */
      LogHolder.feed(level,className,message)

  /** Implement a handler for the event a fatal situation occurs */
  def handleFatal(message: String): Unit
  /** Make log entry with level Fatal (see ActorLogger.Level for documentation) */
  inline def fatal(message: => String): Unit = feed(Level.Fatal,CallingClass.fullName,message)
  /** Make log entry with level Error (see ActorLogger.Level for documentation) */
  inline def error(message: => String): Unit = feed(Level.Error,CallingClass.fullName,message)
  /** Make log entry with level Warn (see ActorLogger.Level for documentation) */
  inline def warn(message: => String): Unit  = feed (Level.Warn,CallingClass.fullName,message)
  /** Make log entry with level Info (see ActorLogger.Level for documentation) */
  inline def info(message: => String): Unit  = feed(Level.Info,CallingClass.fullName,message)
  /** Make log entry with level Debug (see ActorLogger.Level for documentation) */
  inline def debug(message: => String): Unit = feed(Level.Debug,CallingClass.fullName,message)
