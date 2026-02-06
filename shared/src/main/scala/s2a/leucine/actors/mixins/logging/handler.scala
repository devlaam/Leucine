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
  import ActorLogger.{Entry, Level, Ordinal, ShowGroups, GroupBase}
  import Static.{Kind, kindInfo, pathInfo, callInfo}
  import LogHolder.{ActorFilter, allPass}

  /* FixLevel defines the logging level used at compile time. */
  type FixPassLevel <: Level

  /* With directSpool you can force all log call to be made directly. */
  def directSpool: Boolean

  /* Select if you want full class paths in your logs or just class names. */
  type FullPath <: Boolean

  /* Select if you want all parameters with values in your traces or just dots. */
  type FullParameters <: Boolean

  /* Select if you want to see the confidential log messages or public ones. */
  type ShowConfidential <: Boolean

  /* Filter which log entries may pass based on the level and path/name of the origin (object,class,method) */
  def sourcePathFilter(level: Level, path: String): Boolean

  /* Filter which log entries may pass based on the level and current actor path/name in action. */
  def actorPathFilter(level: Level, path: String): Boolean

  /* Default setting for debug logs that are not part of a group. */
  type GroupDebugDefault <: Boolean

  /* Default setting for trace logs that are not part of a group. */
  type GroupTraceDefault <: Boolean

  /* Method that returns which debug/trace groups may be shown. */
  transparent inline def showGroups: ShowGroups[?]

  protected def entry(feed: Boolean, level: Level, actorFilter: ActorFilter, sourceKind: Kind, sourcePath: String, message: => String): Option[Entry]

  // TODO: why can't i refactor this using inline val fixLevel = constValue[Ordinal[FixLevel]]
  // here with fixLevel as substitute for the expression on the method feed?? This leads to an
  // inlining error

  /**
   * General method for feeding the logger with log statements. Due to inlining it is completely
   * stripped down to the statements that are relevant at the logging level of execution. */
  // TODO: why is it not possible to remove the [actors] from private[actors] here?? The method is
  // completely private, but it leads to an inlining error.
  inline private[actors] def feed(inline level: Level, inline kind: Kind, inline path: String, inline message: String): Unit =
    /* See if the current fixed level surpassed the level of this entry, if not, we are done. If this
     * method is called from the fixed level methods or with a compile time constant in in the variable
     * log methods code will be eliminated when not reachable. We do not use 'inline if' here because
     * we want to allow for variable level use as well. Inline should work automagically is possible */
    inline if level.ordinal <= constValue[Ordinal[FixPassLevel]] then
      /* If we are dealing with a Fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      inline if level.ordinal == Level.Fatal.ordinal then handleFatal(message)
      if sourcePathFilter(level,path) then
        /* Now, construct and feed if needed the log entry directly to the process handler or to the log queue */
        inline if directSpool
          then entry(false,level,actorPathFilter,kind,path,message).foreach(process)
          else entry(true,level,actorPathFilter,kind,path,message).foreach(trySpool)

  /** This method is called for every log entry when the entries are spooled. */
  def process(entry: Entry): Unit

  /** Test if we have enough new logs for spooling */
  private[actors] def trySpool(entry: Entry): Unit

  /** Implement a handler for the event a fatal situation occurs */
  def handleFatal(message: String): Unit

  /* Discussion: Logging from the within the system is kind of hard, because we usually do not
   * have the logger object at hand. We could extract it from the guard services, or make a
   * special field for it. Or we make all internal logs available via a generic method call
   * that the user may implement and couple to his/her logger. A bit like the traceln() call
   * in the context (which does not belong there anyway). Actually i think this is the best
   * option right now. The user will couple the logs he want to see to system (remove the
   * private[actors] modifier. If he does not want to see them => Ignore them. */

  /**
   * Make lazy (delayed) log entry with level System (see ActorLogger.Level for documentation on the level).
   * For use with logs from Leucine itself. Cannot be suppressed by setting a level value. */
  private[actors] def system(message: => String): Unit =
    if directSpool
    then entry(false,Level.System,allPass,Static.Unknown,"",message).foreach(process)
    else entry(true,Level.System,allPass,Static.Unknown,"",message).foreach(trySpool)

  /**
   * Make lazy (delayed) log entry with level Fatal (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when FixLevel is set to System. */
  inline def fatal(message: => String): Unit =
    feed(Level.Fatal,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Error (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def error(message: => String): Unit =
    feed(Level.Error,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Warn (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def warn(message: => String): Unit  =
    feed(Level.Warn,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Info (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def info(message: => String): Unit  =
    feed(Level.Info,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Info (see ActorLogger.Level for documentation on the level).
   * Use this call to log sensitive information like usernames and passwords. During testing, with
   * ShowConfidential set to true, the confidentialMessage is used for logging. At production, set
   * ShowConfidential to false. Now the publicMessage is passed to the logger. The reference to
   * confidentialMesssage is removed at compile time. The whole call is also eliminated from the code
   * when not needed on a best effort approach. */
  inline def info(confidentialMesssage: => String, publicMessage: => String): Unit  =
    inline if constValue[ShowConfidential]
    then feed(Level.Info,kindInfo,pathInfo(constValue[FullPath]),confidentialMesssage)
    else feed(Level.Info,kindInfo,pathInfo(constValue[FullPath]),publicMessage)

  /**
   * Make lazy (delayed) log entry with level Beta (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def beta(message: => String): Unit  =
    feed(Level.Beta,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Beta (see ActorLogger.Level for documentation on the level).
   * Use this call to log sensitive information like usernames and passwords. During testing, with
   * ShowConfidential set to true, the confidentialMessage is used for logging. At production, set
   * ShowConfidential to false. Now the publicMessage is passed to the logger. The reference to
   * confidentialMesssage is removed at compile time. The whole call is also eliminated from the code
   * when not needed on a best effort approach. */
  inline def beta(confidentialMesssage: => String, publicMessage: => String): Unit  =
    inline if constValue[ShowConfidential]
    then feed(Level.Beta,kindInfo,pathInfo(constValue[FullPath]),confidentialMesssage)
    else feed(Level.Beta,kindInfo,pathInfo(constValue[FullPath]),publicMessage)

  /**
   * Make lazy (delayed) log entry with level Debug (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def debug(message: => String): Unit = inline if constValue[GroupDebugDefault] then
    feed(Level.Debug,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Debug (see ActorLogger.Level for documentation on the level).
   * Log entry is only made when the group in the parameter is activated via showGroups.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def debug[Group <: GroupBase](group: Group, message: => String): Unit = inline if showGroups.contains(group) then
    feed(Level.Debug,kindInfo,pathInfo(constValue[FullPath]),message)

  /**
   * Make lazy (delayed) log entry with level Trace (see ActorLogger.Level for documentation on the level).
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace(): Unit = inline if constValue[GroupTraceDefault] then
    feed(Level.Trace,kindInfo,pathInfo(constValue[FullPath]),callInfo(constValue[FullPath],constValue[FullParameters]))

  /**
   * Make lazy (delayed) log entry with level Trace (see ActorLogger.Level for documentation on the level).
   * Override the global setting FullParameters with the parameter withParameters to investigate a special case.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace(withParameters: Boolean): Unit = inline if constValue[GroupTraceDefault] then
    feed(Level.Trace,kindInfo,pathInfo(constValue[FullPath]),callInfo(constValue[FullPath],withParameters))

  /**
   * Make lazy (delayed) log entry with level Trace (see ActorLogger.Level for documentation on the level).
   * Log entry is only made when the group in the parameter is activated via showGroups.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[Group <: GroupBase](group: Group): Unit = inline if showGroups.contains(group) then
    feed(Level.Trace,kindInfo,pathInfo(constValue[FullPath]),callInfo(constValue[FullPath],constValue[FullParameters]))

  /**
   * Make lazy (delayed) log entry with level Trace (see ActorLogger.Level for documentation on the level).
   * Log entry is only made when the group in the parameter is activated via showGroups.
   * Override the global setting FullParameters with the parameter withParameters to investigate a special case.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[Group <: GroupBase](group: Group, withParameters: Boolean): Unit = inline if showGroups.contains(group) then
    feed(Level.Trace,kindInfo,pathInfo(constValue[FullPath]),callInfo(constValue[FullPath],withParameters))
