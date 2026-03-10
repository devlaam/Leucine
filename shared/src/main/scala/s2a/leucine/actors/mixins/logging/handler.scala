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


/**
 * The LogHandler makes available all logging methods like log.warn(...) etc. The trait is used as
 * internal extension for the actor logger. Not for external use. */
private trait LogHandler extends LogHandlerConfig:
  import ActorLogger.{Entry, Level, Channel}
  import Static.{Kind, kindInfo, pathInfo, callInfo}
  import LogHolder.ActorFilter

  /**
   * Make a new log entry, returns the constructed entry for further processing if succeeded.
   * With feed true, the entry will be directly stored otherwise only constructed. */
  protected def entry(feed: Boolean, level: Level, channel: Channel, actorFilter: ActorFilter, sourceKind: Kind, sourcePath: String, message: => String): Option[Entry]

  /** Test if we have enough new logs for spooling */
  private[actors] def trySpool(entry: Entry): Unit

  /**
   * General method for feeding the logger with log statements. Due to inlining it is completely
   * stripped down to the statements that are relevant at the logging level of execution. */
  inline private[actors] def feed(inline level: Level, inline channel: Channel, inline kind: Kind, inline path: String, inline message: String): Unit =
    /* See if the current fixed level surpassed the level of this entry, if not, we are done. If this
     * method is called from the fixed level methods or with a compile time constant in in the variable
     * log methods code will be eliminated when not reachable. We do not use 'inline if' here because
     * we want to allow for variable level use as well. Inline should work automagically if possible */
    inline if level.ordinal <= fixPassLevel.ordinal then
      /* If we are dealing with a Fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      inline if level.ordinal == Level.Fatal.ordinal then appFatal(message)
      if sourcePathFilter(level,path) then
        /* Now, construct and feed if needed the log entry directly to the process handler or to the log queue */
        inline if directSpool
          then entry(false,level,channel,actorPathFilter,kind,path,message).foreach(process)
          else entry(true,level,channel,actorPathFilter,kind,path,message).foreach(trySpool)

  /** Get the  appropriate level dependent info for system logging events for the source path. */
  inline private def getInfo(inline level: Level): String =
    /* Since the level required to be constant we can check compile time which code to include. This is
     * nice, since we do not have the fullPath and fullParameters values available at compile time for
     * system logs. There is no way to generate a stable type path in case we do not know what the top
     * level logger object, defined by the application builder, is. The circumvent this problem we generate
     * all possible required data at compile time. Its not nice, but it does not make the system slower,
     * just more bulky. */
    inline if level.ordinal == Level.Trace.ordinal
    then
      /* For tracing we might need one of the following callInfo strings. */
      if fullPath
      then
        if fullParameters
        then callInfo(true,true)
        else callInfo(true,false)
      else
        if fullParameters
        then callInfo(false,true)
        else callInfo(false,false)
    else
      /* In all other situations, the pathInfo is sufficient. */
      if fullPath
      then pathInfo(true)
      else pathInfo(false)

  /** Get the Channel that belongs to this log level. For system logs, this coupling is fixed. */
  inline private def getChannel(inline level: Level) =
    if      level.ordinal > Level.Info.ordinal  then Channel.SysDvl
    else if level.ordinal > Level.Fatal.ordinal then Channel.SysPrd
    else                                             Channel.Pass

  /**
   * Make system log entry, use for messages from the actor framework itself. Although the call cannot be
   * removed from the code at compile time, it is still inlined to ensure it generates the correct context
   * information. The logic test to see if logging is required is fast. */
  private[actors] inline def syslog(inline level: Level, message: => String): Unit =
    if level <= fixPassLevel then
      inline if level.ordinal == Level.Fatal.ordinal then sysFatal(message)
      if level.ordinal == Level.Fatal.ordinal ||
         showChannels.hasSysPrd && level.ordinal <= Level.Info.ordinal ||
         showChannels.hasSysDvl && level.ordinal >= Level.Beta.ordinal then
        if directSpool
        /* Due to issue https://github.com/scala/scala3/issues/25350 we must provide the full path to fixPass locally. */
        then entry(false,level,getChannel(level),LogHolder.fixPass(true),kindInfo,getInfo(level),message).foreach(process)
        else entry(true,level,getChannel(level),LogHolder.fixPass(true),kindInfo,getInfo(level),message).foreach(trySpool)

  /**
   * Make log entry with level Fatal, indicates that further processing is unreliable and
   * shutdown is imminent. This call is eliminated from the code when FixLevel is set to System. */
  inline def fatal(message: => String): Unit =
    feed(Level.Fatal,Channel.Pass,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Error, indicate severe disturbances in process handling, but system
   * can continue with other tasks. This call is eliminated from the code when not needed on a
   * best effort approach. */
  inline def error(message: => String): Unit =
    inline if showChannels.contains(Channel.AppPrd) then
      feed(Level.Error,Channel.AppPrd,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Warn, indication that something is out of the ordinary, but processing
   * can continue. This call is eliminated from the code when not needed on a best effort approach. */
  inline def warn(message: => String): Unit  =
    inline if showChannels.contains(Channel.AppPrd) then
      feed(Level.Warn,Channel.AppPrd,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Info, to keep the user informed about the systems whereabouts.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def info(message: => String): Unit  =
    inline if showChannels.contains(Channel.AppPrd) then
      feed(Level.Info,Channel.AppPrd,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Info, to keep the user informed about the systems whereabouts.
   * Use this call to log sensitive information like usernames and passwords. During testing, with
   * ShowConfidential set to true, the confidentialMessage is used for logging. At production, set
   * ShowConfidential to false. Now the publicMessage is passed to the logger. The reference to
   * confidentialMesssage is removed at compile time. The whole call is also eliminated from the code
   * when not needed on a best effort approach. */
  inline def info(confidentialMesssage: => String, publicMessage: => String): Unit  =
    inline if showChannels.contains(Channel.AppPrd) then
      inline if showConfidential
      then feed(Level.Info,Channel.AppPrd,kindInfo,pathInfo(fullPath),confidentialMesssage)
      else feed(Level.Info,Channel.AppPrd,kindInfo,pathInfo(fullPath),publicMessage)

  /**
   * Make log entry with level Beta, to keep the developer informed about the systems whereabouts.
   * Promote debug messages that are important during beta testing to the level Beta. That way they
   * can easily be eliminated by setting the FixPassLevel/passLevel to Info for production code.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def beta(message: => String): Unit  =
    inline if showChannels.contains(Channel.AppDvl) then
      feed(Level.Beta,Channel.AppDvl,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Beta, to keep the developer informed about the systems whereabouts.
   * Promote debug messages that are important during beta testing to the level Beta. That way they
   * can easily be eliminated by setting the FixPassLevel/passLevel to Info for production code.
   * Use this call to log sensitive information like usernames and passwords. During testing, with
   * ShowConfidential set to true, the confidentialMessage is used for logging. At production, set
   * ShowConfidential to false. Now the publicMessage is passed to the logger. The reference to
   * confidentialMesssage is removed at compile time. The whole call is also eliminated from the code
   * when not needed on a best effort approach. */
  inline def beta(confidentialMesssage: => String, publicMessage: => String): Unit  =
    inline if showChannels.contains(Channel.AppDvl) then
      inline if showConfidential
      then feed(Level.Beta,Channel.AppDvl,kindInfo,pathInfo(fullPath),confidentialMesssage)
      else feed(Level.Beta,Channel.AppDvl,kindInfo,pathInfo(fullPath),publicMessage)

  /**
   * Make log entry with level Debug, to communicate internals of the system for diagnostic purposes.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def debug(message: => String): Unit =
    inline if showChannels.contains(Channel.AppDvl) then
      feed(Level.Debug,Channel.AppDvl,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Debug, to communicate internals of the system for diagnostic purposes.
   * Log entry is only made when the channel in the parameter is activated via showChannels.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def debug[CH <: Channel](channel: CH, message: => String): Unit =
    inline if showChannels.contains(channel) then
      feed(Level.Debug,channel,kindInfo,pathInfo(fullPath),message)

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace(): Unit =
    inline if showChannels.contains(Channel.AppDvl) then
      feed(Level.Trace,Channel.AppDvl,kindInfo,callInfo(fullPath,fullParameters),"")

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed.
   * With the possibility to enter extra information in the form of a message.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace(message: => String): Unit =
    inline if showChannels.contains(Channel.AppDvl) then
      feed(Level.Trace,Channel.AppDvl,kindInfo,callInfo(fullPath,fullParameters),message)

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed. Override
   * the global setting FullParameters with the parameter withParameters to investigate a special case.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace(withParameters: Boolean): Unit =
    inline if showChannels.contains(Channel.AppDvl) then
      feed(Level.Trace,Channel.AppDvl,kindInfo,callInfo(fullPath,withParameters),"")

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed. Override
   * the global setting FullParameters with the parameter withParameters to investigate a special case.
   * With the possibility to enter extra information in the form of a message.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace(withParameters: Boolean, message: => String): Unit =
    inline if showChannels.contains(Channel.AppDvl) then
      feed(Level.Trace,Channel.AppDvl,kindInfo,callInfo(fullPath,withParameters),message)

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed.
   * Log entry is only made when the channel in the parameter is activated via showChannels.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH): Unit =
    inline if showChannels.contains(channel) then
      feed(Level.Trace,channel,kindInfo,callInfo(fullPath,fullParameters),"")

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed.
   * Log entry is only made when the channel in the parameter is activated via showChannels.
   * With the possibility to enter extra information in the form of a message.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH, message: => String): Unit =
    inline if showChannels.contains(channel) then
      feed(Level.Trace,channel,kindInfo,callInfo(fullPath,fullParameters),message)

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed.
   * Log entry is only made when the channel in the parameter is activated via showChannels. Override the
   * global setting FullParameters with the parameter withParameters to investigate a special case.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH, withParameters: Boolean): Unit =
    inline if showChannels.contains(channel) then
      feed(Level.Trace,channel,kindInfo,callInfo(fullPath,withParameters),"")

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if needed.
   * Log entry is only made when the channel in the parameter is activated via showChannels. Override the
   * global setting FullParameters with the parameter withParameters to investigate a special case.
   * With the possibility to enter extra information in the form of a message.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH, withParameters: Boolean, message: => String ): Unit =
    inline if showChannels.contains(channel) then
      feed(Level.Trace,channel,kindInfo,callInfo(fullPath,withParameters),message)
