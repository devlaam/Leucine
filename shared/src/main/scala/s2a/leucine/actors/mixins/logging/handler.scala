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
  import ActorLogger.{Entry, Level, Channel, Filter, Capture, Slow}
  import Static.{kindInfo, pathInfo, callInfo}

  /**
   * Make a new log entry, returns the constructed entry for further processing if succeeded.
   * With feed true, the entry will be directly stored otherwise only constructed. */
  protected def entry(feed: Boolean, capture: Capture): Option[Entry]

  /** Router for the log entries. */
  private[actors] def preprocess(entry: Entry): Unit

  /**
   * General method for feeding the logger with log statements. Due to inlining it is completely
   * stripped down to the statements that are relevant at the logging level of execution. */
  inline private[actors] def feed(capture: Capture): Unit =
    if capture.passSource then entry(!spooling.direct,capture).foreach(preprocess)

  /* See if the current fixed level surpassed the level of this entry, if not, we are done. Eliminates
   * subsequent code if the test is not fulfilled. */
  inline private[actors] def pass(inline level: Level) = level.ordinal <= fixPassLevel.ordinal

  /* See if the current fixed level surpassed the level of this entry and if we may publish in this
   * channel, if not, we are done. Eliminates subsequent code if the test is not fulfilled.  */
  inline private[actors] def pass(inline level: Level, inline channel: Channel) =
    level.ordinal <= fixPassLevel.ordinal && showChannels.contains(channel)


  /** Get the  appropriate level dependent info for system logging events for the source path. */
  inline private def getInfo(inline level: Level): String =
    /* Since the level required to be constant we can check compile time which code to include. This is
     * nice, since we do not have the fullPath and fullParameters values available at compile time for
     * system logs. There is no way to generate a stable type path in case we do not know what the top
     * level logger object, defined by the application builder, is. The circumvent this problem we generate
     * all possible required data at compile time. Its not nice, but it does not make the system slower,
     * just a little more bulky. */
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
   * information. The logic test to see if logging is required is fast. The call publishes the message in
   * in the channel Pass, SysPrd or SysDvl depending on the level. */
  private[actors] inline def syslog(inline level: Level, inline message: String): Unit =
    if level <= fixPassLevel then
      inline if level.ordinal == Level.Fatal.ordinal then sysFatal(message)
      if level.ordinal == Level.Fatal.ordinal ||
         showChannels.hasSysPrd && level.ordinal <= Level.Info.ordinal ||
         showChannels.hasSysDvl && level.ordinal >= Level.Beta.ordinal then
        /* Due to issue https://github.com/scala/scala3/issues/25350 we must provide the full path to fixPass locally. */
        entry(!spooling.direct,Capture(level,getChannel(level),Filter.Pass,kindInfo,getInfo(level),message)).foreach(preprocess)

  /**
   * Make log entry with level Fatal, indicates that further processing is unreliable and shutdown is imminent.
   * This call is only eliminated from the code when fixLevel is set to Ignore. The call publishes the message
   * in the channel Pass, so is cannot be stopped with ShowChannels(()). Make sure your message does not require
   * lengthy calculations, preferably use a short fixed string. */
  inline def fatal(message: String | Slow): Unit =
    inline if pass(Level.Fatal) then
      /* For this fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      appFatal(message.toString)
      feed(Capture(Level.Fatal,Channel.Pass,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Fatal, indicates that further processing is unreliable and shutdown is imminent,
   * with the ability to pass a throwable. Leucine just passes the the throwable and does not interact with it.
   * This call is only eliminated from the code when fixLevel is set to Ignore. The call publishes the message
   * in the channel Pass, so is cannot be stopped with ShowChannels(()). */
  inline def fatal(message: String | Slow, throwable: Throwable): Unit =
    inline if pass(Level.Fatal) then
      /* For this fatal event, extra steps may be needed, for the system may crash before
       * the log queue is flushed. First report that this happened. */
      appFatal(message.toString)
      feed(Capture(Level.Fatal,Channel.Pass,filter,kindInfo,pathInfo(fullPath),message,Some(throwable)))

  /**
   * Make log entry with level Error, indicating severe disturbances in process handling, but system
   * can continue with other tasks. This call is eliminated from the code when not needed on a
   * best effort approach. The call publishes the message in the channel AppPrd. */
  inline def error(message: String | Slow): Unit =
    inline if pass(Level.Error,Channel.AppPrd) then
      feed(Capture(Level.Error,Channel.AppPrd,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Error, indicating severe disturbances in process handling, but system can continue
   * with other tasks. This call allows to pass a throwable. Leucine just passes the the throwable and does not
   * interact with it. The call is eliminated from the code when not needed on a best effort approach and publishes
   * the message in the channel AppPrd. */
  inline def error(message: String | Slow, throwable: Throwable): Unit =
    inline if pass(Level.Error,Channel.AppPrd) then
      feed(Capture(Level.Error,Channel.AppPrd,filter,kindInfo,pathInfo(fullPath),message,Some(throwable)))

  /**
   * Make log entry with level Warn, indicating that something is out of the ordinary, but processing
   * can continue. This call is eliminated from the code when not needed on a best effort approach. The
   * call publishes the message in the channel AppPrd. */
  inline def warn(message: String | Slow): Unit  =
    inline if pass(Level.Warn,Channel.AppPrd) then
      feed(Capture(Level.Warn,Channel.AppPrd,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Info, to keep the user informed about the systems whereabouts. This call is
   * eliminated from the code when not needed on a best effort approach. The call publishes the message in
   * the channel AppPrd. */
  inline def info(message: String | Slow): Unit  =
    inline if pass(Level.Info,Channel.AppPrd) then
      feed(Capture(Level.Info,Channel.AppPrd,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Info, to keep the user informed about the systems whereabouts.
   * Use this call to log sensitive information like usernames and passwords. During testing, with
   * showConfidential set to true, the confidentialMessage is used for logging. At production, set
   * showConfidential to false. Now the publicMessage is passed to the logger. The reference to
   * confidentialMesssage is removed at compile time. The whole call is also eliminated from the code
   * when not needed on a best effort approach. The call publishes the message in the channel AppPrd. */
  inline def info(confidentialMesssage: String | Slow, publicMessage: String | Slow): Unit  =
    inline if pass(Level.Info,Channel.AppPrd) then
      inline if showConfidential
      then feed(Capture(Level.Info,Channel.AppPrd,filter,kindInfo,pathInfo(fullPath),confidentialMesssage))
      else feed(Capture(Level.Info,Channel.AppPrd,filter,kindInfo,pathInfo(fullPath),publicMessage))

  /**
   * Make log entry with level Beta, to keep the developer informed about the systems whereabouts.
   * Promote debug messages that are important during beta testing to the level Beta. That way they
   * can easily be eliminated by setting the fixPassLevel/passLevel to Info for production code, but
   * still been seen during Beta testing, without the clutter of all the other debug logging. This call
   * is eliminated from the code when not needed on a best effort approach. The call publishes the message
   * in the channel AppDvl. */
  inline def beta(message: String | Slow): Unit  =
    inline if pass(Level.Beta,Channel.AppDvl) then
      feed(Capture(Level.Beta,Channel.AppDvl,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Beta, to keep the developer informed about the systems whereabouts.
   * Promote debug messages that are important during beta testing to the level Beta. That way they
   * can easily be eliminated by setting the FixPassLevel/passLevel to Info for production code, but
   * still been seen during Beta testing, without the clutter of all the other debug logging.
   * Use this call to log sensitive information like usernames and passwords. During testing, with
   * ShowConfidential set to true, the confidentialMessage is used for logging. At production, set
   * ShowConfidential to false. Now the publicMessage is passed to the logger. The reference to
   * confidentialMesssage is removed at compile time. The whole call is also eliminated from the code
   * when not needed on a best effort approach. The call publishes the message in the channel AppDvl. */
  inline def beta(confidentialMesssage: String | Slow, publicMessage: String | Slow): Unit  =
    inline if pass(Level.Beta,Channel.AppDvl) then
      inline if showConfidential
      then feed(Capture(Level.Beta,Channel.AppDvl,filter,kindInfo,pathInfo(fullPath),confidentialMesssage))
      else feed(Capture(Level.Beta,Channel.AppDvl,filter,kindInfo,pathInfo(fullPath),publicMessage))

  /**
   * Make log entry with level Debug, to communicate internals of the system for diagnostic purposes.
   * This call is eliminated from the code when not needed on a best effort approach. The call publishes
   * the message in the channel AppDvl. */
  inline def debug(message: String | Slow): Unit =
    inline if pass(Level.Beta,Channel.AppDvl) then
      feed(Capture(Level.Debug,Channel.AppDvl,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Debug, to communicate internals of the system for diagnostic purposes.
   * Log entry is only made when the channel in the parameter is activated via showChannels.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def debug[CH <: Channel](channel: CH, message: String | Slow): Unit =
    inline if pass(Level.Debug,channel) then
      feed(Capture(Level.Debug,channel,filter,kindInfo,pathInfo(fullPath),message))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if fullParameters
   * is set to true. You typically put this call as first statement after a class, object or method
   * definition. This call is eliminated from the code when not needed on a best effort approach.
   * The call publishes the message in the channel AppDvl. */
  inline def trace(): Unit =
    inline if pass(Level.Trace,Channel.AppDvl) then
      feed(Capture(Level.Trace,Channel.AppDvl,filter,kindInfo,callInfo(fullPath,fullParameters),""))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if fullParameters
   * is set to true. You typically put this call as first statement after a class, object or method
   * definition. With the possibility to enter extra information in the form of a message. This call is
   * eliminated from the code when not needed on a best effort approach.  The call publishes the message
   * in the channel AppDvl. */
  inline def trace(message: String | Slow): Unit =
    inline if pass(Level.Trace,Channel.AppDvl) then
      feed(Capture(Level.Trace,Channel.AppDvl,filter,kindInfo,callInfo(fullPath,fullParameters),message))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if withParameters
   * is set to true, this overriding the global setting fullParameters, to investigate a special case.
   * You typically put this call as  first statement after a class, object or method definition. This call is eliminated from the code when not needed on a best
   * effort approach.  The call publishes the message in the channel AppDvl. */
  inline def trace(withParameters: Boolean): Unit =
    inline if pass(Level.Trace,Channel.AppDvl) then
      feed(Capture(Level.Trace,Channel.AppDvl,filter,kindInfo,callInfo(fullPath,withParameters),""))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if withParameters
   * is set to true, this overriding the global setting fullParameters, to investigate a special case.
   * You typically put this call as  first statement after a class, object or method definition.
   * With the possibility to enter extra information in the form of a message. This call is eliminated
   * from the code when not needed on a best effort approach.  The call publishes the message in the
   * channel AppDvl. */
  inline def trace(withParameters: Boolean, message: String | Slow): Unit =
    inline if pass(Level.Trace,Channel.AppDvl) then
      feed(Capture(Level.Trace,Channel.AppDvl,filter,kindInfo,callInfo(fullPath,withParameters),message))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if fullParameters
   * is set to true. You typically put this call as first statement after a class, object or method
   * definition. Log entry is only made when the channel in the parameter is activated via showChannels.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH): Unit =
    inline if pass(Level.Trace,channel) then
      feed(Capture(Level.Trace,channel,filter,kindInfo,callInfo(fullPath,fullParameters),""))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if fullParameters
   * is set to true. You typically put this call as first statement after a class, object or method
   * definition. Log entry is only made when the channel in the parameter is activated via showChannels.
   * With the possibility to enter extra information in the form of a message. This call is eliminated
   * from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH, message: String | Slow): Unit =
    inline if pass(Level.Trace,channel) then
      feed(Capture(Level.Trace,channel,filter,kindInfo,callInfo(fullPath,fullParameters),message))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if withParameters
   * is set to true, this overriding the global setting fullParameters, to investigate a special case.
   * You typically put this call as first statement after a class, object or method definition.
   * Log entry is only made when the channel in the parameter is activated via showChannels.
   * This call is eliminated from the code when not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH, withParameters: Boolean): Unit =
    inline if pass(Level.Trace,channel) then
      feed(Capture(Level.Trace,channel,filter,kindInfo,callInfo(fullPath,withParameters),""))

  /**
   * Make log entry with level Trace, to follow the flow of the code in detail for diagnostic purposes.
   * Call extracts the Object/Class/Method path, and full parameters with arguments if withParameters
   * is set to true, this overriding the global setting fullParameters, to investigate a special case.
   * You typically put this call as first statement after a class, object or method definition. Log entry
   * is only made when the channel in the parameter is activated via showChannels. With the possibility
   * to enter extra information in the form of a message. This call is eliminated from the code when
   * not needed on a best effort approach. */
  inline def trace[CH <: Channel](channel: CH, withParameters: Boolean, message: String | Slow): Unit =
    inline if pass(Level.Trace,channel) then
      feed(Capture(Level.Trace,channel,filter,kindInfo,callInfo(fullPath,withParameters),message))
