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


/** Methods stub for when there is no logging mixin used. */
private[actors] trait LogDefs extends BareDefs :
  private[actors] def logInit(): Unit = ()
  private[actors] def logExit(): Unit = ()


/**
 * Mixin which enables fast local logging in the actor. The logging statements are collected and
 * combined at times which minimize delays the concurrent handling of actors. If you do not mixin
 * this trait, logging inside the actor and called instances and methods still work, but each log
 * call synchronizes separately. And you are unaware in which actor the log call was executed.  */
trait LogAid extends ActorInit, ActorDefs :
  this: BareActor =>
  import ActorLogger.{Level, Timing}

  /* Here we get a copy of the level and timing as there are set in the current active logHolder.
   * These are extracted from the general logger and are not (should not) be changed over time.
   * We do not use val's here to keep the number of extra objects per actor small. */
  private def fixedLevel  = ActorGuard.logger.logHolder.level
  private def fixedTiming = ActorGuard.logger.logHolder.timing

  /* The fixed holder is what we should regularly use for collecting log statements in this actor.
   * Since this object is reused every time the actor is rescheduled on the thread, we keep it in
   * a val. It settings are copied once an do not change over the entire lifetime of the actor. */
  private val fixedHolder = LogHolder(path,fixedLevel,fixedTiming)

  /* Keeps the actor local logHolder ready for use. If we do not allow for local settings, the actor
   * log settings are the same as for logging outside the actors, and (thus) equal for all actors.
   * This is the default situation. If we do allow for local settings every actor that makes use of
   * logging will construct a new logHolder on every instant the actor is rescheduled for execution. */
  private def logHolder: LogHolder =
    /* See if we allow for local setting of the log level of timing ... */
    if ActorGuard.logger.localSettings
    /* ... if so, construct a new logHOlder with the currently active settings */
    then LogHolder(path,logLevel,logTiming)
    /* ... if not, we simply reuse the available one. */
    else fixedHolder

  /**
   * Override the logLevel with a new level for logging for actor local investigation. Note this
   * value is ignored if local settings are not allowed (to be set in the Main Logger Object) */
  protected def logLevel: Level = fixedLevel

  /**
   * Override the logTiming with a new timing for logging for actor local investigation. Note this
   * value is ignored if local settings are not allowed (to be set in the Main Logger Object) */
  protected def logTiming: Timing = fixedTiming

  /* Method to be called just before are the actor is scheduled on a new thread for execution, but
   * before its message queue is processed. It installs a new logHolder that collects all log entries. */
  private[actors] override def logInit(): Unit = LogLocal.fill(logHolder)

  /* Method to be called directly after the message queue is emptied. All acquired log entries will be
   * moved to a central location and the logHolder is emptied and removed from the thread. */
  private[actors] override def logExit(): Unit = LogLocal.empty()

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()

