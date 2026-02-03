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
 * call synchronizes separately (expensive!), you are unaware in which actor the log call was
 * executed and incidents are not reported on a per-actor basis.  */
trait LogAid extends ActorInit, ActorDefs :
  this: BareActor =>
  import ActorLogger.{Level, Timing}

  /** Get the current active logger if defined. */
  private def activeLogger: Option[ActorLogger] = ActorGuard.logger

  /* Here we get a copy of the level and timing as there are set in the current active logHolder.
   * These are extracted from the general logger and are not (should not) be changed over time.
   * We do not use val's here to keep the number of extra objects per actor small. The fallback's
   * are irrelevant since they will never be used if there is no logger active. */
  private def fixedPassLevel: Level     = activeLogger.map(_.logHolder.passLevel).getOrElse(Level.System)
  private def fixedTiming: Timing       = activeLogger.map(_.logHolder.timing).getOrElse(Timing.Recent)
  private def fixedIncidentLevel: Level = activeLogger.map(_.logHolder.incidentLevel).getOrElse(Level.System)

  /* The holder is what we use for collecting log statements in this actor. Since this object is reused every
   * time the actor is rescheduled on the thread, we try to reuse it as much as possible. Its initial settings
   * are copied from the fixed settings. */
  private var holder = LogHolder(path,fixedPassLevel,fixedIncidentLevel,fixedTiming)

  /**
   * Keeps the actor local logHolder ready for use. If we do not allow for local settings, the actor
   * log settings are the same as for logging outside the actors, and (thus) equal for all actors. This
   * is the default situation. If we do allow for local settings every actor that makes use of logging
   * will construct a new logHolder if the settings have changed. Usually that is not very often. */
  private def logHolder: LogHolder =
    /* See if we allow for local settings, if not we can reuse the initial holder (which is current) */
    if !activeLogger.map(_.localSettings).getOrElse(true) then holder
    /* See if any of the settings have changed, if not we can reuse the current holder */
    else if holder.alike(logPassLevel,logIncidentLevel,logTiming) then holder
    /* In all other situations we must construct a new holder, keep its internals and replace and return.
     * Since this method is always called in the thread where the message queue is processed and that is
     * never done in parallel with other actions on this actor, there is no need to synchronize. */
    else { holder = holder.inherit(logPassLevel,logIncidentLevel,logTiming); holder }

  /**
   * Override the global passLevel with a new level for logging for actor local investigation. Note this
   * value is ignored if local settings are not allowed (to be set in the Main Logger Object). If changes
   * are made after actor construction, the will be effective eventually. */
  protected def logPassLevel: Level = fixedPassLevel

  /**
   * Override the global incidentLevel with a new level for logging for actor local investigation. Note this
   * value is ignored if local settings are not allowed (to be set in the Main Logger Object). If changes
   * are made after actor construction, the will be effective eventually. */
  protected def logIncidentLevel: Level = fixedIncidentLevel

  /**
   * Override the logTiming with a new timing for logging for actor local investigation. Note this
   * value is ignored if local settings are not allowed (to be set in the Main Logger Object). If changes
   * are made after actor construction, the will be effective eventually. */
  protected def logTiming: Timing = fixedTiming

  /* Method to be called just before are the actor is scheduled on a new thread for execution, but
   * before its message queue is processed. It installs a new logHolder that collects all log entries. */
  private[actors] override def logInit(): Unit = if activeLogger.isDefined then LogLocal.fill(logHolder)

  /* Method to be called directly after the message queue is emptied. All acquired log entries will be
   * moved to a central location and the logHolder is emptied and removed from the thread. */
  private[actors] override def logExit(): Unit = if activeLogger.isDefined then LogLocal.empty()

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeLogs(): Option[MonitorAid.Logs] = Some(MonitorAid.Logs(holder.getIncidents))


  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()

