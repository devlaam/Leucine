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
trait LogAid(logger: ActorLogger) extends ActorInit, ActorDefs :
  this: BareActor =>
  import ActorLogger.{Level, Timing}

  /* Local copies of the passLevel and timing from the global values. Although you may change the
   * global values from the outside, this has no effect once the actor is created. If the values
   * are changed with logSettings this has effect directly. */
  private var _level: Level    = logger.passLevel
  private var _timing: Timing  = logger.timing

  /* Local functions to produce the values of passLevel and timing inside the local instance of LogHolder. */
  private def level(): Level   = _level
  private def timing(): Timing = _timing

  /* The holder is what we use for collecting log statements in this actor. This object is loaded
   * every time the actor is rescheduled on the thread. It has internal variables that we update
   * via functions directly for efficiency reasons.  */
  private val holder = LogHolder(path,level,logger.incidentLevel,timing)

  /**
   * With logSettings you can update the logging pass level and the timing of the logger. It only works if
   * setting these variables is globally allowed for. These settings are effective immediately and stretch
   * to any code that is executed from this actor. However, only call this method from the constructor or
   * from within the message handler of the actor. Do NOT call it from outside of the actor or from a future.
   * If an argument is missing the value of the parameter is unchanged. */
  protected def logSettings(level: Level = _level, timing: Timing = _timing): Unit =
    /* See if we are allowed to update the current values. */
    if logger.localSettings then
      /* If so, replace the values. */
      _level  = level
      _timing = timing

  /**
   * Method to be called just before are the actor is scheduled on a new thread for execution, but
   * before its message queue is processed. It installs a new logHolder that collects all log entries. */
  private[actors] override def logInit(): Unit = logger.local.fill(holder)

  /**
   * Method to be called directly after the message queue is emptied. All acquired log entries will be
   * moved to a central location and the logHolder is emptied and removed from the thread. */
  private[actors] override def logExit(): Unit = logger.local.empty()

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeLogs(): Option[MonitorAid.Logs] = Some(MonitorAid.Logs(holder.getIncidents))


  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()

