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

import java.util.concurrent.Callable


/* Methods stub for when there is no monitor mixin used. */
private[actors] trait MonitorDefs  extends BareDefs:
  private[actors] def probeBare(): Option[MonitorAid.Bare]  = None
  private[actors] def probeStash(): Option[MonitorAid.Stash]  = None
  private[actors] def probeTiming(): Option[MonitorAid.Timing]  = None
  private[actors] def probeFamily(): Option[MonitorAid.Family]  = None
  private[actors] def monitorSend[Sender >: Common <: Accept](mail: Actor.Mail, envelope: Env[Sender]): Unit = ()
  private[actors] def monitorEnter[Sender >: Common <: Accept](envelope: Env[Sender]): Unit = ()
  private[actors] def monitorExit[Sender >: Common <: Accept](envelope: Env[Sender]): Unit = ()
  private[actors] def monitorStart(): Unit = ()
  private[actors] def monitorStop(): Unit = ()
  protected def processLoad: Double = 0


/** Extend your actor with this mixin to put it under monitoring */
trait MonitorAid(monitor: ActorMonitor)(using context: ActorContext) extends ActorInit, ActorDefs :
  this: NameActor =>
  import MonitorAid.{Action, Trace, Tracing}

  /* Fields to measure the time spend inside the thread and outside the thread */
  private var lastClockTime: Long = 0
  private var threadStartMoment: Long = 0
  private var threadPlayTime: Long = 0
  private var threadPauseTime: Long = 0

  /* Temporary fast storage for the trace objects. */
  private var traces: List[Trace] = Nil

  /* Variable the keeps the state of taking probes in the actor. If true new probes are
   * scheduled on regular intervals. If false, new probes are not scheduled any more. */
  private var probing: Boolean = false

  /**
   * This is the personal setting of tracing. There is a public setting as well.
   * If tracing is active for this actor depends on both settings, in a symmetric manner.
   * There are two levels of tracing. TraceFull and TraceCount. TraceFull traces every
   * message with timestamp and action. This may be memory intensive. TraceCount just
   * counts the messages from sender to receiver per letter. Usually this number is limited.
   * However, both forms should only be used at debugging, since it requires a lot of
   * synchronized operations.
   * If both are Enabled or one is Enabled and the other is Default, the TraceFull is
   * active. In all other cases it is not. TraceCount is active when both are at Default
   * or one of the settings is Enabled. This implies that you can enable/disable
   * the tracing TraceFull here as long as the global tracing is Default or Enabled.
   * Setting this to Disabled will always prohibit tracing TraceFull of this actor,
   * and setting it to Default/Enabled leaves the fate in the hands of the global setting. */
  protected def tracing: Tracing = Tracing.Default

  /**
   * See if we may trace this actor in TraceAll mode now. This is the case if one of the
   * tracing settings is Enabled and the other Default or Enabled. */
  private def mayTraceAll: Boolean = tracing.ordinal + monitor.tracing.ordinal > 2

  /**
   * See if we may trace this actor in TraceCount mode now. This is the case if one of the
   * tracing settings is Enabled or both are Default. */
  private def mayTraceCount: Boolean = tracing.ordinal + monitor.tracing.ordinal > 1

  /** Method called from the actor store its activity */
  private def addTrace(trace: Trace): Unit = synchronized { traces = trace :: traces }

  /** Calculate the gain in time since the last visit. */
  private def gain(newClockTime: Long): Long =
    /* Calculate the increase in time. This should be positive of course, but there never
     * is an absolute guarantee of the presence of a monotonic clock on every OS. So better
     * be inaccurate as negative. */
    val result        = math.abs(newClockTime - lastClockTime)
    lastClockTime     = newClockTime
    result

  /** Variable holding the cancel object for probing the actor. */
  private var cancelProbe = Cancellable.empty

  /** Method to create a new schedulable object to probe the actor. */
  private def callProbe = new Callable[Unit] {
    /* Try to make a probe and if successful, schedule a new one. */
    def call(): Unit = if probeNow() then probeRenew() }

  /** Start the probe actions */
  private def probeStart() = synchronized {
    /* The probing flag must be set to continue scheduling late probes. But we cannot take probes
     * right away, since the actor may still be in the construction phase (deadlock!). But it will
     * make no sense either, since there is nothing going on at the start. */
    probing = true
    /* Start the probe interval timer.  */
    probeRenew() }

  /** Call this to restart the probing, if we are still probing. */
  private def probeRenew(): Unit =
    /* Schedule the probing to take place over a fixed amount of time. */
    context.schedule(callProbe,monitor.probeInterval)

  /** Method to do the actual probing of the actor. Returns if probes were made. */
  private def probeNow(): Boolean =
    /* Take the samples in a synchronized way. */
    val (samples,traces,probed) = synchronized {
      /* Do the actual probing work on all mixins and the bare actor trait. */
      val samples = if probing then List(probeBare(),probeStash(),probeTiming(),probeFamily()).flatten else Nil
      /* Get the traces gathered from this actor */
      val traces = if probing then this.traces else Nil
      /* Clean the trace storage */
      this.traces = Nil
      /* Communicate the result outside the synchronized environment */
      (samples,traces,probing) }
    /* If probes were made, store them. */
    if probed then
      /* Store the collected samples in the monitor storage */
      monitor.setSamples(storePath,samples)
      /* Store the posts in the monitor storage */
      if mayTraceCount then monitor.setPosts(path,traces)
      /* Store the traces in the monitor storage */
      if mayTraceAll then monitor.setTraces(path,traces)
    /* Return the fact if we made any probes.*/
    probed

  /** Cancel the delayed probe actions, but also perform one last probe action right now. */
  private def probeStop() =
    /* Canceling the scheduled next probe is on best effort basis. It cannot be guaranteed,
     * the event will come never the less. So we must be ready for that. */
    cancelProbe.cancel()
    /* Make the last probe manually */
    probeNow()
    /* Setting this to false prohibits the further scheduled executions of probes. */
    synchronized { probing = false }

  /* For workers we do not want to use the full name, but a one name for all workers in this family. So
   * we only use the path up to and including the worker prefix. The rest is dropped. */
  /** path under which we store the data for this actor. Its the full family name or a shorten version in case of a worker. */
  private val storePath = if isWorker then path.substring(0,path.length()-name.length()+context.workerPrefix.length()) else path

  /**
   * Method called from the actor to indicate that operations have commenced. */
  private[actors] override def monitorStart(): Unit =
    /* Actions to enable time measurement of this actor. */
    val time = System.nanoTime
    threadStartMoment = gain(time)
    /* Register this actor in the monitor as existing. */
    monitor.addActor(storePath)
    /* Trace if requested, also trace the actor as created.  */
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Created,Actor.Post(path)))
    /* Make the first probe of the actor and continue to do so on regular intervals. */
    probeStart()

  /** Method called from the actor to indicate that it receives a new letter */
  private[actors] override def monitorSend[Sender >: Common <: Accept](mail: Actor.Mail, envelope: Env[Sender]): Unit =
    /* Trace if requested, the letter is received or rejected. */
    if mayTraceCount then addTrace(Trace(System.nanoTime-monitor.baseline,Action(mail),Actor.Post(mail,path,envelope)))

  /** Method called from the actor to indicate that it starts processing a letter. */
  private[actors] override def monitorEnter[Sender >: Common <: Accept](envelope: Env[Sender]): Unit =
    val time = System.nanoTime
    /* Trace if requested, the letter processing is initiated */
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Initiated,Actor.Post(Actor.Mail.Received,path,envelope)))
    /* The time past since has to be added to the total time in pause. */
    threadPauseTime += gain(time)

  /** Method called from the actor to indicate that it finished processing a letter. */
  private[actors] override def monitorExit[Sender >: Common <: Accept](envelope: Env[Sender]): Unit  =
    val time = System.nanoTime
    /* The time past since has to be added to the total time in play. */
    threadPlayTime += gain(time)
    /* Trace if requested, the letter processing is completed */
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Completed,Actor.Post(Actor.Mail.Processed,path,envelope)))

  /** Method called from the actor to indicate that we are done in this actor. */
  private[actors] override def monitorStop(): Unit  =
    val time = System.nanoTime
    /* The time past since has to be added to the total time in pause, since we can
     * never directly stop inside a letter. */
    threadPauseTime += gain(time)
    /* Tell the monitor this actor is done. Depending on the path it may just ignore
     * it or take cleaning up actions. */
    monitor.delActor(storePath)
    /* Probing makes no sense any more. Determine the last samples and stop scheduled probe actions. */
    probeStop()
    /* Trace if requested, the actor is terminated */
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Terminated,Actor.Post(path)))

  /** Calculate the relative time this actor spend performing processing letters. */
  protected override def processLoad: Double =
    /* This is an calculation based on non synchronized values, but if we call this inside
     * the letter handling we are fine or in callbacks we are fine since they are guaranteed
     * to be sequential. */
    val threadRunTime = threadPlayTime + threadPauseTime
    /* Calculate the relative time if the thread has run at all. */
    if threadRunTime == 0 then 0D else threadPlayTime.toDouble/threadRunTime.toDouble


  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()



object MonitorAid :

  /* All trace entries are counted, so you can see none is failing. */
  private var _tracer: Int = 0
  private def tracer: Int = synchronized { _tracer += 1; _tracer }

  enum Action :
    /* The actor is created */
    case Created
    /* The actor is terminated */
    case Terminated
    /* The message is accepted for delivery */
    case Accepted
    /* The message is refused */
    case Refused
    /* Letter processing has begone */
    case Initiated
    /* Letter processing is done */
    case Completed

  object Action :
    def apply(mail: Actor.Mail): Action = if mail < Actor.Mail.Received then Refused else Accepted


  /**
   * This is for the  personal or public setting of tracing. If tracing is active for this actor
   * depends on both settings, in a symmetric manner. See the documentation at their definition
   * locations.  */
  enum Tracing :
    /* Full tracing is disabled, Count Tracing depends on other setting. */
    case Disabled
    /* Full / Count Tracing depends on other setting */
    case Default
    /* Count Tracing is enabled, Full Tracing depends on other setting. */
    case Enabled


  /** Class to capture trace information. */
  class Trace(val time: Long, val action: Action, val post: Actor.Post) extends Ordered[Trace] :
    import Action.*
    var nr = tracer
    /* Sorting is first on action time and subsequently on trace number. This should be sufficient to be unique in
     * all circumstances. */
    def compare(that: Trace): Int =
      if      this.time < that.time then -1
      else if this.time > that.time then  1
      else if this.nr   < that.nr   then -1
      else if this.nr   > that.nr   then  1
      else if this.##   < that.##   then -1
      else if this.##   > that.##   then  1
      else                                0
    def show = action match
      case Created | Terminated                       => s"time=$time, nr=$nr, action=$action, actor=${post.receiver}"
      case Accepted | Refused | Initiated | Completed => s"time=$time, nr=$nr, action=$action, ${post.full}"

  object Trace :
    def empty(time: Long) = new Trace(time,Action.Created,Actor.Post(""))

  /** Class to return the results on a monitor probe. */
  sealed trait Sample :
    def show: String

  /** Class to marshal all the KPI's of the bare actor. */
  case class Bare(phase: BareActor.Phase, stop: Actor.Stop, lettersSum: Int, lettersMax: Int, exceptionsSum: Int, needles: Int, processLoad: Double) extends Sample :
    def userPPM = (processLoad * 1000000).toInt
    def show = s"phase=$phase, stop=$stop, lettersSum=$lettersSum, lettersMax=$lettersMax, exceptionsSum=$exceptionsSum, needles=$needles, processLoad=${userPPM}ppm"

  /** Class to marshal all the KPI's of the Stash mixin. */
  case class Stash(lettersSum: Int, lettersMax: Int) extends Sample :
    def show = s"stashSum=$lettersSum, stashMax=$lettersMax"

  /** Class to marshal all the KPI's of the Timing mixin. */
  case class Timing(lettersSum: Int, lettersMax: Int, anchorsSize: Int) extends Sample :
    def show = s"timersSum=$lettersSum, timersMax=$lettersMax, timersNow=$anchorsSize"

  /** Class to marshal all the KPI's of the Families mixins. */
  case class Family(namedChildrenNow: Int, allChildrenNow: Int, workersSum: Long) extends Sample :
    def show = s"namedChildrenNow=$namedChildrenNow, allChildrenNow=$allChildrenNow, workersSum=$workersSum"
