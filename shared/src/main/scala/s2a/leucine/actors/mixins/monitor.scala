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
private[actors] trait MonitorDefs :
  private[actors] type Env
  private[actors] def probeBare(): Option[MonitorActor.Bare]  = None
  private[actors] def probeStash(): Option[MonitorActor.Stash]  = None
  private[actors] def probeTiming(): Option[MonitorActor.Timing]  = None
  private[actors] def probeFamily(): Option[MonitorActor.Family]  = None
  private[actors] def monitorSend(isActive: Boolean, envelope: Env): Unit = ()
  private[actors] def monitorEnter(envelope: Env): Unit = ()
  private[actors] def monitorExit(envelope: Env): Unit = ()
  private[actors] def monitorStop(): Unit = ()
  private[actors] def userLoad: Double = 0

/** Extend your actor with this mixin to put it under monitoring */
trait MonitorActor(monitor: ActorMonitor)(using context: ActorContext) extends ActorDefs :
  import MonitorActor.{Action, Trace, Tracing}

  /** The prefix used in actornames for actors that are workers */
  protected def workerPrefix: String

  /** The character that will be used in the full name definitions of the actors.*/
  protected def familyPathSeparator: Char

  /* Fields to measure the time spend inside the thread and outside the thread */
  private var lastClockTime: Long = 0
  private var threadStartMoment: Long = 0
  private var threadPlayTime: Long = 0
  private var treadPauseTime: Long = 0

  /* Temporary fast storage for the trace objects. */
  private var traces: List[Trace] = Nil

  /**
   * This is the personal setting of tracing. These is a public setting as well.
   * If tracing is active for this actor depends on both settings, in a symmetric manner.
   * If both are Enabled or one is Enabled and the other is Default, the tracing is
   * active. In all other cases it is not. This implies that you can enable/disable
   * the tracing here as long as the global tracing is Default or Enabled. Setting this
   * setting to Disabled will always prohibit tracing of this actor, and setting it to
   * Default/Enabled leaves the fate in the hands of the global setting. */
  @volatile protected var tracing: Tracing = Tracing.Default

  /**
   * See if we may trace this actor now. This is the case if one of them is Enabled
   * and the other Default or Enabled. */
  private var mayTrace: Boolean = tracing.ordinal + monitor.tracing.ordinal > 2

  /* Provisional way to regain the letters/senders from the envelope. */
  private[actors] def repack(env: Env): BareActor.Envelope[MyLetter,Sender]

  /** Method called from the actor store its activity */
  private def addTrace(trace: Trace): Unit = synchronized { traces = trace :: traces }

  /** Calculate the gain in time since the last visit. */
  private def gain(newClockTime: Long): Long =
    /* Calculate the increase in time. This should be positive of course, but there never
     * is an absolute guarantee of the presence of a monotic clock on every OS. So better
     * be inaccurate as negative. */
    val result        = math.abs(newClockTime - lastClockTime)
    lastClockTime     = newClockTime
    result

  /** Variable holding the cancel object for probing the actor. */
  private var cancelProbe = Cancellable.empty

  /** Method to create a new scheduable object to probe the actor. */
  private def callProbe   = new Callable[Unit] { def call(): Unit = probeNow(true) }

  /** Method to do the actual probing of the actor. */
  private def probeNow(continue: Boolean): Unit =
    /* Schedule the probing to take place over a fixed amount of time. */
    cancelProbe = if continue then context.schedule(callProbe,monitor.probeInterval) else Cancellable.empty
    val (samples,traces) = synchronized {
      /* Do the actual probing work on all mixins and the bare actor trait. */
      val samples = List(probeBare(),probeStash(),probeTiming(),probeFamily()).flatten
      /* Get the traces gathered from this actor */
      val traces = this.traces
      /* Clean the trace storage */
      this.traces = Nil
      /* Communicate the result outside the synchronized environment */
      (samples,traces) }
    /* Store the collected samples in the monitor storage */
    monitor.setSamples(storePath,samples)
    /* Store the traces in the monitor storage */
    monitor.setTraces(path,traces)


  /** Cancel the delayed probe actions, but also perfom one last probe action right now. */
  private def probeCancel() = synchronized {
    cancelProbe.cancel()
    probeNow(false) }

  /** Values contains if this actor is a worker based on its name prefix (# per default) */
  private val isWorker = name.startsWith(workerPrefix)

  /* For workers we do not want to use the full name, but a one name for all workers in this family. So
   * we only use the path up to and including the worker prefix. The rest is dropped. */
  /** path under which we store the data for this actor. Its the full family name or a shorten version in case of a worker. */
  private val storePath = if isWorker then path.substring(0,path.length()-name.length()+workerPrefix.length()) else path

  /**
   * Method called from the actor to indicate that operations have commenced. Since the actor mixes in this trait,
   * it can be called from this trait as well. */
  private[actors] def monitorStart(): Unit =
    val time = System.nanoTime
    threadStartMoment = gain(time)
    monitor.addActor(storePath)
    if mayTrace then addTrace(Trace(time-monitor.baseline,path,Action.Created))
    probeNow(true)

  /** Method called from the actor to indicate that it receives a new letter */
  private[actors] override def monitorSend(isActive: Boolean, envelope: Env): Unit =
    /* Trace if requested, the letter is received or rejected. */
    if mayTrace then addTrace(Trace(System.nanoTime-monitor.baseline,path,isActive,repack(envelope)))

  /** Method called from the actor to indicate that it starts processing a letter. */
  private[actors] override def monitorEnter(envelope: Env): Unit =
    val time = System.nanoTime
    /* Trace if requested, the letter processing is initiated */
    if mayTrace then addTrace(Trace(time-monitor.baseline,path,Action.Initiated,repack(envelope)))
    /* The time past since has to be added to the total time in pause. */
    treadPauseTime += gain(time)

  /** Method called from the actor to indicate that it finished processing a letter. */
  private[actors] override def monitorExit(envelope: Env): Unit  =
    val time = System.nanoTime
    /* The time past since has to be added to the total time in play. */
    threadPlayTime += gain(time)
    /* Trace if requested, the letter processing is completed */
    if mayTrace then addTrace(Trace(time-monitor.baseline,path,Action.Completed,repack(envelope)))

  /** Method called from the actor to indicate that we are done in this actor. */
  private[actors] override def monitorStop(): Unit  =
    val time = System.nanoTime
    /* The time past since has to be added to the total time in pause, since we can
     * never directly stop inside a letter. */
    treadPauseTime += gain(time)
    /* Tell the monitor this actor is done. Depending on the path it may just ignore
     * it or take cleaning up actions. */
    monitor.delActor(storePath)
    /* Probing makes no sense any more. Determine the last samples and stop scheduled probe actions. */
    probeCancel()
    /* Trace if requested, the actor is terminated */
    if mayTrace then addTrace(Trace(time-monitor.baseline,path,Action.Terminated))

  /** Calculate the relative time this actor spend performing processing letters. */
  private[actors] override def userLoad: Double =
    val totalThreadTime = threadPlayTime + treadPauseTime
    if totalThreadTime == 0 then 0D else threadPlayTime.toDouble/totalThreadTime.toDouble

  /* Activates the real time monitoring. */
  monitorStart()


object MonitorActor:

  enum Action :
    case Created, Terminated, Accepted, Refused, Initiated, Completed

  enum Tracing :
    case Disabled, Default, Enabled

  case class Trace(time: Long, actor: String, action: Action, letter: String, sender: String) extends Ordered[Trace] :
    def compare(that: Trace): Int =
      if      this.time < that.time then -1
      else if this.time > that.time then  1
      else                                0
    def show = s"time=$time, actor=$actor, action=$action, letter=$letter, sender=$sender"

  object Trace :
    def empty(time: Long) = new Trace(time,"",Action.Created,"","")
    def apply[L,S <: Actor[?]](time: Long, actor: String, accept: Boolean, env: BareActor.Envelope[L,S]): Trace =
      apply(time,actor,if accept then Action.Accepted else Action.Refused,env)
    def apply[L,S <: Actor[?]](time: Long, actor: String, action: Action, env: BareActor.Envelope[L,S]): Trace =
      new Trace(time,actor,action,env.letter.toString,env.sender.path)
    def apply[L,S <: Actor[?]](time: Long, actor: String, action: Action): Trace =
      new Trace(time,actor,action,"","")

  /** Class to return the results on a monitor probe. */
  sealed trait Sample :
    def show: String

  /** Class to marshall all the KPI's of the bare actor. */
  case class Bare(phase: BareActor.Phase, lettersSum: Int, lettersMax: Int, exceptionsSum: Int, userLoad: Double) extends Sample :
    def userPPM = (userLoad * 1000000).toInt
    def show = s"phase=$phase, lettersSum=$lettersSum, lettersMax=$lettersMax, exceptionsSum=$exceptionsSum, userLoad=${userPPM}ppm"

  /** Class to marshall all the KPI's of the Stash mixin. */
  case class Stash(lettersSum: Int, lettersMax: Int) extends Sample :
    def show = s"stashSum=$lettersSum, stashMax=$lettersMax"

  /** Class to marshall all the KPI's of the Timing mixin. */
  case class Timing(lettersSum: Int, lettersMax: Int, anchorsSize: Int) extends Sample :
    def show = s"timersSum=$lettersSum, timersMax=$lettersMax, timersNow=$anchorsSize"

  /** Class to marshall all the KPI's of the Families mixins. */
  case class Family(childerenNow: Int, workersSum: Long) extends Sample :
    def show = s"childerenNow=$childerenNow, workersSum=$workersSum"


