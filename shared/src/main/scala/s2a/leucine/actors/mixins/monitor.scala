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
  private[actors] def probeBare(): Option[MonitorAid.Bare]  = None
  private[actors] def probeStash(): Option[MonitorAid.Stash]  = None
  private[actors] def probeTiming(): Option[MonitorAid.Timing]  = None
  private[actors] def probeFamily(): Option[MonitorAid.Family]  = None
  private[actors] def monitorSend(isActive: Boolean, envelope: Env): Unit = ()
  private[actors] def monitorEnter(envelope: Env): Unit = ()
  private[actors] def monitorExit(envelope: Env): Unit = ()
  private[actors] def monitorStop(): Unit = ()
  private[actors] def userLoad: Double = 0


/** Extend your actor with this mixin to put it under monitoring */
trait MonitorAid(monitor: ActorMonitor)(using context: ActorContext) extends ActorDefs :
  this: NameActor =>
  import MonitorAid.{Action, Trace, Post, Tracing}

  /* Fields to measure the time spend inside the thread and outside the thread */
  private var lastClockTime: Long = 0
  private var threadStartMoment: Long = 0
  private var threadPlayTime: Long = 0
  private var treadPauseTime: Long = 0

  /* Temporary fast storage for the trace objects. */
  private var traces: List[Trace] = Nil

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
    /* Store the posts in the monitor storage */
    if mayTraceCount then monitor.setPosts(path,traces)
    /* Store the traces in the monitor storage */
    if mayTraceAll then monitor.setTraces(path,traces)


  /** Cancel the delayed probe actions, but also perfom one last probe action right now. */
  private def probeCancel() = synchronized {
    cancelProbe.cancel()
    probeNow(false) }

  /* For workers we do not want to use the full name, but a one name for all workers in this family. So
   * we only use the path up to and including the worker prefix. The rest is dropped. */
  /** path under which we store the data for this actor. Its the full family name or a shorten version in case of a worker. */
  private val storePath = if isWorker then path.substring(0,path.length()-name.length()+context.workerPrefix.length()) else path

  /**
   * Method called from the actor to indicate that operations have commenced. Since the actor mixes in this trait,
   * it can be called from this trait as well. */
  private[actors] def monitorStart(): Unit =
    val time = System.nanoTime
    threadStartMoment = gain(time)
    monitor.addActor(storePath)
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Created,path))
    probeNow(true)

  /** Method called from the actor to indicate that it receives a new letter */
  private[actors] override def monitorSend(isActive: Boolean, envelope: Env): Unit =
    /* Trace if requested, the letter is received or rejected. */
    if mayTraceCount then addTrace(Trace(System.nanoTime-monitor.baseline,isActive,path,repack(envelope)))

  /** Method called from the actor to indicate that it starts processing a letter. */
  private[actors] override def monitorEnter(envelope: Env): Unit =
    val time = System.nanoTime
    /* Trace if requested, the letter processing is initiated */
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Initiated,path,repack(envelope)))
    /* The time past since has to be added to the total time in pause. */
    treadPauseTime += gain(time)

  /** Method called from the actor to indicate that it finished processing a letter. */
  private[actors] override def monitorExit(envelope: Env): Unit  =
    val time = System.nanoTime
    /* The time past since has to be added to the total time in play. */
    threadPlayTime += gain(time)
    /* Trace if requested, the letter processing is completed */
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Completed,path,repack(envelope)))

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
    if mayTraceAll then addTrace(Trace(time-monitor.baseline,Action.Terminated,path))

  /** Calculate the relative time this actor spend performing processing letters. */
  private[actors] override def userLoad: Double =
    val totalThreadTime = threadPlayTime + treadPauseTime
    if totalThreadTime == 0 then 0D else threadPlayTime.toDouble/totalThreadTime.toDouble

  /* Activates the real time monitoring. */
  monitorStart()


object MonitorAid :

  /* All trace entries are counted, so you can see none is failing. */
  private var _tracer: Int = 0
  private def tracer: Int = synchronized { _tracer += 1; _tracer }

  enum Action :
    case Created, Terminated, Accepted, Refused, Initiated, Completed
  object Action :
    def handOver(accept: Boolean) = if accept then Accepted else Refused

  enum Tracing :
    case Disabled, Default, Enabled

  class Trace(val time: Long, val action: Action, val post: Post) extends Ordered[Trace] :
    var nr = tracer
    /* Sorting is first on action time and subsequently on trace number. This should be suffient to be unique in
     * all circumstances. */
    def compare(that: Trace): Int =
      if      this.time < that.time then -1
      else if this.time > that.time then  1
      else if this.nr   < that.nr   then -1
      else if this.nr   > that.nr   then  1
      else if this.##   < that.##   then -1
      else if this.##   > that.##   then  1
      else                                0
    def show = s"time=$time, nr=$nr, action=$action, letter=${post.letter}, from=${post.receiver}, to=${post.sender}"

  object Trace :
    def empty(time: Long) = new Trace(time,Action.Created,Post.empty)
    def apply[L,S <: Actor](time: Long, accept: Boolean, receiver: String, env: BareActor.Envelope[L,S]): Trace =
      apply(time,Action.handOver(accept),receiver,env)
    def apply[L,S <: Actor](time: Long, action: Action, receiver: String, env: BareActor.Envelope[L,S]): Trace =
      new Trace(time,action,Post(receiver,env.letter.toString,env.sender.path))
    def apply[L,S <: Actor](time: Long, action: Action, receiver: String): Trace =
      new Trace(time,action,Post(receiver,"",""))

  case class Post(val receiver: String, val letter: String, val sender: String) extends Ordered[Post] :
    def compare(that: Post): Int =
      if      this.sender   < that.sender   then -1
      else if this.sender   > that.sender   then  1
      else if this.receiver < that.receiver then -1
      else if this.receiver > that.receiver then  1
      else if this.letter   < that.letter   then -1
      else if this.letter   > that.letter   then  1
      else                                  0
    def show= s"from=$sender, to=$receiver, letter=$letter"

  object Post :
    val empty = new Post("","","")

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
  case class Family(namedChildrenNow: Int, allChildrenNow: Int, workersSum: Long) extends Sample :
    def show = s"namedChildrenNow=$namedChildrenNow, allChildrenNow=$allChildrenNow, workersSum=$workersSum"
