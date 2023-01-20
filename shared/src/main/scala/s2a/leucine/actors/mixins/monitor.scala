package s2a.leucine.actors

import java.util.concurrent.Callable

trait MonitorDefs :
  private[actors] def probeBare(): Option[MonitorActor.Bare]  = None
  private[actors] def probeStash(): Option[MonitorActor.Stash]  = None
  private[actors] def probeTiming(): Option[MonitorActor.Timing]  = None
  private[actors] def probeFamily(): Option[MonitorActor.Family]  = None
  private[actors] def monitorEnter(): Unit = ()
  private[actors] def monitorExit(): Unit = ()
  private[actors] def monitorStop(): Unit = ()
  private[actors] def userLoad: Double = 0

/* Extend your actor with this mixin to put it under monitoring */
trait MonitorActor(monitor: ActorMonitor)(using context: ActorContext) extends ActorDefs :

  protected def workerPrefix: String
  protected def familyPathSeparator: Char

  /* Fields to measure the time spend inside the thread and outside the thread*/
  private var lastClockTime: Long = 0
  private var threadStartMoment: Long = 0
  private var threadPlayTime: Long = 0
  private var treadPauseTime: Long = 0

  /* Calculate the gain in time since the last visit. */
  private def gain: Long =
    val newClockTime  = System.nanoTime()
    /* Calculate the increase in time. This should be positive of course, but there never
     * is an absolute guarantee of the presence of a monotic clock on every OS. So better
     * be inaccurate as negative. */
    val result        = math.abs(newClockTime - lastClockTime)
    lastClockTime     = newClockTime
    result

  private var cancelProbe = Cancellable.empty
  private def callProbe   = new Callable[Unit] { def call(): Unit = probeNow(true) }

  private def probeNow(continue: Boolean): Unit = synchronized {
    cancelProbe = if continue then context.schedule(callProbe,monitor.probeInterval) else Cancellable.empty
    val samples = List(probeBare(),probeStash(),probeTiming(),probeFamily()).flatten
    monitor.setSamples(storePath,samples) }

  private def probeCancel() = synchronized {
    cancelProbe.cancel()
    probeNow(false) }


  private val isWorker = name.startsWith(workerPrefix)

  /* For workers we do not want to use the full name, but a one name for all workers in this family. So
   * we only use the path up to and including the worker prefix. The rest is dropped. */
  private val storePath = if isWorker then path.substring(0,path.length()-name.length()+workerPrefix.length()) else path

  private[actors] def monitorStart(): Unit =
    threadStartMoment = gain
    monitor.addActor(storePath)
    probeNow(true)

  private[actors] override def monitorEnter(): Unit =
    treadPauseTime += gain

  private[actors] override def monitorExit(): Unit  =
    threadPlayTime += gain

  private[actors] override def monitorStop(): Unit  =
    treadPauseTime += gain
    monitor.delActor(storePath)
    probeCancel()

  private[actors] override def userLoad: Double =
    val totalThreadTime = threadPlayTime + treadPauseTime
    if totalThreadTime == 0 then 0D else threadPlayTime.toDouble/totalThreadTime.toDouble

  monitorStart()


object MonitorActor:
  /* Class to return the results on a monitor probe.*/
  sealed trait Sample :
    def show: String
  case class Bare(phase: BareActor.Phase, lettersSum: Int, lettersMax: Int, exceptionsSum: Int, userLoad: Double) extends Sample :
    def userPPM = (userLoad * 1000000).toInt
    def show = s"phase=$phase, lettersSum=$lettersSum, lettersMax=$lettersMax, exceptionsSum=$exceptionsSum, userLoad=${userPPM}ppm"
  case class Stash(lettersSum: Int, lettersMax: Int) extends Sample :
    def show = s"stashSum=$lettersSum, stashMax=$lettersMax"
  case class Timing(lettersSum: Int, lettersMax: Int, anchorsSize: Int) extends Sample :
    def show = s"timersSum=$lettersSum, timersMax=$lettersMax, timersNow=$anchorsSize"
  case class Family() extends Sample :
    def show = ""


