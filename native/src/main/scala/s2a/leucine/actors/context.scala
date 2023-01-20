package s2a.leucine.actors


import java.util.concurrent.Callable

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.{Queue,SortedMap}

/* Unfortunately the current native implementation does not have threadpools or timers. We only have a
 * global execution context and a Thread.Sleep. So we have to implement a loop ourselves for the moment.
 * One advantage: no need for synchronization! */
class ContextImplementation extends PlatformContext :

  private lazy val executionContext = ExecutionContext.global

  /* Set this to false to break the main loop */
  var continue = true

  /* Queue for all runnables that need to be executed directly */
  private var tasks: Queue[Runnable] = Queue.empty

  /* Map for all callables that need to be executed on a specific moment. */
  private var timers: SortedMap[Long,Callable[Unit]] = SortedMap.empty

  /* List for all callables that need attemped every cycle. */
  private var attempts: List[Callable[_]] = Nil

  /* Variable that keeps track of the time this application has run since
   * the start. In nanosec*/
  private var passedTime: Long = 0

  /* Time the system goes to sleep if there is nothing to do. In millisec */
  private val pauseMS: Int  = 10

  private def hasFirstTimer: Boolean = !timers.isEmpty && (timers.head._1 < passedTime)
  private def execFirstTimer(): Unit =
    val (time,task) = timers.head
    timers = timers - time
    task.call()

  private def hasFirstTask: Boolean = !tasks.isEmpty
  private def execFirstTask(): Unit =
    val (task,rest) = tasks.dequeue
    tasks = rest
    task.run()


  /* Main loop with a periodic hook */
  private def mainLoop(hook: () => Unit, interval: Long): Unit =
    /* Stores the time the system first started (not necessary the real time) */
    val baseTime: Long = System.nanoTime()
    /* Stores the last time we called the hook */
    var lastHook: Long = 0L
    /* Test if enough time has past to make a new call to then hook */
    def mustCallHook = passedTime - lastHook > interval

    /* While we are allowed to run the full program in this loop */
    while continue do
      /* Every turn we measure how much time has passed. */
      passedTime = System.nanoTime() - baseTime
      /* In order of priorty execute the following orders: */
      if      hasFirstTimer then execFirstTimer()
      else if hasFirstTask  then execFirstTask()
      else                       Thread.sleep(pauseMS)
      /* See if we must call the hook. */
      /* If there are no tasks and timers left, where is nothing to continue. */
      continue = continue && (!tasks.isEmpty || !timers.isEmpty)



  /* Application starts active. Once this is set to false, the planned tasks and timers
   * are completed, no new ones will be accepted. */
  private var _active = true

  /** True as long as there has been no Shutdown request. */
  def active =  _active

  /** True if all treads have completed, for the current Native this is never the case
    * since the main has stopped if you could get true, but you can only read this from
    * main tread. */
  def terminated = !continue

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then tasks = tasks.enqueue(runnable)

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    var time = passedTime + delay.toNanos
    /* There may already be a time with this exact delay, so we plan it a few nanosecs later. */
    while timers.contains(time) do time = time + 1
    timers = timers + (time -> callable)
    new Cancellable { def cancel() = timers = timers - time  }

  /** Place a task on the Execution Context which is executed after some event arrives.
    * The arrival of the event is asynchronically probed by the attempt function,
    * which should return None in case of a failure. */
  def await[M](callable: Callable[Unit], attempt: () => Option[M]): Cancellable = ???
    // var active: Boolean = true
    // val makeAttempt: Callable[Option[M]] = new Callable[Option[M]] { def call() = if active then attempt() else None }
    // new Cancellable { def cancel() = active = false  }

  /** Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
    * there current thasks. With force=true the current execution is interrupted. In any case, no new tasks
    * will be accepted. */
  def shutdown(force: Boolean): Unit =
    _active = false
    if force then continue = false

  /** This method enters an endless loop until the application finishes. Every timeout, it will probe a shutdownrequest.
    * There may be other reasons for shutdown as well. After all threads have completed (by force or not) the method
    * returns. Call in the main thread as last action there. In Native it this also starts the mainloop. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean): Unit =
    def hook(): Unit = { if shutdownRequest then shutdown(force) }
    mainLoop(hook,time.toNanos)

