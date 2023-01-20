package s2a.leucine.actors


import java.util.concurrent.{Executors, ThreadFactory, TimeUnit, Callable, ScheduledFuture}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration


class ContextImplementation extends PlatformContext :

  private lazy val threadPool       = ContextImplementation.threadPool(true)
  private lazy val executionContext = ExecutionContext.fromExecutorService(threadPool)
  private lazy val scheduler        = Executors.newSingleThreadScheduledExecutor(ContextImplementation.threadFactoryDefault)

  /** True as long as there has been no Shutdown request. */
  def active = !executionContext.isShutdown()

  /** True if all treads have completed */
  def terminated = executionContext.isTerminated()

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then executionContext.execute(runnable)

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    val scheduledFuture: ScheduledFuture[Unit] = scheduler.schedule[Unit](callable,delay.toMillis,TimeUnit.MILLISECONDS)
    new Cancellable { def cancel() = scheduledFuture.cancel(false)  }

  /** Place a task on the Execution Context which is executed after some event arrives.
    * The arrival of the event is asynchronically probed by the attempt function,
    * which should return None in case of a failure. */
  def await[M](callable: Callable[Unit], attempt: () => Option[M]): Cancellable = ???

  /** Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
    * there current thasks. With force=true the current execution is interrupted. In any case, no new tasks
    * will be accepted. */
  def shutdown(force: Boolean): Unit = if force then executionContext.shutdownNow() else executionContext.shutdown()

  /** This method enters an endless loop until the application finishes. Every timeout, it will probe a shutdownrequest.
    * There may be other reasons for shutdown as well. After all thread have completed (by force or not) the method
    * returns. Call in the main thread as last action there. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean): Unit =
    println(s"waitForExit JVM")
    while active || !terminated do
      if active && shutdownRequest then shutdown(force)
      threadPool.awaitTermination(time.toSeconds, TimeUnit.SECONDS)


object ContextImplementation :

  val processorCount = Runtime.getRuntime().availableProcessors()

  /* Non-daemon threads keep the application from exiting. Daemon threads do not, but an application
   * exit does not terminate the threads by itself. (Note the JVM itself must be running, an JVM exit
   * terminates daemon threads nevertheless). Daemon threads run at lower priority.
   * Thus this means they may still run after the exit. This might be the intention, but usually
   * it is not and produce strange effects. In an orderly system you want all threads to have
   * finished before you relinguish control to the user. Or you must be sure to be the sole process
   * in the JVM. */

  def threadFactoryDefault = Executors.defaultThreadFactory()

  def threadFactory(daemon: Boolean) = new ThreadFactory :
    override def newThread(runnable: Runnable) =
      val thread = new Thread(runnable)
      thread.setDaemon(daemon)
      thread

  def threadPool(daemon: Boolean) = Executors.newFixedThreadPool(processorCount,threadFactoryDefault)

