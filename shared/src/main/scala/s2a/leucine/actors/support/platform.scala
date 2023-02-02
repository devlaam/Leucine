package s2a.leucine.actors

import java.util.concurrent.Callable
import scala.concurrent.duration.FiniteDuration


/** Used to construct the actor context with platform depended methods. */
trait PlatformContext:

  /** The average thread load per core. Override to change. This is only used on multithreaded platforms. */
  def load: Int = 4

  /** The natural pause time for this context. Its meaning and use is platform dependant. */
  def pause: FiniteDuration

  /** True as long as there has been no Shutdown request. */
  def active: Boolean

  /**
   * True if all treads have completed, for JS this is never the case since the main
   * thread is always running. We cannot probe if the tasks there were scheduled manually
   * all have been completed.  */
  def terminated: Boolean

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable

  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
   * digestable process. As longs as there is no result yet, the attempt should produce None */
  def await[M](digestable: Digestable[M], attempt: () => Option[M]): Cancellable

  /**
   * Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
   * there current thasks. With force=true the current execution is interrupted. In any case, no new tasks
   * will be accepted. */
  def shutdown(force: Boolean): Unit

  /**
   * This method waits until the application finishes. Every timeout, it will probe a shutdownrequest.
   * There may be other reasons for shutdown as well. After all threads have completed (by force or not) the method
   * returns. Call in the main thread as last action there.
   * After return some other tasks may still be runnning. This will usually not be a problem, since
   * when they complete the application will exit, just as intented, or, inside a webapp, keeps running,
   * needed to be able to continue to respond on other events. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean): Unit
