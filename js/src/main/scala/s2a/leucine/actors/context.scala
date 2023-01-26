package s2a.leucine.actors


import java.util.concurrent.{Callable, TimeUnit}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetTimeoutHandle


abstract class ContextImplementation extends PlatformContext :

  private lazy val executionContext = ExecutionContext.global

  private var _active = true

  /** True as long as there has been no Shutdown request. */
  def active =  _active

  /** True if all treads have completed, for JS this is never the case since the main
    * thread is always running. We cannot probe if the tasks there were scheduled manually
    * all have been completed.  */
  def terminated = false

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then executionContext.execute(runnable)

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    val timeoutHandle: SetTimeoutHandle = timers.setTimeout(delay)(callable.call())
    new Cancellable { def cancel() = timers.clearTimeout(timeoutHandle) }

  /** Place a task on the Execution Context which is executed after some event arrives. When
    * it arrives it may produce an result of some type. This result is subsequently passed to the
    * digestable process. As longs as there is no result yet, the attempt should produce None */
  def await[M](digestable: Digestable[M], attempt: () => Option[M]): Cancellable =
    new ContextImplementation.Awaitable(attempt().map(digestable.digest).isEmpty,pause)

  /** In JavaScript it is not possible to shutdown the only executer, so this just makes sure that no new
    * runnables/callables are scheduled for execution. Eventually all actors will be starved. */
  def shutdown(force: Boolean): Unit = _active = false

  /** This method enters an endless loop until the application finishes. Every timeout, it will probe a shutdownrequest.
    * There may be other reasons for shutdown as well. After all thread have completed (by force or not) the method
    * returns. Call in the main thread as last action there. In JavaScript it is not possible to block, so this uses a
    * schedule timer to see if we may stop.
    * After return some other tasks may still be runnning. This will usually not be a problem, since
    * when they complete the application will exit, just as intented, or, inside a webapp, keeps running,
    * needed to be able to continue to respond on other events. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean): Unit =
    def delay: Callable[Unit] = new Callable[Unit] { def call(): Unit = tryExit() }
    def tryExit() = if active then
      if shutdownRequest then shutdown(force)
      schedule(delay,time)
    tryExit()



object ContextImplementation :

  /* Class which continously retries an attempt until it succeeds or is cancelled. The doAttempt
   * by name reference should return true if it succeedded and false otherwise. The delay between
   * each attempt should be in ms. The attempt itself should not block. */
  private class Awaitable(doAttempt: => Boolean, delay: FiniteDuration) extends Cancellable :
    private var continue = true
    private def schedule(c: Callable[Unit]): SetTimeoutHandle = timers.setTimeout(delay)(callable.call())
    private var th: SetTimeoutHandle = schedule(callable)
    private def callable: Callable[Unit] = new Callable[Unit] :
      def call() = if continue && !doAttempt then th = schedule(this)
    def cancel() = { continue = false; timers.clearTimeout(th) }

