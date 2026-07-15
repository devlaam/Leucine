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
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetTimeoutHandle


/** Context implementation for Javascript */
abstract class ContextImplementation extends PlatformContext :
  import Auxiliary.toUnit

  private lazy val executionContext = ExecutionContext.global

  /** True as long as there has been no Shutdown request. */
  private var _active: Boolean = true

  /** Save access method on the _active variable */
  def active: Boolean = _active

  /** Indicates if the context runs on system threads or in an emulated environment. */
  def emulated: Boolean = false

  /**
   * True if all treads have completed, for JS this is never the case since the main
   * thread is always running. We cannot probe if the tasks there were scheduled manually
   * all have been completed.  */
  def terminated: Boolean = false

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then executionContext.execute(runnable)

  /** Enqueue a new task on the special single threaded executor. */
  def enqueue(runnable: Runnable): Unit = execute(runnable)

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    /* We only accept new schedules tasks when the system is still running. */
    if active then
      /* Start the timer with the callable. */
      val timeoutHandle: SetTimeoutHandle = timers.setTimeout(delay)(callable.call())
      /* Construct a Cancellable with the cancel method clearing the timer. */
      new Cancellable { def cancel() = timers.clearTimeout(timeoutHandle) }
    /* If not active, there is no task started, so nothing to cancel. */
    else Cancellable.empty

  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
   * digestible process. As longs as there is no result yet, the attempt should produce None.
   * The attempt as well as the digest from digestible are both executed on the main (only) thread
   * and should finish asap. Should not to any work. Typically use this to send a letter.  */
  def await[M](digestible: Digestible[M], attempt: => Option[M]): Cancellable =
    /* Proactively test if we should start the construction of the Awaitable at all. */
    if active
    /* If so, try to make one and return it as Cancellable. We give active as mayAttempt parameter
     * here so no new attempts are scheduled in case of a shutdown. */
    then ContextImplementation.Awaitable(attempt.map(digestible.digest).isDefined,active,idleThreadPause)
    /* Otherwise there is nothing to cancel. */
    else Cancellable.empty

  /**
   * In JavaScript it is not possible to shutdown the only executor, so this just makes sure that no new
   * runnables/callables are scheduled for execution. Eventually all actors will be starved. Note that
   * the force option does nothing in this case, there is nothing to force in a single threaded application. */
  def shutdown(force: Boolean): Unit = _active = false

  /** This method makes the thread loop ready for reuse after termination. Not required for this platform. */
  private[s2a] def revive(): Unit =  ()

  /**
   * This method enters an endless time loop and immediately returns. Every timeout, it will probe a shutdown request.
   * There may be other reasons for shutdown as well. After all "threads" have completed (by force or not) the method
   * calls complete(). Call in the main thread as last action there. In JavaScript it is not possible to block,
   * so this uses a scheduled timer to see if we are complete. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean, complete: () => Unit): Unit =
    /* Make a callable with an action to perform when the timer expires */
    def delay: Callable[Unit] = new Callable[Unit] { def call(): Unit = tryExit(true) }
    /* Every time the timer expires we get a call with timer=true. When called manually, then timer=false */
    def tryExit(timer: Boolean) = //if !active then complete() else
      /* In case we are still active ... */
      if active then
        /* ... we must reschedule the timer */
        schedule(delay,time).toUnit
        /* ... and test if we received a shutdown request, if so invoke a shutdown. We do not want a shutdown
         * on the manual start, so skip it in that case and let the system take off. */
        if timer && shutdownRequest then shutdown(force)
      /* ... if not, we may call complete. No timer is rescheduled, so this is definitively the last call. */
      else complete()
    /* Start the timer manually and exit. */
    tryExit(false)



object ContextImplementation :
  import Auxiliary.toUnit

  /** Returns the platform that is currently running, here Java Script. */
  def platform = PlatformContext.Platform.JS


  /**
   * Class which continuously retries an attempt until it succeeds or is cancelled. The doAttempt
   * by name reference should return true if it succeeded and false otherwise. The delay between
   * each attempt should be in ms. The parameter mayAttempt should return true as long as we
   * are allowed to perform an attempt. The attempt itself should not block. */
  private class Awaitable(doAttempt: => Boolean, mayAttempt: => Boolean, delay: FiniteDuration) extends Cancellable :

    /* Allows prohibiting this awaitable to reschedule itself. */
    private var continue = true

    /* Call to schedule a new task in the future. */
    private def schedule(callable: Callable[Unit]): SetTimeoutHandle = timers.setTimeout(delay)(callable.call())

    /* Holding variable for the current active scheduled task.*/
    private var th: SetTimeoutHandle = schedule(callable)

    /* Construct a new callable with an embedded call. */
    private def callable: Callable[Unit] = new Callable[Unit] :
      /* This is called as soon as the scheduled timer has ended. It issues a new doAttempt
       * if this allowed by mayAttempt, which should be false if no work is allowed in the
       * main executor anymore. */
      def call() =
        /* See if we may continue. The order here is important due to short circuiting. If
         * continue was already false there is nothing to do. Otherwise see if we still are
         * allowed to do an attempt. If so do the actual attempt and see if we are done. */
        continue = continue && mayAttempt && !doAttempt
        /* If so (yes, there can be a race since the permission may be revoked in the
         * meantime, but that will at be cost one timeout at maximum or the task may
         * be directly rejected) then ... */
        if continue then th = schedule(this)

    /** Prohibit this awaitable to reschedule and try to cancel any already scheduled awaitable. */
    def cancel() = if continue then
      /* The method does nothing if continue was already false, if not, make it so */
      continue = false
      /* and prohibit scheduling a new task, but once started, do not interrupt. */
      timers.clearTimeout(th)


  /** Real sleeps are not possible on JS, so we cheat with a timer. */
  private[actors] def sleep(loop: => Unit, delay: FiniteDuration): Boolean =
    timers.setTimeout(delay)(loop).toUnit
    false

