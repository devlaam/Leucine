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


import java.util.concurrent.{Callable, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.scalajs.js.timers
import scala.scalajs.js.timers.SetTimeoutHandle


/** Context implementation for Javascript */
abstract class ContextImplementation extends PlatformContext :

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

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    if active then
      val timeoutHandle: SetTimeoutHandle = timers.setTimeout(delay)(callable.call())
      new Cancellable { def cancel() = timers.clearTimeout(timeoutHandle) }
    else Cancellable.empty

  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
  * digestible process. As longs as there is no result yet, the attempt should produce None */
  def await[M](digestible: Digestible[M], attempt: => Option[M]): Cancellable =
    if active
    then new ContextImplementation.Awaitable(attempt.map(digestible.digest).isDefined,idleThreadPause)
    else Cancellable.empty

  /**
   * In JavaScript it is not possible to shutdown the only executer, so this just makes sure that no new
   * runnables/callables are scheduled for execution. Eventually all actors will be starved. */
  def shutdown(force: Boolean): Unit = _active = false

  /** This method makes the thread loop ready for reuse after termination. Not required for this platform. */
  private[s2a] def revive(): Unit =  ()

  /**
   * This method enters an endless loop but immedeately returns. Every timeout, it will probe a shutdownrequest.
   * There may be other reasons for shutdown as well. After all threads have completed (by force or not) the method
   * calls complete(). Call in the main thread as last action there. In JavaScript it is not possible to block,
   * so this uses a schedule timer to see if we are complete. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean, complete: () => Unit): Unit =
    def delay: Callable[Unit] = new Callable[Unit] { def call(): Unit = tryExit(true) }
    def tryExit(check: Boolean) = if !active then complete() else
      schedule(delay,time)
      if check && shutdownRequest then shutdown(force)
    tryExit(false)



object ContextImplementation :

  /** Returns the platform that is currently running, here Java Script. */
  def platform = PlatformContext.Platform.JS

  /**
   * Class which continously retries an attempt until it succeeds or is cancelled. The doAttempt
   * by name reference should return true if it succeedded and false otherwise. The delay between
   * each attempt should be in ms. The attempt itself should not block. */
  private class Awaitable(doAttempt: => Boolean, delay: FiniteDuration) extends Cancellable :
    private var continue = true
    private def schedule(c: Callable[Unit]): SetTimeoutHandle = timers.setTimeout(delay)(callable.call())
    private var th: SetTimeoutHandle = schedule(callable)
    private def callable: Callable[Unit] = new Callable[Unit] :
      def call() = if continue && !doAttempt then th = schedule(this)
    def cancel() = { continue = false; timers.clearTimeout(th) }

  /** Real sleeps are not possible on JS, so we cheat with a timer. */
  private[actors] def sleep(loop: => Unit, delay: FiniteDuration): Boolean =
    timers.setTimeout(delay)(loop)
    false

