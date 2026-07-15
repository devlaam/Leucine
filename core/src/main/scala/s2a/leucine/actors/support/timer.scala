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
import scala.concurrent.duration.{FiniteDuration, DurationInt}


/**
 * Class to embed timer functions needed to make repeated calls. The parameter interval is
 * for the user to define. Set this to a reasonable value, say 5 seconds for short running
 * applications and maybe 1 minute for servers. The action is called at repeated intervals
 * until the timer is stopped. This is a reusable timer. The action should not be used to
 * do vast amounts of work, since all timers share one thread. If needed relocate the work
 * to a new thread. Make sure this call is reentrant and thread safe. */
private class Timer(val interval: FiniteDuration, val action: () => Unit)(using context: ActorContext)  :

  /* Variable that keeps the state of timer. If true new actions are scheduled on regular intervals.
   * If false, new actions are not scheduled any more. */
  private var enabled: Boolean = false

  /** Variable holding the cancel object for new actions. */
  private var cancellable = Cancellable.empty

  /** Set new enabled state, return true if anything changed. */
  private def enable(state: Boolean): Boolean =
    if enabled == state then false else
      enabled = state
      true

  /** Method to create a new schedulable object to generate a timer action. */
  private def callable = new Callable[Unit] :
    /* The executed method when the timer expires is call() */
    def call(): Unit =
      /* First perform the action */
      action()
      /* If still enabled, reschedule a new timer, if not, clear the cancellable.
       * Synchronize this in order to have a well defined cancellable value. */
      synchronized(renew(enabled,interval))

  /** Call this to restart the actions, if we are still carrying out actions. */
  private def renew(act: Boolean, delay: FiniteDuration): Unit =
    /* Schedule the event to take place over a fixed amount of time. If act is false,
     * clean the cancellable variable to signal there is no current timer running. */
    cancellable = if act then context.schedule(callable,delay) else Cancellable.empty

  /**
   * Start the timer. Set forceFirst to true, if you directly want the first action to
   * be called. If the timer from the last active period is still running, no new timer
   * is started and forceFirst is ignored. */
  private[actors] def start(forceFirst: Boolean) = synchronized :
    /* The enabled flag must be set to continue scheduling late actions. Only
     * act if something actually changed. If however the timer is still active
     * possible after a stop(allowLast(true) we do not start a new timer.
     * Note that it is essential this all happens inside the synchronized environment.
     * This ensures we are not inside an renew, so cancellable will have the
     * last defined value. We are also not inside stop() itself. */
    if enable(true) && cancellable.isEmpty then
      /* If requested to call action directly, this is still scheduled, but with
       * a very short interval. Otherwise the normal interval is used. This ensures no
       * actions are taken in this thread, and that they are strictly sequential. */
      if forceFirst then renew(true,1.milli) else renew(true,interval)

  /**
   * Cancel the delayed actions. Allow the scheduled action to finish, if allowLast is true.
   * This is on best effort basis. Even with allowLast = false, an already initiated action
   * may start never the less. With  allowLast = true, the currently running timer is kept,
   * and may expire only once more. Note that the timers may or may not keep the application
   * running, depending on the platform. At application termination its best to manually
   * perform the action yourself if needed and call this with allowLast = false, to ensure
   * the quickest possible exit. */
  private[actors] def stop(allowLast: Boolean) = synchronized :
    /* Setting this to false prohibits the further scheduled executions of actions. Only
     * act if something actually changed. Note that it is essential this all happens
     * inside the synchronized environment. This ensures we are not inside an attempt
     * to reschedule a timer. Or we are before one, and then enabled will be false
     * in time, or we are after one, in which case cancellable will have the newly
     * defined value. We are also not inside start(). */
    if enable(false) && !allowLast then
      /* If so, we must try to cancel the timer, if we do not allow for a last action.
       * This cancellation may not succeed, and the action may still take place. However,
       * a new one will not be scheduled for certain. */
      cancellable.cancel()
      /* Signal there is no current timer running. */
      cancellable = Cancellable.empty

