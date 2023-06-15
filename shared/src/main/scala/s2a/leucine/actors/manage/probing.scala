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

/** Trait that dresses the Monitor with timer functions needed to make probes. */
private transparent trait Probing(using context: ActorContext)  :

  /* Variable that keeps the state of taking probes in the actor. If true new probes are
   * scheduled on regular intervals. If false, new probes are not scheduled any more. */
  private var enabled: Boolean = false

  /** Variable holding the cancel object for probing the actor. */
  private var cancelProbe = Cancellable.empty

  /** Separate object to synchronize enabled */
  private val guard: Object = new Object

  /** Set new enabled state, return true if anything changed. */
  private def enable(state: Boolean): Boolean =
    if enabled == state then false else
      enabled = state
      true

  /** Method to do the actual probing of the actor. To be implemented by the user. */
  private[actors] protected def probeNow(): Unit

  /** Method to create a new schedulable object to probe the actor. */
  private def callProbe = new Callable[Unit] :
    /* The executed method when the timer expires is call() */
    def call(): Unit =
      /* First make the probe */
      probeNow()
      /* If still enabled, reschedule a new timer, if not, clear the cancelProbe.
       * Synchronize this in order to have a well defined cancelProbe value. */
      guard.synchronized(probeRenew(enabled,probeInterval))

  /** Call this to restart the probing, if we are still probing. */
  private def probeRenew(act: Boolean, delay: FiniteDuration): Unit =
    /* Schedule the probing to take place over a fixed amount of time. If act is false,
     * clean the cancelProbe variable to signal there is no current timer running. */
    cancelProbe = if act then context.schedule(callProbe,delay) else Cancellable.empty

  /**
   * Start the probe actions. Set forceFirst to true, if you directly want
   * the first probe to be made. If the timer from the last active period
   * is still running, no new timer is started and forceFirst is ignored. */
  private[actors] def probeStart(forceFirst: Boolean) = guard.synchronized {
    /* The enabled flag must be set to continue scheduling late probes. Only
     * act if something actually changed. If however the timer is still active
     * possible after a probeStop(allowLast(true) we do not start a new timer.
     * Note that it is essential this all happens inside the synchronized environment.
     * This ensures we are not inside an probeRenew, so cancelProbe will have the
     * last defined value. We are also not inside probeStop(). */
    if enable(true) && cancelProbe.isEmpty then
      /* If requested to make a probe directly, this is still scheduled, but with
       * a very short interval. Otherwise the normal probeInterval is used. This
       * ensures no probes are taken in this thread, and that they are strictly
       * sequential. */
      if forceFirst then probeRenew(true,1.milli) else probeRenew(true,probeInterval) }

  /**
   * Cancel the delayed probe actions. Allow the scheduled probe to finish, if
   * allowLast is true. This is on best effort basis. Even with allowLast = false,
   * an already initiated probe may start never the less. With  allowLast = true,
   * the currently running timer is kept, and may expire only once more. */
  private[actors] def probeStop(allowLast: Boolean) = guard.synchronized {
    /* Setting this to false prohibits the further scheduled executions of probes. Only
     * act if something actually changed. Note that it is essential this all happens
     * inside the synchronized environment. This ensures we are not inside an attempt
     * to reschedule a timer. Or we are before one, and then enabled will be false
     * in time, or we are after one, in which case cancelProbe will have the newly
     * defined value. We are also not inside probeStart(). */
    if enable(false) && !allowLast then
      /* If so, we must try to cancel the timer, if we do not allow for a last probe.
       * This cancellation may not succeed, and the probe may still take place. However,
       * a new one will not be scheduled for certain. */
      cancelProbe.cancel()
      /* Signal there is no current timer running. */
      cancelProbe = Cancellable.empty }

  /**
   * Default probe interval. This is for the user to define. Set this to a reasonable value,
   * say 5 seconds for short running applications and maybe 1 minute for servers. */
  protected def probeInterval: FiniteDuration

