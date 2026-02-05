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

import scala.concurrent.duration.FiniteDuration

/** Trait that dresses the Monitor with timer functions needed to make probes. */
transparent trait Probing(using context: ActorContext)  :

  /* A timer is used to schedule the regular probe actions. This is defined lazy for the
   * probeInterval will not yet be available at this part of the object construction. */
  private lazy val timer: Timer = Timer(probeInterval,probeNow)

  /**
   * Default probe interval. This is for the user to define. Set this to a reasonable value,
   * say 5 seconds for short running applications and maybe 1 minute for servers. */
  protected def probeInterval: FiniteDuration

  /** Method to do the actual probing of the actor. To be implemented by the user. */
  private[actors] protected def probeNow(): Unit

  /**
   * Start the probe actions. Set forceFirst to true, if you directly want the first probe to be
   * made. If the timer from the last active period is still running, no new timer is started and
   * forceFirst is ignored. */
  private[actors] protected def probeStart(forceFirst: Boolean): Unit = timer.start(forceFirst)

  /**
   * Cancel the delayed probe actions. Allow the scheduled probe to finish, if allowLast is true.
   * This is on best effort basis. Even with allowLast = false, an already initiated probe may start
   * nevertheless. With allowLast = true, the currently running timer is kept, and may expire only
   * once more. Note that the timers may or may not keep the application running, depending on the
   * platform. At application termination its best to manually perform the action on probeNow() if
   * needed and call this with allowLast = false, to ensure the quickest possible exit. */
  private[actors] protected def probeStop(allowLast: Boolean): Unit = timer.start(allowLast)
