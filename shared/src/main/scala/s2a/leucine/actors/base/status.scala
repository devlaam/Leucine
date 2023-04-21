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

/** The StatusActor contains the variable state of the actor */
transparent trait StatusActor(using context: ActorContext) extends UserActor :
  import Actor.{Activity, Stop}
  import BareActor.Phase

  /**
   * Holds all the envelops send to this actor in the mutable MQueue. Note that the Finish
   * letter is not on the queue but a separate state. This is because we cannot post Finish
   * on type E and when we make a union type we must type match on every letter being processed. */
  private[actors] val mailbox: BurstQueue[Env[?]] = new BurstQueue[Env[?]]

  /**
   * Variable that keeps track of the phase the actor is in. All actions on phase must be synchronized
   * since it may be reached from different threads at equal times. Synchronization is done at the place
   * of use for maximal efficiency */
  private[actors] var phase: Phase = Phase.Start

  /**
   * Variable that keeps track of the stop that has been requested. Some stops cannot be reverted, or
   * or only increased.  */
  private[actors] var stopper: Stop = Stop.Never

  /**
   * Variable that keeps track of the transient state between processes.
   * Note that although access on state may be from different threads, it is strictly sequential, so
   * there is no need to protect manipulations. */
  private[actors] var state: State = initialState

  /** Counter for the total number of exceptions during the lifetime of this actor. */
  private[actors] var excepts: Int = 0

  /**
   * Counter for the total number of silent periods (needle drops). Anytime the actor starts the
   * processing loop this is reset. It is periodically increased when in pause mode. */
  private[actors] var needles: Int = 0

  /** See the current activity state of this actor */
  def activity: Activity = synchronized {
    if phase.active
    then if stopper == Stop.Final then Activity.Haltable    else Activity.Running
    else if phase   == Phase.Done then Activity.Terminated  else Activity.Stopping }
