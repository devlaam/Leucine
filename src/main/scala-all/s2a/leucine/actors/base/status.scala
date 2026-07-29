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
private transparent trait StatusActor extends UserActor :
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
   * of use for maximal efficiency. */
  private[actors] var phase: Phase = Phase.Start

  /**
   * Variable that keeps track of the stop that has been requested. Some stops cannot be reverted, or
   * or only increased. */
  private[actors] var stopper: Stop = Stop.Never

  /** Counter for the total number of exceptions during the lifetime of this actor. */
  private[actors] var excepts: Int = 0

  /** Counter for the total number of failed (unhandled) messages during the lifetime of this actor. */
  private[actors] var failed: Int = 0

  /**
   * Counter for the total number of silent periods (needle drops). Anytime the actor starts the
   * processing loop this is reset. It is periodically increased when in pause mode. */
  private[actors] var needles: Int = 0

  /** See the current activity state of this actor */
  def activity: Activity = synchronized :
    /* In a very rare case it may come to pass that the activity is probed by the guard/monitor before the
     * object construction is completed. This is because these have independent timers and the newly
     * constructing actor is registered somewhere in the process, not necessarily at the end. In that
     * case the phase value has not yet been initialized. We then report the activity as Starting. */
    if phase == null then Activity.Starting
    /* In case the phase is already set, we consider an actor Haltable when the system may stop it
     * in a non trivial manner, i.e. a Final actor is stopped at the end of the whole actor systems
     * lifetime, but this does not count as Haltable, other values of the stopper do. If the system
     * is actively stopping an actor, this is considered Stopping. At the end it is Terminated. */
    // TODO: We hebben de betekenis van Haltable veranderd, maar dat gaat fout in the Guard.
    // Haltable was alleen geldig indien stopper == Stop.Final.
    // Ga even terug naar de oude definitie.
    else phase match
      // case Phase.Start  => if stopper >= Stop.Final then Activity.Starting else Activity.Haltable
      // case Phase.Play   => if stopper >= Stop.Final then Activity.Running  else Activity.Haltable
      // case Phase.Pause  => if stopper >= Stop.Final then Activity.Running  else Activity.Haltable
      case Phase.Start  => if stopper != Stop.Final then Activity.Starting else Activity.Haltable
      case Phase.Play   => if stopper != Stop.Final then Activity.Running  else Activity.Haltable
      case Phase.Pause  => if stopper != Stop.Final then Activity.Running  else Activity.Haltable
      case Phase.Finish => Activity.Stopping
      case Phase.Stop   => Activity.Stopping
      case Phase.Done   => Activity.Terminated

