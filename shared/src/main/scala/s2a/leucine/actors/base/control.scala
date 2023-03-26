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


/** The ControlActor implements the methods that may change the behaviour of the actor. */
transparent trait ControlActor(using context: ActorContext) extends ProcessActor :
  import Actor.Stop
  import BareActor.Phase

  /**
   * Called from the guard to drop a needle. If the number of needles exceeds a threshold,
   * the actor is assumed to be silent. */
  private[actors] def dropNeedle(root: Boolean): Unit =
    /* Dropping needles only makes sense when we are active. */
    val passOn =  synchronized { activity.active && (
      /* See if we are not double booked. This is the case when this actor is requested to
       * stop on Silent and one of its ancesters is as well. In that case we remove this booking
       * and ignore the signal. */
      if (stopper == Stop.Silent) && !root then
        /* Remove the booking */
        ActorGuard.dropNeedles(false,this)
        /* Ignore this Silent stopper. */
        stopper = Stop.Never
        /* Do not pass the signal on. */
        false
      /* See if we still want the actor to stop on Silent. This is the case when we received
       * the request ourselves, or when it is passed from the parent in a family. */
      else if (stopper == Stop.Silent) || !root then
        /* See if we are indeed doing nothing, in that case drop needle. Note: this test
         * is essential. Allthough the needles are cleared at every processLoop, this only
         * happens at the start, on a very busy actor, clearing may be selldom. */
        if (phase == Phase.Start) || (phase == Phase.Pause) then needles = needles + 1
        /* If the limit of needles is reached, the actor is considered silent. We may now
         * stop if there are no children any more. If there still are, we must wait until
         * these have stopped. */
        if (needles > context.silentStop) && (familySize == 0) then
          /* We want to report Silent as stop cause, also when the needle came from the parent. */
          stopper = Stop.Silent
          /* The actual stop ritual is by finish. */
          stopWith(true)
        /* Pass the signal on. */
        true
      /* In the other cases there is nothing to do or pass. */
      else false )}
    /* Pass the needle on to the children, since the stop request considers the whole tree.
     * Note: this must be done outside the synchronization to prevent possible deadlocks. This
     * may imply we miss an actor that was newly registered. That is no problem. A silent stop
     * comes eventually. */
    if passOn then familyDropNeedle()

  /**
   * A letter is send to this actor directly by an other actor. Returns if the letter was accepted
   * for delivery. Note, this does not mean it also processed. In the mean time the actor may have stopped.
   * A letter is accepted if the actor is still active and if there is room in the mailbox to store it. */
  final private[actors] def sendEnvelope(envelope: Env): Boolean = synchronized {
    if context.actorTracing then println(s"In actor=$path: Enqueue message $envelope, phase=${phase}")
    /* A letter is only accepted as long as we are active and the mailbox is not too full. */
    val accept = phase.active && mailbox.size < maxMailboxSize
    /* Trace if we have to, we accepted or refused the letter processing */
    monitorSend(accept,envelope)
    /* If the message is not accepted say so, else do ... */
    if !accept then false else
      /* ... test the current mailbox size for the high level water mark. */
      protectRaise(mailbox.size)
      /* ... put the mail in the box */
      mailbox.enqueue(envelope)
      /* ... trigger the processLoop, so execution starts, if currenly in Pause. */
      processTrigger(true)
      true }

  /**
   * Stop this actor asap, but complete the running letter if finish is true. This is an internal call
   * which does not change the stopper value, which holds the primary user action to stop the actor */
  private[actors] def stopWith(finish: Boolean): Unit = synchronized {
    if context.actorTracing then println(s"In actor=$path: Before stopInternal($finish) message, phase=${phase}")
    phase = phase match
      /* When we did not yet start, but the party is already over, nothing to do. */
      case Phase.Start  => deferred(processStop(false,finish)); Phase.Stop
      /* If we are already looping, proceed to stop, which will halt the looping after the
       * letter is done or finish and let the mailbox complete. */
      case Phase.Play   => if finish then Phase.Finish else Phase.Stop
      /* If we we were just taking a break, we can stop immediately. */
      case Phase.Pause  => deferred(processStop(false,finish)); Phase.Stop
      /* The finish command was already given but we may want to stop even quicker */
      case Phase.Finish => if finish then Phase.Finish else Phase.Stop
      /* Repeated call to stop has no effect. */
      case Phase.Stop   => Phase.Stop
      /* When we are done, we are done. */
      case Phase.Done   => Phase.Done }

  /**
   * Stopping of an actor is organised in levels of sevirity. The lowest level (Direct) terminates directly, the
   * highest level never terminates. The latter is the default. Levels can always be decreased, increase is only
   * possible if the stop action was not yet started. Direct and Finish start immediately, and cannot be retracted. */
  final def stop(value: Stop): Unit =
    /* Activate/deactivate ther needle dropping, which is there to see if the actor turned silent. */
    def drop(state: Boolean): Unit = ActorGuard.dropNeedles(state,this);
    /* Stop with finish if there are no childeren for this actor. */
    def stopChildless(): Unit = if familySize == 0 then stopWith(true)
    /* All changes to the stopper are protected by a test on the current phase. There can only be one
     * cause to stop the actor. Once the teardown process has started, this cannot be changed anymore.
     * This is important for the user can call stop from different threads an the parent actor sometimes
     * tries to stop the child twice. The second time will be ignored. Furthermore, the stop level cannot
     * be increased in most cases. */
    synchronized { stopper = value match
      /* A request to stop the actor right now is always honoured, unless the actor is allready stopping.
       * It is allowed to overwrite a Stop.Finish here to accelerate the stopping process. */
      case Stop.Direct  if (stopper >  Stop.Direct) && (phase < Phase.Stop)   => stopWith(false); Stop.Direct
      /* A request to stop the actor and finish completion of the mailbox is honoured when we are not already doing this.
       * Note that, due to the nested synchronization calls, the situation where (phase >= Phase.Finish) after
       * stopper >  Stop.Finish should not appear. But for consistency and protection this test is kept in. */
      case Stop.Finish  if (stopper >  Stop.Finish) && (phase < Phase.Finish) => stopWith(true);  Stop.Finish
      /* A stop on Barren (ie stop wheren there are no children, equals a request to finish when there are no children
       * right now, and otherwise the actor waits until this situation occours. If we were waiting on silence before
       * this is deleted, since the silence test also waits on the disappearance of the childeren, but Barren is stronger. */
      case Stop.Barren  if (stopper == Stop.Silent) && (phase < Phase.Finish) => drop(false); stopChildless(); Stop.Barren
      /* In case we were not waiting on Silent before, we can directly test or wait. */
      case Stop.Barren  if (stopper >  Stop.Silent) && (phase < Phase.Finish) => stopChildless(); Stop.Barren
      /* If we want to stop on Silent (but were not before, make sure to start the needle dropping.  */
      case Stop.Silent  if (stopper >  Stop.Silent) && (phase < Phase.Finish) => drop(true);  Stop.Silent
      /* Setting Stop to Final when we were on Barren is a simple reset, so the Barren condition is not tested any more. */
      case Stop.Final   if (stopper == Stop.Barren) && (phase < Phase.Finish) => Stop.Final
      /*  Setting Stop to Final when we were on Silent requires removing the actor from needle dropping. */
      case Stop.Final   if (stopper == Stop.Silent) && (phase < Phase.Finish) => drop(false); Stop.Final
      /* In other cases we just set the stopper to Final, which is probed by the guard. The guard decides if the actor
       * must stop. When the time has come, he will stop the actors by the internal stop call. */
      case Stop.Final   if (stopper >  Stop.Final ) && (phase < Phase.Finish) => Stop.Final
      /* Just reset the stopper to prohibit stopping, but only when the teardown did not yet start. */
      case Stop.Never   if (stopper == Stop.Barren) && (phase < Phase.Finish) => Stop.Never
      /* Cancel needle dropping to prohibit stopping, but only when the teardown did not yet start. */
      case Stop.Never   if (stopper == Stop.Silent) && (phase < Phase.Finish) => drop(false); Stop.Never
      /* In all other cases (only Final) reset the stopper. */
      case Stop.Never   if (stopper >  Stop.Silent) && (phase < Phase.Finish) => Stop.Never
      /* In all other situations the call cannot be honoured and nothing is changed. */
      case _            => stopper }
