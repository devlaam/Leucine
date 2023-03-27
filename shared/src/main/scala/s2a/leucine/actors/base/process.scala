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

/** The ProcessActor implements process methods that convey this actor. */
transparent trait ProcessActor(using context: ActorContext) extends StatusActor :
  import BareActor.Phase

  /** Execute an action later on the context. */
  private[actors] def deferred(action: => Unit): Unit =
    val runnable = new Runnable { def run(): Unit = action }
    context.execute(runnable)

  /** Triggers the processLoop into execution, depending on the phase. */
  final private[actors] def processTrigger(coreTask: Boolean): Unit = synchronized {
    if context.actorTracing then println(s"In actor=$path: Trigger message, phase=${phase}")
    phase = phase match
      /* If this is the very first trigger, we must also call beforeStart() */
      case Phase.Start  => processPlay(coreTask); Phase.Play
      /* If we are already looping, nothing to do. */
      case Phase.Play   => Phase.Play
      /* If we were just taking a break, start the loop again. */
      case Phase.Pause  => processPlay(coreTask); Phase.Play
      /* The finish letter was received, we do not except triggers. Do nothing */
      case Phase.Finish => Phase.Finish
      /* We are already stopping and do not except triggers. Do nothing */
      case Phase.Stop   => Phase.Stop
      /* When we are done, we are done. */
      case Phase.Done   => Phase.Done }

  /** Call processPlay to continue the processLoop. */
  private[actors] def processPlay(reset: Boolean): Unit =
    if context.actorTracing then println(s"In actor=$path: processPlay() called in phase=${phase}")
    /* Reset the dropped needles counter (note we only call processPlay synchronised.) if
     * requested, otherwise make sure it does not exceed the silentStop value. */
    if reset then needles = 0 else needles = needles min context.silentStop
    /* Put the processLoop() on the context. */
    deferred(processLoop())

  /**
   * Tries to process the contents of one envelope. If there is an exception, this is delived to
   * the user. If this method is not implemented, the exception is only counted, and the processLoop
   * will advance to the next envelope.  */
  private[actors] def processEnveloppe(envelope: Env): Unit =
    /* Start measuring the time passed in the user environment, and trace when requested */
    monitorEnter(envelope)
    /* User code is protected by an exception guard.*/
    try
      /* Execute the receiver handler. */
      state = deliverEnveloppe(envelope,state)
      /* stashEnqueue checks if there was a stash request for this message. If so it is enqueued. */
      stashEnqueue(envelope)
    catch
      /* Normal exceptions may be handled by the user or ignored. They are counted as well. */
      case exception: Exception => excepts += 1; state = deliverException(envelope,state,exception,excepts)
      /* Runtime (and other) errors bubble up. */
      case error: Error         => throw error
    /* Make sure the clock is stopped, and trace when requested */
    finally monitorExit(envelope)

  /**
   * Primairy process loop. As soon as there are any letters, this loop runs
   * until all the letters are processed and the queues are exhausted. */
  private[actors] def processLoop(): Unit =
    if context.actorTracing then println(s"In actor=$path: enter processLoop() in phase=${phase}")
    /* List of envelopes to process. It starts with the mailbox, which is augmented with
     * destashed evenlopes if any and possibly one event. Since the eventsDequeue and the
     * mailbox.dequeue both need synchronization we do that on once. */
    var envs: List[Env] = synchronized { eventsDequeue(stashDequeue(mailbox.dequeue())) }
    /* Test if must report an empty mailbox */
    if envs.isEmpty then protectReset()
    /* Loop through all envelopes. We try to process as many as we can within this timeslice. */
    while !envs.isEmpty && synchronized { phase != Phase.Stop } do
      /* Process the first envelope in line. */
      processEnveloppe(envs.head)
      /* When done, see if we must augment the list with more important envelopes. If there are no
       * events possible then there is no need to synchronize. The stash is always filled within the
       * letter handling. */
      envs = if eventsPossible then synchronized { eventsDequeue(stashDequeue(envs.tail)) } else stashDequeue(envs.tail)
      /* In case we have mailbox protection we may have to take action, since the queue may have grown */
      protectAlarm()
    /* If we have any children that were removed report them now. This means we still receive callbacks on the removed
     * children after a Stop, but only as long as they were rejected when this actor was still active. Note that we
     * report outside the inner loop. These callbacks are not that important that we want to test for them every processed
     * letter. When the callback comes eventually, that is sufficient. */
    familyReport()
    /* The loop is done, we must exit. If this due to a stop, we may have dropped envelopes. */
    processExit(!envs.isEmpty)

  /**
   * Call processStop to terminate the processLoop. Dropped should contain the number
   * of letters that could not be completed due to a forced stop. If finish is true
   * the stop was not forced, but the current queue was allowed to be completed. */
  private[actors] def processStop(dropped: Boolean, finish: Boolean): Unit = synchronized {
    if context.actorTracing then println(s"In actor=$path: processStop() called in phase=${phase}")
    /* Stop all scheduled timers. */
    eventsCancel()
    /* Stop/finish the family tree recursively. */
    familyStop(finish)
    /* See if there is anything left that was not processed */
    val complete = !dropped && mailbox.isEmpty && stashEmpty
    /* Remove the remaining letters, if any. */
    mailbox.clear()
    /* Remove the remaining stashed letters, if any. */
    stashClear()
    /* Stop the monitoring of processed time. */
    monitorStop()
    /* If we have no family or no children any more we may directly terminate, otherwise
     * we must wait until the last child has terminated. */
    if familySize == 0 then deferred(processTerminate(complete)) else familyTerminate(complete) }

  /** Last goodbyes of this actor. */
  private[actors] def processTerminate(complete: Boolean): Unit =
    /* Call the stop event handler of this actor, if implemented. */
    stopped(stopper,complete)
    /* We must abandon after the stopped has called, so that we call all stopped in a family in
     * the correct order, since they may be executed in different threads. When all stopped
     * children of a family are done, the processTerminate of the parent is called.
     * When the actor is not a child (anymore) familyAbandon() returns false, in which case
     * we must remove it from the guard itself. */
    if !familyAbandon() then ActorGuard.remove(this)
    /* This actor is now Done. Note that, actors called by familyAbandon above may reach the
     * phase Done before this actor does. However, this actor has completed its actions before
     * the others have.  */
    synchronized { phase = Phase.Done }

  /** Afterwork from the processLoop. If dropped is true, there were letters that could not be completed. */
  private[actors] def processExit(dropped: Boolean): Unit = synchronized {
    if context.actorTracing then println(s"In actor=$path: exit processLoop() in phase=${phase}")
    /* There are regular (core) tasks that handle some envelopped message. There can stem from the
     * the mailbox, the stash or the eventqueue. These are all handled in normal operation and when
     * we are finishing, the eventqueue is ignored. So we first calculate these coreFinishTasks */
    val coreFinishTasks = stashFlush      || !mailbox.isEmpty
    /* and then the corePlayTasks are found by adding the events if present. */
    val corePlayTasks   = coreFinishTasks || eventsPresent
    /* Appart from the core tasks there are reports (callbacks) on situations that occur. We report
     * a child was removed or if the mailbox is empty again. These are called report tasks. */
    val reportTasks     = !protectIdle    || familyRemoved
    /* So we start play again when we are in Finish mode if there are coreFinishTasks or reportTasks.
     * (Note that with Stop.Direct the reportTasks are skipped as well) */
    val withFinishTasks = coreFinishTasks || reportTasks
    /* And we start play again in regular operation when there are any of the core or play tasks. */
    val withPlayTasks   = corePlayTasks   || reportTasks
    /* See what has changed in the meantime and how to proceed. */
    phase = phase match
      /* This situation cannot occur, phase should be advanced before loop is started */
      case Phase.Start  => assert(false, "Unexpected Phase.Start in processLoop"); Phase.Done
      /* If there are play tasks, start the loop again, and only fully reset the needles on the core tasks. */
      case Phase.Play   => if withPlayTasks then { processPlay(corePlayTasks); Phase.Play } else Phase.Pause
      /* This situation cannot occur, a running loop may not be paused. */
      case Phase.Pause  => assert(false, "Unexpected Phase.Pause in processLoop"); Phase.Done
      /* If there are finish tasks, start the loop again, and reset does not realy matter, since needle dropping has stopped. */
      case Phase.Finish => if withFinishTasks then  { processPlay(coreFinishTasks); Phase.Finish } else { processStop(dropped,true); Phase.Stop }
      /* If we got an exteral stop request, make an end to this. */
      case Phase.Stop   => processStop(dropped,false); Phase.Stop
      /* This situation cannot occur, during loop phase cannot proceed to Phase.Done */
      case Phase.Done   => assert(false, "Unexpected Phase.Done in processLoop"); Phase.Done }
