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


/** The BareActor implements all methods needed for basic actor operation. It should not be instantiated by the user. */
abstract class BareActor(using context: ActorContext) extends Actor, ActorDefs:
  import BareActor.Phase

  if context.trace then println(s"In actor=$path: Constructed")

  /** Use this inside the actor to test for an anonymous sender */
  type Anonymous = Actor.Anonymous.type

  /* Pack the letter with the sender. Here the sender is ignored. */
  private[actors] def pack(letter: MyLetter, sender: Sender): Env

  /**
   * Holds all the envelops send to this actor in the mutable MQueue. Note that the Finish
   * letter is not on the queueu but a separate state. This is because we cannot post Finish
   * on type E and when we make a union type we must type match on every letter being processed. */
  private val mailbox: BurstQueue[Env] = new BurstQueue[Env]

  /**
   * Variable that keeps track of the phase the actor is in. All actions on phase must be synchronized
   * since it may be reached from different threads at equal times. Synchronization is done at the place
   * of use for maximal efficientcy */
  private var phase: Phase = Phase.Start

  /**
   * Variable that keeps track of the transient state between processes.
   * Note that allthough access on state may be from different threads, it is strictly sequential, so
   * there is no need to protect manipulations. */
  private var state: ActState = initialState

  /** Counter for the total number of exceptions during the lifetime of this actor. */
  private var excepts: Int = 0

  /** See if this actor is still active. */
  def isActive: Boolean = synchronized { phase.active }

  /** See if this actor is completely terminated. */
  def isTerminated: Boolean = synchronized { phase == Phase.Done }

  /**
   * Generates an unique name of the structure ClassName#Hash. This can be used instead of
   * self invented names. It is given inside the actor constructor.  */
  protected def uniqueName: String =
    val hash: Long = ##.toLong & 0xFFFFFFFFL
    s"${getClass.getSimpleName}#$hash"

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeBare(): Option[MonitorActor.Bare] =
    val result = MonitorActor.Bare(phase,mailbox.sum,mailbox.max,excepts,userLoad)
    mailbox.reset()
    Some(result)

  /** Execute an action later on the context. */
  private[actors] def deferred(action: => Unit): Unit =
    val runnable = new Runnable { def run(): Unit = action }
    context.execute(runnable)

  /** Call processPlay to continue the processLoop. */
  private def processPlay(): Unit =
    if context.trace then println(s"In actor=$path: processPlay() called in phase=${phase}")
    deferred(processLoop())

  /**
   * Call processStop to terminate the processLoop. Dropped should contain the number
   * of letters that could not be completed due to a forced stop. If finish is true
   * the stop was not forced, but the current queue was allowed to be completed. */
  private def processStop(dropped: Boolean, finish: Boolean): Unit = synchronized {
    if context.trace then println(s"In actor=$path: processStop() called in phase=${phase}")
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
    stopped(complete)
    /* We must abandon after the stopped has called, so that we call all stopped in a family in
     * the correct order, since they may be executed in different threads. When all stopped
     * of the children of a family are done, the processTerminate of the parent is called. */
    familyAbandon(name)
    /* This actor is now Done. Note that, actors called by familyAbandon above may reach the
     * phase Done before this actor does. However, this actor has completed its actions before
     * the others have.  */
    synchronized { phase = Phase.Done }

  /**
   * Tries to process the contents of one envelope. If there is an exception, this is delived to
   * the user. If this method is not implemented, the exception is only counted, and the processLoop
   * will advance to the next envelope.  */
  private def processEnveloppe(envelope: Env): Unit =
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
  private def processLoop(): Unit =
    if context.trace then println(s"In actor=$path: enter processLoop() in phase=${phase}")
    /* List of envelopes to process. It starts with the mailbox, which is augmented with
     * destashed evenlopes if any and possibly one event. Since the eventsDequeue and the
     * mailbox.dequeue both need synchronization we do that on once. */
    var envs: List[Env] = synchronized { eventsDequeue(stashDequeue(mailbox.dequeue())) }
    /* Loop through all envelopes. We try to process as many as we can within this timeslice. */
    while !envs.isEmpty && synchronized { phase != Phase.Stop } do
      /* Process the first envelope in line. */
      processEnveloppe(envs.head)
      /* When done, see if we must augment the list with more important envelopes. If there are no
       * events possible then there is no need to synchronize. The stash is always filled within the
       * letter handling. */
      envs = if eventsPossible then synchronized { eventsDequeue(stashDequeue(envs.tail)) } else stashDequeue(envs.tail)
    /* The loop is done, we must exit. If this due to a stop, we may have dropped envelopes. */
    processExit(!envs.isEmpty)

  /** Afterwork from the processLoop. If dropped is true, there were letters that could not be completed. */
  private def processExit(dropped: Boolean): Unit = synchronized {
    if context.trace then println(s"In actor=$path: exit processLoop() in phase=${phase}")
    /* See what has changed in the meantime and how to proceed. */
    phase = phase match
      /* This situation cannot occur, phase should be advanced before loop is started */
      case Phase.Start  => assert(false, "Unexpected Phase.Start in processLoop"); Phase.Done
      /* If there are no more letters on the queue or stashed, wait for new ones, otherwise continue */
      case Phase.Play   => if !eventsPresent && !stashFlush && mailbox.isEmpty then Phase.Pause else { processPlay(); Phase.Play }
      /* This situation cannot occur, a running loop may not be paused. */
      case Phase.Pause  => assert(false, "Unexpected Phase.Pause in processLoop"); Phase.Done
      /* If we got an Finish letter, we must complete the queue or stop.  */
      case Phase.Finish => if mailbox.isEmpty then { processStop(dropped,true); Phase.Stop } else { processPlay(); Phase.Finish }
      /* If we got an exteral stop request, make an end to this. */
      case Phase.Stop   => processStop(dropped,false); Phase.Stop
      /* This situation cannot occur, during loop phase cannot proceed to Phase.Done */
      case Phase.Done   => assert(false, "Unexpected Phase.Done in processLoop"); Phase.Done }


  /** Triggers the processLoop into execution, depending on the phase. */
  final private[actors] def processTrigger(): Unit = synchronized {
    if context.trace then println(s"In actor=$path: Trigger message, phase=${phase}")
    phase = phase match
      /* If this is the very first trigger, we must also call beforeStart() */
      case Phase.Start  => processPlay(); Phase.Play
      /* If we are already looping, nothing to do. */
      case Phase.Play   => Phase.Play
      /* If we were just taking a break, start the loop again. */
      case Phase.Pause  => processPlay(); Phase.Play
      /* The finish letter was received, we do not except triggers. Do nothing */
      case Phase.Finish => Phase.Finish
      /* We are already stopping and do not except triggers. Do nothing */
      case Phase.Stop   => Phase.Stop
      /* When we are done, we are done. */
      case Phase.Done   => Phase.Done }

  /**
   * This calls an implementation by the user. It typically holds a handler that acts according the content of the letter.
   * If you want to work with actor states, override this receive method. Make sure your state is completely immutable. */
  private[actors] def deliverEnveloppe(envelope: Env, state: ActState): ActState

  /**
   * This calls an implementation by the user. The default implementation is to ignore the exception and pass on to the
   * next letter. Errors are not caught and blubble up. Now, this follows the Java style. */
  private[actors] def deliverException(envelope: Env, state: ActState, exception: Exception, exceptionCounter: Int): ActState

  /**
   * This defines the initial state that is used before the first letter is processed if needed. The related definition must
   * be in the actor constructor of the user code. */
  private[actors] def initialState: ActState

  /**
   * Called before actor deactivation and guaranteed after the last message is processed. If there were any
   * unprocessed letters in this actor at teardown, complete is false. These could be in the normal mailbox
   * or on the stash, if present.
   * In case of a actorContext shutdown this is NOT called, for this disruptly terminates the processing loops.
   * It is however called when stopDirect() or stopFinish() are used. The actor may still be around
   * after this method is called, but will never accept new messages. The parent is still defined,
   * when stopped() is executed (but may already stopped processing messages) but all the childeren
   * will already be removed from the list, and their stopped() methods have already been called. */
  protected def stopped(complete: Boolean): Unit = ()

  /**
   * A letter is send to this actor directly by an other actor. Returns if the letter was accepted
   * for delivery. Note, this does not mean it also processed. In the mean time the actor may stop. */
  final private[actors] def sendEnvelope(envelope: Env): Boolean = synchronized {
    if context.trace then println(s"In actor=$path: Enqueue message $envelope, phase=${phase}")
    /* Trace if we have to, we accepted (active) or refused (inactive) the letter processing */
    monitorSend(phase.active,envelope)
    /* See if we may accept the letter, if so, enqueue it and trigger the processLoop. */
    if !phase.active then false else
      mailbox.enqueue(envelope)
      processTrigger()
      true }

  /** Stop this actor asap, but complete the running letter. */
  private[actors] def stopWith(finish: Boolean): Unit = synchronized {
    if context.trace then println(s"In actor=$path: Before stopInternal($finish) message, phase=${phase}")
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

  /** Stop this actor asap, but complete the running letter. Terminate afterwards. */
  final def stopDirect(): Unit = stopWith(false)

  /** The mailbox is processed, but no more letters are accepted. Terminate afterwards. */
  final def stopFinish(): Unit = stopWith(true)

  /** In the base actor the path and name are equal. */
  def path: String = name

  /** Used as sender for all messages send from this actor without explicit sender. */
  given this.type = this


object BareActor :

  /* The actor passes different phases in its lifetime; not called states, for these are external to the handling.
   * After construction the phase is Start. When the first message comes in, it calls beforeStart and advances to Play.
   * From there it may oscillate between Play and Pause. The phase is Pause when the message queue is empty and Play
   * as long as there are letters on the queue. If a Finish letter arrives while the loop is running, the phase moves
   * to Finish, and the current queue is fisished. If stopDirect() is called, the phase advances to Stop, which terminates
   * the loop asap. Subsequently afterStop() is called, and the pahse becomes Done. It may never be reactivated again.
   * The first three phases are active (so the actor may accept letters, the last ones are not, and describe the state
   * in various ways of tearing down. */
  private[actors]  enum Phase(val active: Boolean) :
    /** The first phase the actor is in after creation. */
    case Start  extends Phase(true)
    /** The active phase when the actor is processing a letter. */
    case Play   extends Phase(true)
    /** A passive phase were the actor waits for new letters to process. */
    case Pause  extends Phase(true)
    /** A phase in which the actor will terminate the current queue and then stop. */
    case Finish extends Phase(false)
    /** A phase in which the actor will terminate the current letter and then stop. */
    case Stop   extends Phase(false)
    /** The last phase were the actor will not accept any new letters. */
    case Done   extends Phase(false)

  /** The Envelope is responsible for holding the letter and the sender together. */
  private[actors] class Envelope[L,S](val letter: L, val sender: S)


