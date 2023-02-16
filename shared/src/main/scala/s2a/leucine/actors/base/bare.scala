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
abstract class BareActor[ML <: Actor.Letter, AS <: Actor.State](using context: ActorContext) extends Actor[ML], ActorDefs:
  import BareActor.Phase

  /* MyLetter and ActState implement the bounded types for every mixin. */

  /** This is the base type for all letters that this actor can receive. */
  private[actors] type MyLetter = ML

  /** This is the base type for all states that this actor can be in. */
  private[actors] type ActState = AS

  /** This is the envelope type. It may be just a letter or letter+sender. */
  private[actors] type Env

  /** Use this inside the actor to test for an anonymous sender */
  type Anonymous = Actor.Anonymous.type

  /** Actor dependend packing of letter and sender into one enveloppe. */
  private[actors] def pack(letter: MyLetter, sender: Sender): Env

  /**
   * Holds all the envelops send to this actor in the mutable MQueue. Note that the Finish
   * letter is not on the queueu but a separate state. This is because we cannot post Finish
   * on type E and when we make a union type we must type match on every letter being processed. */
  private val envelopes: BurstQueue[Env] = new BurstQueue[Env]

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

  /**
   * See if this actor is still active. Once it cannot longer process any letters this turns false.
   * This is before stopped() is called. Cleanup work my still be ongoing. Note that in an asynchronous
   * system, the answer may already have changed after the read. Once it turns to false however, it will
   * never return to true again. */
  def isActive: Boolean = phase.active

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeBare(): Option[MonitorActor.Bare] =
    val result = MonitorActor.Bare(phase,envelopes.sum,envelopes.max,excepts,userLoad)
    envelopes.reset()
    Some(result)

  /** Execute an action later on the context. */
  private def deferred(action: => Unit) =
    val runnable = new Runnable { def run(): Unit = action }
    context.execute(runnable)

  /** Call processPlay to continue the processLoop. */
  private def processPlay(): Unit =
    if context.trace then println(s"In actor=$name: processPlay() called in phase=${phase}")
    deferred(processLoop())

  /** Call processStop to terminate the processLoop. */
  private def processStop(finish: Boolean): Unit = synchronized {
    if context.trace then println(s"In actor=$name: processStop() called in phase=${phase}")
    /* Stop all scheduled timers. */
    eventsCancel()
    /* In case we have family and stopped by a finish letter, pass them on, otherwise directly stop them. */
    if finish then familyFinish() else familyStop()
    /* After a stop, we can remove the remaining letters, if any. */
    envelopes.clear()
    /* Stop the monitoring of processed time. */
    monitorStop()
    /* Inform the user this actor is about terminate, and after that remove me from the parents list. */
    deferred(processTerminate()) }

  /** Last goodbyes of this actor. */
  private def processTerminate(): Unit =
    stopped()
    familyAbandon(name)

  /**
   * Primairy process loop. As soon as there are any letters, this loop runs
   * until all the letters are processed and the queue is exhausted. If there
   * is an exception, the next letter is processed without any cleaning up. */
  private def processLoop(): Unit =
    if context.trace then println(s"In actor=$name: enter processLoop() in phase=${phase}")
    /* Get the letters+senders as quickly as possible, release the queue. The stashQueue, when
     * present, may only be manipulated from within the actor, which is not running when we are
     * here. So we do not need to guard this dequeue call. But first we must see if there are
     * any events that need to be processed, and those need protection, just as the envelopes do. */
    val envs = synchronized {
      if      eventsPresent  then eventsDequeue
      else if stashFlush     then stashDequeue
      else                        envelopes.dequeue }
    if context.trace then println(s"In actor=$name: Process queue with ${envs.size} letters in thread ${Thread.currentThread().getName()}")
    /* Process the letters one by one, if the actor stops being active, all further letters are ignored. The loop runs to the end.*/
    for
      /* Get a envelope (letter with sender if present) from the copied queueu */
      envelope <- envs
      /* Test if we may continue, this is the case if the state is Phase.Play and receive the Phase.Stop. We must skip all further
       * letters (we could also break the loop, but this is an exception in Scala. Just ignoring a few letters is most likely quicker.
       * The other phases do not interrupt the loop or are not possible. If we got a stashFlush we also break the loop so that we can
       * first handle the stashed messages.  */
      if !stashFlush && synchronized{!eventsPresent && (phase != Phase.Stop)}
    do
      /* Start measuring the time passed in the user environment */
      monitorEnter()
      /* User code is protected by an exception guard.*/
      try
        /* Execute the receiver handler. */
        state = processEnveloppe(envelope,state)
        /* stashEnqueue checks if there was a stash request for this message. If so it is enqueued. */
        stashEnqueue(envelope)
      catch
        /* Normal exceptions may be handled by the user or ignored. They are counted as well. */
        case exception: Exception => excepts += 1; state = processException(envelope,state,exception,excepts)
        /* Runtime (and other) errors bubble up. */
        case error: Error         => throw error
      /* Make sure the clock is stopped. */
      finally monitorExit()
    /* The loop is done, we may exit and reevaluate what to do next. */
    processExit()

  /** Afterwork from the processLoop.  */
  private def processExit(): Unit = synchronized {
    if context.trace then println(s"In actor=$name: exit processLoop() in phase=${phase}")
    /* See what has changed in the meantime and how to proceed. */
    phase = phase match
      /* This situation cannot occur, phase should be advanced before loop is started */
      case Phase.Start  => assert(false, "Unexpected Phase.Start in processLoop"); Phase.Done
      /* If there are no more letters on the queue or stashed, wait for new ones, otherwise continue */
      case Phase.Play   => if !eventsPresent && !stashFlush && envelopes.isEmpty then Phase.Pause else { processPlay(); Phase.Play }
      /* This situation cannot occur, a running loop may not be paused. */
      case Phase.Pause  => assert(false, "Unexpected Phase.Pause in processLoop"); Phase.Done
      /* If we got an Finish letter, we must complete the queue or stop.  */
      case Phase.Finish => if envelopes.isEmpty then { processStop(true); Phase.Done } else { processPlay(); Phase.Finish }
      /* If we got an exteral stop request, make an end to this. */
      case Phase.Stop   => processStop(false); Phase.Done
      /* This situation cannot occur, during loop phase cannot proceed to Phase.Done */
      case Phase.Done   => assert(false, "Unexpected Phase.Done in processLoop"); Phase.Done }


  /** Triggers the processLoop into execution, depending on the phase. */
  final private[actors] def processTrigger(): Unit = synchronized {
    if context.trace then println(s"In actor=$name: Trigger message, phase=${phase}")
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
  private[actors] def processEnveloppe(envelope: Env, state: ActState): ActState

  /**
   * This calls an implementation by the user. The default implementation is to ignore the exception and pass on to the
   * next letter. Errors are not caught and blubble up. Now, this follows the Java style. */
  private[actors] def processException(envelope: Env, state: ActState, exception: Exception, exceptionCounter: Int): ActState

  /**
   * This defines the initial state that is used before the first letter is processed if needed. The related definition must
   * be in the actor constructor of the user code. */
  private[actors] def initialState: ActState

  /**
   * Called before actor deactivation and guaranteed after the last message is processed.
   * In case of a actorContext shutdown this is NOT called, for this disruptly terminates the processing loops.
   * It is however called when stopDirect() or stopFinish() are used. The actor may still be around
   * after this method is called, but will never accept new messages. The parent is still defined,
   * when stopped() is executed (but may already stopped processing messages) but all the childeren
   * will already be removed from the list. They where requested or forced to stop, but may not already
   * have actually done so. */
   // TODO: add receivedLetters and remainingLetters (do not forget stashed)
  protected def stopped(): Unit = ()

  /**
   * A letter is send to this actor directly by an other actor. Returns if the letter was accepted
   * for delivery. Note, this does not mean it also processed. In the mean time the actor may stop. */
  final private[actors] def sendEnvelope(envelope: Env): Boolean = synchronized {
    if context.trace then println(s"In actor=$name: Enqueue message $envelope, phase=${phase}")
    /* See if we may accept the letter, if so, enqueue it and trigger the processLoop. */
    if !phase.active then false else
      envelopes.enqueue(envelope)
      processTrigger()
      true }

  /** Stop this actor asap, but complete the running letter. */
  final def stopDirect(): Unit = synchronized {
    if context.trace then println(s"In actor=$name: Before stopDirect message, phase=${phase}")
    phase = phase match
      /* When we did not yet start, but the party is already over, nothing to do. */
      case Phase.Start  => deferred(processStop(true)); Phase.Done
      /* If we are already looping, proceed to stop, which will halt the looping after the letter is done */
      case Phase.Play   => Phase.Stop
      /* If we we were just taking a break, we can stop immediately. */
      case Phase.Pause  => deferred(processStop(false)); Phase.Done
      /* The finish letter was received, but we want to stop even quicker */
      case Phase.Finish => Phase.Stop
      /* Repeated call to stopDirect has no effect. */
      case Phase.Stop   => Phase.Stop
      /* When we are done, we are done. */
      case Phase.Done   => Phase.Done }

  /** The running queue is processed, but no more letters are accepted. Terminate afterwards. */
  final def stopFinish(): Unit = synchronized {
    if context.trace then println(s"In actor=$name: Terminate message, phase=${phase}")
    phase = phase match
      /* If the action is to finish, this will be the only action performed. */
      case Phase.Start  => deferred(processStop(false)); Phase.Done
      /* If the loop is running, set the phase to Phase.Finish, so it may complete but not restart. */
      case Phase.Play   => Phase.Finish
      /* If we were pausing at the moment, there are no messages, we can directly stop. */
      case Phase.Pause  => deferred(processStop(true)); Phase.Done
      /* An other finish letter was already received, we do not except new letters. Do nothing */
      case Phase.Finish => Phase.Finish
      /* We are already stopping and do not except new letters, also no finish letters. Do nothing */
      case Phase.Stop   => Phase.Stop
      /* When we are done, we are done. */
      case Phase.Done   => Phase.Done }

  /** In the base actor the path and name are equal. */
  def path: String = name

  /**
   * To be able to refer to this as an Actor, which is also used as sender for all messages
   * send from this actor without explicit sender. */
  given self: Actor[MyLetter] = this

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


