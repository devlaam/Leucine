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
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration


/* Methods stub for when there is no timing mixin used. */
private[actors] trait TimingDefs extends BareDefs :
  //private[actors] type Env[T]
  //private[actors] type Sender
  private[actors] def eventsPossible: Boolean = false
  private[actors] def eventsPresent: Boolean = false
  private[actors] def eventsCancel(): Unit = ()
  private[actors] def eventsDequeue(tail: List[Env[?]]): List[Env[?]] = tail


/** Mixin which enables the actor to send mails to itself on predefined moments and wait for i/o events.
 * This actor sends messages to itself. For the moment, you must explicitly allow this in the sender
 * type definition of this actor. Otherwise mixing in this Aid generates a compiler error. */
trait TimingAid(using context: ActorContext) extends ActorDefs :
  this: ProcessActor =>
  //this: BareActor =>
  private type This = this.type
  type Sender >: This <: Actor
  private type Event[T >: This <: Sender] = TimingAid.Event[Sender,This,T,MyLetter]

  /** Actor dependent packing of letter and sender into one envelope. */
  //private[actors] def pack[T <: Sender](letter: MyLetter[T], sender: T | This): Env[T]
  //private[actors] def pack[T <: Sender](letter: MyLetter[T], sender: T): Env[T]
  private[actors] def pack[T >: This <: Sender](letter: MyLetter[T], sender: T): Env[T]

  /** Holds all references to the running timers, so we can cancel them when needed. */
  private val anchors: mutable.Map[Object,Cancellable] = mutable.Map.empty

  /** Holds the envelopes waiting to get scheduled */
  //private val events: DropQueue[Event[MyLetter[?]]] = new DropQueue[Event[MyLetter[?]]]()
  private val events: DropQueue[Event[?]] = new DropQueue[Event[?]]()

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeTiming(): Option[MonitorAid.Timing] = Some(MonitorAid.Timing(events.sum,events.max,anchors.size))

  /**
   * Construct a new callable on the fly. The call method puts the event on the events queue and
   * calls trigger in order to start the loop if needed. */
  //private def callable[T <: Sender](event: Event[MyLetter[T]]) =
  private def callable[T >: This <: Sender](event: Event[T]) =
    /* The handle takes an anchored event and puts it on the event queue. Due to the synchronization
     * eventsCancel() and handle cannot interfere. But it is possible that the call() was made before
     * the cancel but the handle afterwards. In that situation cancel() did miss its effect.
     * So we may only proceed if the anchor is still present. */
    def handle() = synchronized { if anchors.contains(event.anchor) then
      events.enqueue(event)
      anchors.remove(event.anchor)
      /* The process may be in pause, so we must tickle it. Events are a core process.*/
      processTrigger(true) }
    new Callable[Unit] { def call(): Unit = handle() }

  /**
   * Construct a new digestible on the fly. The digest method takes the letter given to it, and
   * constructs a new event for the events queue and calls trigger in order to start the loop if
   * needed. */
  private def digestible[T >: This <: Sender](anchor: Object): Digestible[MyLetter[T]] =
    def handle(letter: MyLetter[T]) = synchronized {
      events.enqueue(TimingAid.Event(anchor,letter))
      anchors.remove(anchor)
      /* The process may be in pause, so we must tickle it. Events are a core process.*/
      processTrigger(true) }
    new Digestible[MyLetter[T]] { def digest(letter: MyLetter[T]): Unit = handle(letter) }

  /** See if there could be events present.  */
  private[actors] override def eventsPossible: Boolean = true

  /** See if there are events present.  */
  private[actors] override def eventsPresent: Boolean = synchronized { !events.isEmpty }

  /** Remove all posted letters. */
  private[actors] override def eventsCancel(): Unit = synchronized {
    anchors.values.foreach(_.cancel())
    anchors.clear()
    events.clear() }

  /** Obtain a single event from the event queue, get the letter and put it in an envelope. */
  private[actors] override def eventsDequeue(tail: List[Env[?]]): List[Env[?]] = synchronized {
    val event = events.dequeue.map(event => pack(event.letter,this))
    //val event = events.dequeue.map(event => pack(event.letter,???))
    //val event = events.dequeue.map(event => ???)
    if       tail.isEmpty  then event
    else if  event.isEmpty then tail
    else                        event.head :: tail }

  /**
   * A letter is send to myself after the delay. If there is already a letter posted under this
   * anchor, it will replace that letter. The original is guaranteed not to arrive, even if the
   * timer has already expired during execution of this letter. If the actor was asked to finish,
   * the letter will NOT be posted AND the original letter is removed as well, if present.
   * Returns if the post was accepted. */
  protected def post[T >: This <: Sender](letter: MyLetter[T], delay: FiniteDuration, anchor: Object = this): Boolean = synchronized {
    /* First remove a post that may be out there. */
    dump(anchor)
    /* If we are not active, we may not accept this post. Otherwise ... */
    if !activity.active then false else
      /* ... schedule a new timer and add it to the anchors map. */
      anchors.addOne(anchor -> context.schedule(callable(TimingAid.Event(anchor,letter)),delay))
      true }

  /**
   * Stop a scheduled letter, after dump() it is guaranteed that no letters will arrive on this
   * anchor, even if the timer has already expired during execution of this letter. */
  protected def dump(anchor: Object = this): Unit = synchronized {
    /* We are going to cancel the timer and remove the event. This must be synchronized
     * with the execution of the callable when the timer expires. So that we are sure that the timer
     * is or cancelled of has expired and put its event on the events queue. Thus, call
     * cancel on the timer and remove. It can be that the event was already scheduled so that the anchor
     * is already gone. In that case the remove returns none. If so remove the event from the events queue. */
    anchors.remove(anchor).map(_.cancel()).getOrElse(events.remove(_.anchor == anchor)) }

  /** Stop all timers and make sure no delayed letters arrive any more */
  protected def dumpAll(): Unit = eventsCancel()

  /**
   * Send a letter to yourself the moment an event takes place. When this happens the method
   * fulfill should produce this letter to be put on the events queue.  The fulfill should
   * not block, if it does, it will hold the current thread. This may also hold the whole
   * application in a single threaded environment. As long as the event did not yet arrive,
   * fulfill should produce None asap. It will be probed somewhat later once more. If the actor
   * was asked to finish, the request will be ignored AND any former timer/expectation on
   * this anchor is cleared as well, if present. Returns if the expectation was accepted. */
  protected def expect[T >: This <: Sender](fullfil: => Option[MyLetter[T]], anchor: Object = this): Boolean = synchronized {
    /* First remove prior anchor use. */
    dump(anchor)
    /* If we are not active, we may not accept this expectation. Otherwise ... */
    if !activity.active then false else
    /* ... schedule a new expectation and add it to the anchors map. */
      anchors.addOne(anchor -> context.await(digestible(anchor),fullfil))
      true }


object TimingAid :
  /** Auxiliary class that holds the relevant elements of an event. */
  //private class Event[L](val anchor: Object, val letter: L)
  //private class EventNEW[A <: Actor, T <: A, L[T <: A] <: Actor.Letter](val anchor: Object, val letter: L[T])
  private class Event[A <: Actor, B <: A, T >: B <: A, L[T >: B <: A] <: Actor.Letter](val anchor: Object, val letter: L[T])
