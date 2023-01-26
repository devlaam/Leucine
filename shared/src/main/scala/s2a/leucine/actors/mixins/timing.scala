package s2a.leucine.actors

import java.util.concurrent.Callable
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration


trait TimingDefs :
  private[actors] type Env
  private[actors] def eventsPresent: Boolean = false
  private[actors] def eventsCancel(): Unit = ()
  private[actors] def eventsDequeue: List[Env] = Nil


trait TimingActor(using context: ActorContext) extends ActorDefs :
  import TimingActor.Event

  private[actors] def pack(letter: MyLetter, sender: Sender): Env
  private[actors] def processTrigger(): Unit

  /* Holds all references to the runnig timers, so we can cancel them when needed. */
  private val anchors: mutable.Map[Object,Cancellable] = mutable.Map.empty

  /* Holds the evenvelopes waiting to get scheduled */
  private val events: DropQueue[Event[MyLetter]] = new DropQueue[Event[MyLetter]]()

  /* Take a snapshot of the internals of this actor. */
  private[actors] override def probeTiming(): Option[MonitorActor.Timing] = Some(MonitorActor.Timing(events.sum,events.max,anchors.size))

  /* Construct a new callable on the fly. The call method puts the event on the events queue and
   * calls trigger in order to start the loop if needed. */
  private def callable(event: Event[MyLetter]) =
    def handle() = synchronized {
      events.enqueue(event)
      anchors.remove(event.anchor)
      processTrigger() }
    new Callable[Unit] { def call(): Unit = handle() }

  /* Construct a new digestable on the fly. The digest method takes the letter given to it, and
   * constructs a new event for the events queue and calls trigger in order to start the loop if
   * needed. */
  private def digestable(anchor: Object): Digestable[MyLetter] =
    def handle(letter: MyLetter) = synchronized {
      events.enqueue(Event(anchor,letter))
      anchors.remove(anchor)
      processTrigger() }
    new Digestable[MyLetter] { def digest(letter: MyLetter): Unit = handle(letter) }

  /* See if there are events present.  */
  private[actors] override def eventsPresent: Boolean = synchronized { !events.isEmpty }

  /* Remove all posted letters. */
  private[actors] override def eventsCancel(): Unit = synchronized {
    anchors.values.foreach(_.cancel())
    anchors.clear()
    events.clear() }

  /* Obtain a single event from the eventqueu, get the letter and put it in an envelope. */
  private[actors] override def eventsDequeue: List[Env] = synchronized {
     events.dequeue.map(event => pack(event.letter,self)) }

  /** A letter is send to myself after the delay. If there is already a letter posted under this
    * anchor, it will replace that letter. The original is guaranteed not to arrive, even if the
    * timer has already expired during execution of this letter. */
  protected def post(letter: MyLetter, delay: FiniteDuration, anchor: Object = this): Unit = synchronized {
    /* First remove a post that may be out there. */
    dump(anchor)
    /* Then schedule a new timer and add it to the achors map. */
    anchors.addOne(anchor -> context.schedule(callable(Event(anchor,letter)),delay)) }

  /** Stop a scheduled letter, after dump() it is guaranteed that no letters will arive on this
    * anchor, even if the timer has already expired during execution of this letter. */
  protected def dump(anchor: Object = this): Unit = synchronized {
    /* We are going to cancel the timer and remove the event. This must be synchronized
     * with the execution of the callable when the timer expires. So that we are sure that the timer
     * is or cancelled of has expired and put its event on the events queueu. Thus, call
     * cancel on the timer and remove. It can be that the event was already scheduled so that the anchor
     * is already gone. In that case the remove returns none. If so remove the event from the events queue. */
    anchors.remove(anchor).map(_.cancel()).getOrElse(events.remove(_.anchor == anchor)) }

  /** Stop all timers and make sure no delayed letters arive any more */
  protected def dumpAll(): Unit = eventsCancel()

  /** Send a letter to yourself the moment an event takes place. When this happens the method
    * fullfil should produce this letter to be put on the events queue.  The fullfil should
    * not block, if it does, it will hold the current thread. This may also hold the whole
    * application in a single threaded environment. As long as the event did not yet arrive,
    * fullfill should produce None asap. It will be probed somewhat later once more. */
  protected def expect(fullfil: () => Option[MyLetter], anchor: Object = this): Unit = synchronized {
    /* First remove prior anchor use. */
    dump(anchor)
    /* Then schedule a new expectation and add it to the achors map. */
    anchors.addOne(anchor -> context.await(digestable(anchor),fullfil)) }


object TimingActor :
  private class Event[L](val anchor: Object, val letter: L)


