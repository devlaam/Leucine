package s2a.leucine.actors

import scala.collection.mutable


trait StashDefs :
  private[actors] type Env
  private[actors] def stashFlush: Boolean = false
  private[actors] def stashEnqueue(envelope: Env): Unit = ()
  private[actors] def stashDequeue: List[Env] = Nil


/** Mixin if you need to store letters away.  */
trait StashActor extends ActorDefs :

  private[actors] def pack(letter: MyLetter, sender: Sender): Env

  /* The queue for the letters/senders. */
  private val envelopes: BurstQueue[Env] = new BurstQueue[Env]

  /* flushRequest holds if the user has requested a flush during processing current the letter
   * This variable can only be set from within the actor thread, so during the processLoop. */
  private var flushRequest: Boolean = false

  /* storeRequest holds if the user has requested to keep the current the letter */
  private var storeRequest: Boolean = false

  /* Take a snapshot of the internals of this actor. */
  private[actors] override def probeStash(): Option[MonitorActor.Stash] = Some(MonitorActor.Stash(envelopes.sum,envelopes.max))

  /* Internal test  to see if we must flush */
  private[actors] override def stashFlush: Boolean = flushRequest

  /* Internal operation to completely clear the stash. */
  private def _stashClear(): Unit =
    flushRequest = false
    storeRequest = false
    envelopes.clear()


  /* Internal ebqueue operation for the stash. This handles the storeRequest for
   * the last letter that was processed.  */
  private[actors] override def stashEnqueue(envelope: Env): Unit = if storeRequest then
    /* This call handles the store(), so we are done. Reset the storeRequest */
    storeRequest = false
    /* Put the letter/sender on the stash. */
    envelopes.enqueue(envelope)

  /* Internal dequeue operation for the stash. */
  private[actors] override def stashDequeue: List[Env] =
    /* Just to be save, we remove any store requests after dequeing. */
    storeRequest = false
    /* This call handles the flush, so we are done. Reset the flushRequest */
    flushRequest = false
    /* Get the letters/senders from the queue. */
    envelopes.dequeue


  /* Object Stash for user to manipulate the Stash */
  protected object Stash:
    /** Automatically stores the current letter (and sender) that is processed on the stash. */
    def store(): Unit = storeRequest = true

    /* Store a letter and sender manually on the stash. With this method, you may replace one
     ** letter with an other, or spoof the sender, and reprocess later. */
    def store(letter: MyLetter, sender: Sender): Unit = envelopes.enqueue(pack(letter,sender))

    /** Clear the stash instantly. */
    def clear(): Unit = _stashClear()

    /** Flush the stash to the mailbox. This is usually the last instruction before you switch
     * to a new state to handle the stashed messages once more. */
    def flush(): Unit = flushRequest = !envelopes.isEmpty

    /** See how many letters are on the stash. This is fast O(1). */
    def size: Int = envelopes.size

    /** See is there are any letters on the stash. */
    def isEmpty: Boolean = envelopes.isEmpty

