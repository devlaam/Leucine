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


import scala.collection.mutable


/* Methods stub for when there is no stash mixin used. */
private[actors] trait StashDefs :
  private[actors] type Env
  private[actors] def stashFlush: Boolean = false
  private[actors] def stashEnqueue(envelope: Env): Unit = ()
  private[actors] def stashDequeue(tail: List[Env]): List[Env] = tail
  private[actors] def stashEmpty: Boolean = true
  private[actors] def stashClear(): Unit = ()


/** Mixin if you need to store letters away.  */
trait StashActor extends ActorDefs :

  /* See if this actor is still active. */
  def isActive: Boolean

  /** Actor dependend packing of letter and sender into one enveloppe. */
  private[actors] def pack(letter: MyLetter, sender: Sender): Env

  /** The queue for the letters/senders. */
  private val stashbox: BurstQueue[Env] = new BurstQueue[Env]

  /**
   * flushRequest holds if the user has requested a flush during processing current the letter
   * This variable can only be set from within the actor thread, so during the processLoop. */
  private var flushRequest: Boolean = false

  /** storeRequest holds if the user has requested to keep the current the letter */
  private var storeRequest: Boolean = false

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeStash(): Option[MonitorActor.Stash] = Some(MonitorActor.Stash(stashbox.sum,stashbox.max))

  /** Internal test to see if we must flush */
  private[actors] override def stashFlush: Boolean = flushRequest

  /** Internal operation to completely clear the stash. */
  private[actors] override def stashClear(): Unit =
    flushRequest = false
    storeRequest = false
    stashbox.clear()

  /** See if the stash is empty. */
  private[actors] override def stashEmpty: Boolean = stashbox.isEmpty

  /**
   * Internal enqueue operation for the stash. This handles the storeRequest for
   * the last letter that was processed. */
  private[actors] override def stashEnqueue(envelope: Env): Unit = if storeRequest then
    /* This call handles the store(), so we are done. Reset the storeRequest */
    storeRequest = false
    /* Put the letter/sender on the stash. */
    if isActive then stashbox.enqueue(envelope)

  /** Internal dequeue operation for the stash. */
  private[actors] override def stashDequeue(tail: List[Env]): List[Env] = if !flushRequest then tail else
    /* Just to be save, we remove any store requests after dequeing. */
    storeRequest = false
    /* This call handles the flush, so we are done. Reset the flushRequest */
    flushRequest = false
    /* Get the letters/senders from the queue. */
    stashbox.dequeue(tail)


  /** Object Stash for user to manipulate the Stash */
  protected object Stash:

    /** Automatically stores the current letter (and sender) that is processed on the stash. */
    def store(): Unit = storeRequest = true

    /**
     * Store a letter and sender manually on the stash. With this method, you may replace one
     * letter with an other, or spoof the sender, and reprocess later. */
    def store(letter: MyLetter, sender: Sender): Unit = if isActive then stashbox.enqueue(pack(letter,sender))

    /** Clear the stash instantly. */
    def clear(): Unit = stashClear()

    /**
     * Flush the stash to the mailbox. This is usually the last instruction before you
     * switch to a new state to handle the stashed messages once more. */
    def flush(): Unit = flushRequest = !stashbox.isEmpty

    /** See how many letters are on the stash. This is fast O(1). */
    def size: Int = stashbox.size

    /** See is there are any letters on the stash. */
    def isEmpty: Boolean = stashEmpty

