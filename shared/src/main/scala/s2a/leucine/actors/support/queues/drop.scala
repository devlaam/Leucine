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


/**
 * Mutable queue for managing the event queue.
 * This application typically has very short lists, since placing events
 * is much more rare than consuming them. But consumption is one by one.
 * A nasty extra is that we must sometimes filter a particular element
 * out. This should be rare though. */
class DropQueue[M] extends ShareQueue[M] :
  import DropQueue.*

  /* In 99% of the situations there will be just one element available,
   * (because we only dequeue when we know that the queue is not empty and
   *  we test very often. The queue is more often emptied than filled.
   * The result will be an empty list or one element in a list.  */
  /** Remove one element from the queue (in any) and put that in a list. */
  def dequeue: List[M] =
    var result: List[M] = Nil
    /* So we handle the most occurring situation first */
    if sizeNow == 1 then
      result = if queueIn.isEmpty then queueOut else queueIn
      clear()
    /* Now see if there are elements in the out queue, if sou the request
     * element is upfront.  */
    else if !queueOut.isEmpty then
      result   = queueOut.take(1)
      queueOut = queueOut.drop(1)
      sizeNow  = sizeNow - 1
    /* If queueOut is empty, there could be elements in queueIn, we take
     * opportunity to reverse the queueIn, which must happen anyway. */
    else if !queueIn.isEmpty then
      queueOut = queueIn.reverse
      queueIn  = Nil
      result   = queueOut.take(1)
      queueOut = queueOut.drop(1)
      sizeNow  = sizeNow - 1
    /* The last situation implies sizeNow == 0 and all is already
     * good in that case. Now return the result */
    result

  /** Remove all elements that fulfill the test. This is an expensive operation, may be 2xO(1). */
  def remove(test: M => Boolean): Unit =
    /* If we have only one element, and one of the queues has it (most often case)
     * we can just clear. Otherwise nothing changes. */
    if sizeNow == 1 then
      if queueIn.exists(test) || queueOut.exists(test) then clear()
    /* now if we have a situation were there are no elements in queueOut we
     * take the opportunity to filter and reverse in one go: */
    else if queueOut.isEmpty then
      val rr   = removeReverse(test,queueIn)
      queueIn  = Nil
      queueOut = rr.result
      sizeNow  = rr.size
    /* Well, if the element might be in queueOut but queueIn is empty we do the opposite */
    else if queueIn.isEmpty then
      val rr   = removeReverse(test,queueOut)
      queueIn  = rr.result
      queueOut = Nil
      sizeNow  = rr.size
    /* If both are filled we have the most work, */
    else
      /* Remove the elements from both queues */
      val rrIn  = removeReverse(test,queueIn)
      val rrOut = removeReverse(test,queueOut)
      /* If we did not detect any positives, there is no need to reverse back. */
      if rrIn.changed  then queueIn  = rrIn.result.reverse
      if rrOut.changed then queueOut = rrOut.result.reverse
      sizeNow = rrIn.size + rrOut.size


object DropQueue :

  /* Unfortunately there is no filterRevese in the List definition.
   * we take the opportunity to return the new size and an removal indicator
   * as well */

  /** Helper Class to communicate the removeReverse result. */
  private[actors] case class RemovedReversed[M](val result: List[M], val size: Int, val changed: Boolean)

  /** Remove elements that fulfill the predicate and reverse the list in the process. */
  private[actors] def removeReverse[M](p: M => Boolean, in: List[M]): RemovedReversed[M] =
    var size = 0
    var changed = false
    var result: List[M] = Nil
    var source = in
    while (!source.isEmpty)
      val head = source.head
      if p(head) then changed = true else
        result = head :: result
        size = size + 1
      source = source.drop(1)
    RemovedReversed(result,size,changed)
