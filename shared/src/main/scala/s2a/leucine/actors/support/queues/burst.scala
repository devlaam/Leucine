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
 * Mutable class for managing the message queue .
 * We want this to be fast, and we know we always empty all at once, after which it may
 * grow again. Therefore we might as well use a list. Growing is fast, and whatever other
 * structure we use, there will at least be once an O(n) operation involved. In case there
 * are not a lot of messages List wins from the other collections because of the low
 * overhead setup.*/
class BurstQueue[M] extends ShareQueue[M] :

  /**
   * Get all the messages in posted order, clear queue. Since the list must be reversed
   * this is not fast O(n). If there is already a tail, elements of the queue are prepended. */
  def dequeue(tail: List[M] = Nil): List[M] =
    val result = queueIn reverse_::: tail
    queueIn  = Nil
    sizeNow  = 0
    result
