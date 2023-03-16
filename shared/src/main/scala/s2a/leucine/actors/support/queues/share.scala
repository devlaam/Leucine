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


/** Shared base for BurstQueue and DropQueue */
class ShareQueue[M] :

  /** The queue that holds all incomming messages */
  protected var queueIn: List[M] = Nil

  /** The queue that holds all outgoing messages */
  protected var queueOut: List[M] = Nil

  /** The number of messages currently stored */
  protected var sizeNow: Int = 0

  /** The highest number messages stored at once */
  protected var sizeMax: Int = 0

  /** The total number of messages processed */
  protected var sizeSum: Int = 0


  /** Put an element on the queue. Always fast O(1). */
  def enqueue(message: M): Unit =
    queueIn = message :: queueIn
    sizeNow = sizeNow + 1
    sizeSum = sizeSum + 1
    if sizeNow > sizeMax then sizeMax = sizeNow

  /** See if the queue is empty, always fast: O(1)  */
  def isEmpty: Boolean = sizeNow == 0

  /** The size of this queue, always fast: O(1) */
  def size: Int = sizeNow

  /** Remove all messages from the queue(s), always fast: O(1) */
  def clear(): Unit =
    queueIn  = Nil
    queueOut = Nil
    sizeNow  = 0

  /** The maximum number of messages that were held in the queue at once */
  def max: Int = sizeMax

  /** The total number of messages that were processed by the queue */
  def sum: Int = sizeSum

  /** Clear the statistics on this queue. The queue itself remains as is.  */
  def reset(): Unit =
    sizeMax = sizeNow
    sizeSum = 0
