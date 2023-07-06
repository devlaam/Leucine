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
 * This is special kind of queue, that only shows its head (more a stack actually).
 * An other property is that it is never depleted, there is always at least one
 * element present. At start, this is the element init, but it may be replaced
 * with an other element. After clear however, the element init is restored.
 * The stack is considered empty if no elements have been pushed or directly
 * after a clear operation. The init element is not counted as being present
 * on the stack. */
class StackQueue[M](init: M) extends ShareQueue[M] :

  /** Return the topmost element, there is always one. */
  def head: M = if isEmpty then init else queueIn.head

  /** Put one element on top. */
  // TODO: rename enqueue as push, dequeue as pull ?
  def push(element: M) = enqueue(element)

  /**
   * Remove n elements from the stack (the n last ones that were pushed). If this
   * depletes the queue, this is equivalent to a clear(). Operation is of order
   * O(n) unless n exceeds the size, in which case it is of order O(1). Non
   * positive values for n are ignored. */
  def pop(n: Int): Unit =
    if      n <= 0    then ()
    else if n >= size then clear()
    else
      queueIn = queueIn.drop(n)
      sizeNow = sizeNow - n

  /**
   * Replace the topmost element (if present) with this element, else push this
   * element on top. */
  def patch(element: M) =
    if isEmpty then push(element) else queueIn = element :: queueIn.tail

  /**
   * Trim the stack from the bottom so that maximally n elements remain.
   * If n exceeds the size, this operation has no effect and if n<=0, this
   * is equivalent to a clear(). In both situations the operation is of order
   * O(1). Otherwise it is of order O(n).  */
  def preserve(n: Int): Unit =
    if      n <= 0    then clear()
    else if n >= size then ()
    else
      queueIn = queueIn.drop(size - n)
      sizeNow = n

