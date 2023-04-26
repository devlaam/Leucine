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
 * Mixin which enables the initialization of a class with an unknown number of mixins.
 * Note, this is only required for the mixins that are used by the library user. Make sure
 * that each mixin ends with the following statements:
 *   override def initCount: Int = super.initCount + 1
 *   initReady()
 * In the top most class that is overridden by the user add:
 *   def initComplete(): Unit = { ... }
 *   initReady() */
private transparent trait ActorInit :
  /* Counter which is filled with the number of classes/traits at the start. */
  private var ic: Int  = initCount
  /** Method called when the instantiation is complete. Put you startup code in the implementation. */
  def initComplete(): Unit
  /** Auxiliary method to determine the number of mixins upfront. To be overridden in each mixin. */
  def initCount: Int = 1
  /** Method that each mixin must call at the end of its constructor. */
  def initReady(): Unit =
    /* For each constructed class/trait reduce the counter with 0 */
    ic = ic - 1
    /* When we hit zero, we know the class instantiation is completed. */
    if ic == 0 then initComplete()

