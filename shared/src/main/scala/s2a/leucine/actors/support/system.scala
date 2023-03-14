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
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

/** Trait to define the parameters needed for the context and platform classes to run. */
trait SystemParameters :
  /** Helper value to switch on/off tracing for debugging. Start your debug lines with if trace then ... */
  def actorTracing: Boolean
  /** The natural time the system pauses when there are no tasks. The use is platform dependant. */
  def idleThreadPause: FiniteDuration
  /** The prefix used in actornames for actors that are workers */
  def workerPrefix: String
  /** The character that will be used in the full name definitions of the actors. */
  def familyPathSeparator: Char
  /** Global maximum number of letters per mailbox. */
  def maxMailboxSize: Int


/**
 * Default parameters for the system you may use. If other values are needed, construct an instance
 * of your own and initialise the ActorImplementation with that. You may of course also write a
 * completely new ActorImplementation */
object DefaultSystem extends SystemParameters :
  val actorTracing: Boolean           = false
  val idleThreadPause: FiniteDuration = 10.millis
  val workerPrefix: String            = "#"
  val familyPathSeparator: Char       = '.'
  val maxMailboxSize: Int             = Int.MaxValue
