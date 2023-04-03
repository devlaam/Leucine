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
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt


/**
 * An extended `scala.concurrent.ExecutionContext`; provides the ability to
 * schedule messages to be sent later, and hooks to track the current number of
 * outstanding tasks or log the actor message sends for debugging purposes */
trait ActorContext extends PlatformContext with ExecutionContext :

  /** Helper value to switch on/off tracing for debugging. Start your debug lines with if actorTracing then ... */
  def actorTracing: Boolean

  /** The prefix used in actornames for actors that are workers */
  def workerPrefix: String

  /** The character that will be used in the full name definitions of the actors.*/
  def familyPathSeparator: Char

  /** Global maximum number of letters per mailbox. */
  def maxMailboxSize: Int

  /** The minimal number of poll loops (from the guard) for an actor to be considered silent. */
  def silentStop: Int

  /** Future to be executed on the ActorContext. */
  def future[T](task: => T): Future[T] =
    val promise = Promise[T]()
    def runnable = new Runnable { def run() = promise.complete(Try(task)) }
    execute(runnable)
    promise.future

  /** Delayed task to be executed on the ActorContext. */
  def delayed(task: => Unit, time: FiniteDuration): Cancellable =
    val callable = new Callable[Unit] { def call() = task }
    schedule(callable,time)

  /** Default way to handle an exception. Override if needed. */
  def reportFailure(cause: Throwable): Unit = cause.printStackTrace()


object ActorContext :

  /** The default ActorImplementation for the users. */
  class ActorContexDirectImplementation(val parameters: SystemParameters) extends ContextImplementation, ActorContext, SystemParameters :
    export parameters.*
    def platform = ContextImplementation.platform

  /** Provides a default actor implementation that runs on the threading supported by the platform. */
  val system: ActorContext = new ActorContexDirectImplementation(DefaultSystem)

  /** The emulated ActorImplementation for the users. */
  class ActorContexEmulatedImplementation(val parameters: SystemParameters) extends ContextEmulation, ActorContext, SystemParameters :
    export parameters.*
    def platform = ContextImplementation.platform

  /** Provides a default actor implementation that runs on a platform independant emulation layer for threads. */
  val emulated: ActorContext = new ActorContexEmulatedImplementation(DefaultSystem)
