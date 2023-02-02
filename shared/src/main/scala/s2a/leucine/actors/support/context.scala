package s2a.leucine.actors

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

  /**
   * Helper value to switch on/off tracing for debugging. Start your debug lines with
   * if trace then ... */
  val trace = false

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
  private class ActorImplementation(val pause: FiniteDuration) extends ContextImplementation with ActorContext
  /** Provide a default actor implementation with a pause time of 10ms.*/
  val system: ActorContext = new ActorImplementation(10.millis)

