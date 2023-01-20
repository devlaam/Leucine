package s2a.leucine.actors

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

/**
 * An extended `scala.concurrent.ExecutionContext`; provides the ability to
 * schedule messages to be sent later, and hooks to track the current number of
 * outstanding tasks or log the actor message sends for debugging purposes
 */
trait ActorContext extends PlatformContext with ExecutionContext :

  val trace = false

  def future[T](task: => T): Future[T] =
    val promise = Promise[T]()
    def runnable =  new Runnable { def run() = promise.complete(Try(task)) }
    execute(runnable)
    promise.future

  def reportFailure(cause: Throwable): Unit = cause.printStackTrace()


object ActorContext :
  private class ActorImplementation extends ContextImplementation with ActorContext
  val system: ActorContext = new ActorImplementation()

