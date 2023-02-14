package s2a.control

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.util.Try
import utest.*

import s2a.leucine.actors.ActorContext

/* It is not possible to use eventually to test, for they are not supported on JS/Native.
 * So we must construct something by hand. (This also works for the JVM) */
class Deferred[Result](call: => Result, timeout: FiniteDuration = 100.millis)(using ac: ActorContext):
  private val promise = Promise[Result]()
  def result: Future[Result] = promise.future
  ac.delayed(promise.complete(Try(call)), timeout)
