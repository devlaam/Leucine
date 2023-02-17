package s2a.control

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.util.Try
import utest.*

import s2a.leucine.actors.ActorContext

/* It is not possible to use eventually to test, for they are not supported on JS/Native.
 * So we must construct something by hand. (This also works for the JVM) */
/** Use this to complete the result by a number of events (done) or with a timeout. */
class Deferred[Result](call: => Result, limit: Int = 1, timeout: FiniteDuration = 1000.millis)(using ac: ActorContext):
  private val promise = Promise[Result]()
  private var count = 0
  private var delay = ac.delayed(promise.tryComplete(Try(call)), timeout)
  def result: Future[Result] = promise.future
  def done(): Unit =
    val copy = synchronized { count = count + 1; count }
    if copy == limit then
      delay.cancel()
      promise.tryComplete(Try(call))


