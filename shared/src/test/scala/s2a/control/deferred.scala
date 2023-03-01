package s2a.control

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.util.Try

import utest.*

import s2a.leucine.actors.{ActorContext, PlatformContext }

/* It is not possible to use eventually to test, for they are not supported on JS/Native.
 * So we must construct something by hand. (This also works for the JVM) */
/** Use this to complete the result by a number of events (done) or with a timeout. */
class Deferred[Result](call: => Result, limit: Int = 1, timeout: FiniteDuration = 1000.millis)(using ac: ActorContext) :
  import PlatformContext.Platform
  private val promise = Promise[Result]()
  private var count = 0
  private var result: Option[Result] = None
  private def tryCall(): Unit =
    val called = Try(call)
    result = called.toOption
    promise.tryComplete(called)
  private var delay = ac.delayed(tryCall(), timeout)
  def done(): Unit =
    val copy = synchronized { count = count + 1; count }
    if copy == limit then
      delay.cancel()
      tryCall()
  /* Future handling is platform dependent. SBT cannot handle the futures on my actor
   * execution context for Native. So we treat them seperately with a manual await. */
  def await() = if ac.platform == Platform.Native then
    ac.waitForExit(false,10.seconds)(false, () => ())
  def compare(expected: Result => Unit) =
    def complete(): Unit = result match
      case Some(r) => expected(r)
      case None    => assert(false)
    ac.platform match
    case Platform.JVM    => promise.future.map(expected)
    case Platform.JS     => promise.future.map(expected)
    case Platform.Native => result.map(expected).getOrElse(assert(false))

