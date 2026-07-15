package s2a.control

import scala.concurrent.Promise
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
  ac.revive()
  private val promise = Promise[Result]()
  private val delay   = ac.delayed(tryCall(), timeout)
  private var count   = 0
  private var result: Option[Result] = None
  private def tryCall(): Unit =
    val called = Try(call)
    result = called.toOption
    val _ = promise.tryComplete(called)
  def done(): Unit =
    val copy = synchronized { count = count + 1; count }
    if copy == limit then
      delay.cancel()
      tryCall()
  /* Future handling is platform dependent. SBT cannot handle the futures on my actor
   * execution context in emulated mode. So we treat them separately with a manual await. */
  def await(poll: FiniteDuration = 50.millis) = if ac.emulated then
    ac.waitForExit(false,poll)(result.nonEmpty, () => ())
  def compare(expected: Result => Unit): Unit =
    if ac.emulated
    then result.map(expected).getOrElse(assert(false))
    else ac.platform match
      case Platform.JVM    => promise.future.foreach(expected)
      case Platform.JS     => promise.future.foreach(expected)
      case Platform.Native => result.map(expected).getOrElse(assert(false))

