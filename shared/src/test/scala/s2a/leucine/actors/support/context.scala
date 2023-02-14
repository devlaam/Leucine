package s2a.leucine.actors

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try
import utest.*

import s2a.control.Deferred

//TODO: uTest cannot handle the manual implemention of the event loop in Native. Solve.
object ActorContextTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  val tests = Tests {
    test("futures") {
      test("Standard Future")   - Future(42).map(_ ==> 42)
      test("AC Success Future") - ac.future(42).map(_ ==> 42)
      test("AC Failure Future") - ac.future(throw new Exception("crash")).recover(_.getMessage ==> "crash") }
    test("delays") {
      var d1,d2 = false
      val delay1 = ac.delayed({d1 = true}, 80.millis)
      val delay2 = ac.delayed({d2 = true}, 60.millis)
      ac.delayed({delay2.cancel()}, 30.millis)
      val deferred = Deferred((d1,d2))
      test("continued delay") - deferred.result.map((x,_) => x ==> true)
      test("cancelled delay") - deferred.result.map((_,x) => x ==> false) } }
