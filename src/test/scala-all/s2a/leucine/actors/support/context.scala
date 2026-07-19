package s2a.leucine.actors

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import utest.*

import s2a.control.Deferred

/* SBT does not seems able to catch futures on my execution context, which is kind of reasonable.
 * So these tests are only performed on JVM and JS. (NN = Non Native) */
object ActorContextTestNN extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system
  val stdEx = ExecutionContext.Implicits.global

  val tests = Tests {
      test("futures") {
        test("Standard Future, Standard global ExecutionContext")   - Future(42)(using stdEx).map(_ ==> 42)(using stdEx)
        test("Standard Future, ActorContext as ExecutionContext")   - Future(42).map(_ ==> 42)
        test("AC Success Future") - ac.future(42).map(_ ==> 42)
        test("AC Failure Future") - ac.future(throw new Exception("crash")).recover(_.getMessage ==> "crash") }
      test("delays") {
      var d1,d2 = false
      val _ = ac.delayed({d1 = true}, 80.millis)
      val delay = ac.delayed({d2 = true}, 60.millis)
      val _ = ac.delayed({delay.cancel()}, 30.millis)
      val deferred = Deferred((d1,d2),0,100.millis)
      test("continued delay") - deferred.compare((x,_) => x ==> true)
      test("cancelled delay") - deferred.compare((_,x) => x ==> false) } }

