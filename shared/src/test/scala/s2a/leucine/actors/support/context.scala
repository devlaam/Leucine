package s2a.leucine.actors

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try
import utest.*


//TODO: uTest cannot handle the manual implemention of the event loop in Native. Solve.
object ActorContextTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  val tests = Tests {
    test("futures") {
      test("Standard Future") - Future(42).map(_ ==> 42)
      test("Success Future")  - ac.future(42).map(_ ==> 42)
      test("Failure Future")  - ac.future(throw new Exception("crash")).recover(_.getMessage ==> "crash") }
    test("delays") {
      var d1,d2 = false
      val delay1 = ac.delayed({d1 = true}, 80.millis)
      val delay2 = ac.delayed({d2 = true}, 60.millis)
      ac.delayed({delay2.cancel()}, 30.millis)
      /* It is not possible to use eventually to test here, for they are not supported on JS/Native.
       * So we must construct something by hand. (This also works for the JVM) */
      val p = Promise[(Boolean,Boolean)]()
      val f = p.future
      ac.delayed(p.complete(Try(d1,d2)), 100.millis)
      test("continued delay") - f.map((x,_) => x ==> true)
      test("cancelled delay") - f.map((_,x) => x ==> false) } }
