package s2a.leucine.actors

import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import scala.util.Try
import utest.*

import s2a.control.{Buffer, Deferred, Helpers}

trait TimingAidTest(using ac: ActorContext) :

  class Clock(withDump: Boolean, writeln: String => Unit, done: () => Unit) extends AcceptActor(Clock), TimingAid :

    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      writeln(s"stopped:$complete")
      done()

    var anchor: Object = new Object

    val _ = post(Clock.Stop,120.millis,new Object)

    def receive(letter: Clock.Letter, sender: Sender): Unit = letter match
      case  Clock.Result(value: Int) => writeln(s"$value")
      case  Clock.Done               => stop(Actor.Stop.Finish)
      case  Clock.Stop =>
        writeln(s"stop")
        clearAllTiming()
        val _ = post(Clock.Done,30.millis)
      case  Clock.Twice(value) =>
        if withDump then clearTiming(anchor)
        anchor = new Object
        val _ = post(Clock.Result(value),9.millis, new Object)
        val _ = post(Clock.Twice(value + 2),13.millis, new Object)
        val _ = post(Clock.Twice(value + 1),19.millis, anchor)

  object Clock extends AcceptDefine, Stateless :
    sealed trait Letter extends Actor.Letter[Actor]
    case class Result(value: Int) extends Letter
    case class Twice(value: Int) extends Letter
    case object Stop extends Letter
    case object Done extends Letter


  class Expect(event: => Boolean, writeln: String => Unit, done: () => Unit) extends AcceptActor(Expect), TimingAid :

    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      writeln(s"stopped:$complete")
      done()

    def fullfil(msg: String): Option[Expect.Release] = if event then Some(Expect.Release(msg)) else { writeln("x"); None }
    val _ = expect(fullfil("tada!"))

    def receive(letter: Expect.Letter, sender: Sender): Unit = letter match
      case Expect.Release(msg) =>
        writeln(msg)
        stop(Actor.Stop.Finish)

  object Expect extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case class Release(msg: String) extends Letter


  val tests = Tests {
    import Auxiliary.toUnit
    val buffer = Buffer[String]
    test("clock with multiple timers for dump()"){
      val deferred = Deferred(buffer.readlns)
      val clock = new Clock(true,buffer.writeln,deferred.done)
      clock ! Clock.Twice(1)
      deferred.await()
      deferred.compare(list =>
        /* End should not be  mixed with arrived numbers */
        list.takeRight(2) ==> List("stop","stopped:true")
        /* List should only contain odd numbers. */
        list.dropRight(2).map(i => (i.toInt - 1) % 2).sum ==> 0 ) }
    test("clock with multiple timers for dumpAll()"){
      val deferred = Deferred(buffer.readlns)
      val clock = new Clock(false,buffer.writeln,deferred.done)
      clock ! Clock.Twice(1)
      deferred.await()
      /* Here we are only interested that there are no numbers between stop and stopped.*/
      deferred.compare(_.takeRight(2) ==> List("stop","stopped:true")) }
    test("Timing with expectation"){
      val deferred = Deferred(buffer.readlns)
      val promise = Promise[Unit]()
      val _ = ac.delayed(promise.tryComplete(Try(())).toUnit, 67.millis)
      new Expect(promise.isCompleted,buffer.writeln,deferred.done)
      deferred.await()
      deferred.compare(list =>
        /* End should only contain succeeded event and end. */
        list.takeRight(2) ==> List("tada!","stopped:true")
        /* List should only contain missed events */
        list.dropRight(2).map(c => if c=="x" then 0 else 1).sum ==> 0 ) } }


object TimingAidTestSystem extends TestSuite, TimingAidTest(using ActorContext.system)

object TimingAidTestEmulationNJS extends TestSuite, TimingAidTest(using Helpers.emulatedContext)
