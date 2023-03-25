package s2a.leucine.actors

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred}

object StateActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Clock(val writeln: String => Unit, val done: () => Unit) extends StateActor[Clock.Letter,Clock.Accept,Clock.State]("clock") :
   override protected def stopped(cause: Actor.Stop, complete: Boolean) = done()
   protected def initial = Clock.State(0,0,0)
   protected def receive(letter: Clock.Letter, sender: Sender, state: Clock.State): Clock.State = letter match
     case Clock.Tick(extraSec) => state.advance(extraSec)
     case Clock.PrintTime      => writeln(state.show); state

  object Clock :
    sealed trait Letter extends Actor.Letter
    case class Tick(extraSec: Int) extends Letter
    case object PrintTime extends Letter
    type Accept = Actor.Anonymous

    class State(hour: Int, min: Int, sec: Int) extends Actor.State :
      def advance(eSec: Int): State =
        val sec  = (this.sec + eSec) % 60
        val eMin = (this.sec + eSec) / 60
        val min  = (this.min + eMin) % 60
        val eHr  = (this.min + eMin) / 60
        val hour = (this.hour + eHr) % 24
        State(hour,min,sec)
      def show: String = s"$hour:$min:$sec"

  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    val expect = List("0:0:30","0:2:10","0:18:50","6:29:12")
    test("changing state"){
      val deferred = Deferred(buffer.readlns)
      val clock = new Clock(buffer.writeln,deferred.done)
      clock ! Clock.Tick(30)
      clock ! Clock.PrintTime
      clock ! Clock.Tick(100)
      clock ! Clock.PrintTime
      clock ! Clock.Tick(1000)
      clock ! Clock.PrintTime
      clock ! Clock.Tick(22222)
      clock ! Clock.PrintTime
      clock.stop(Actor.Stop.Finish)
      deferred.await()
      deferred.compare(_ ==> expect) } }

