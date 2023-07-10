package s2a.leucine.actors

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred, Helpers}
import s2a.leucine.actors.PlatformContext.Platform

trait ProcessAidTest(using ac: ActorContext) :

  class Ticker(val writeln: String => Unit, val done: () => Unit) extends RestrictActor(Ticker), ProcessAid :
    import Actor.Post


    protected def process[Sender <: Accept]: PartialFunction[(Letter[Sender],Sender),Receive] =
      case (Ticker.Start,_)    =>
        this ! Ticker.Count(0)
        switch(new Process.Push { def apply[Sender <: Accept] = tick })
      case (Ticker.Count(n),_) => writeln(s"*$n")


    protected def tick[Sender <: Accept]: PartialFunction[(Letter[Sender],Sender),Receive] =
      case (Ticker.Count(n),_) =>
        writeln(s"$n")
          this ! Ticker.Count(n+1)
        if      n<4  then switch(new Process.Push    { def apply[Sender <: Accept] = tock })
        else if n==4 then switch(new Process.Replace { def apply[Sender <: Accept] = tock })
        else              switch(new Process.Pop() )

    protected def tock[Sender <: Accept]: PartialFunction[(Letter[Sender],Sender),Receive] =
      case (Ticker.Count(n),_) =>
        this ! Ticker.Count(n+1)
        switch(new Process.Replace { def apply[Sender <: Accept] = tick })

  object Ticker extends RestrictDefine, Stateless :
    type Accept = Actor
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case class  Count(n: Int) extends Letter[Accept]
    case object Start extends Letter[Accept]


  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, mix by pausing."){
      val expect = List("0","2","4","6","7","8","*9")
      val deferred = Deferred(buffer.readlns)
      val ticker = new Ticker(buffer.writeln,deferred.done)
      ticker ! Ticker.Start
      deferred.await()
      deferred.compare(_ ==> expect) } }


object ProcessAidTestSystem extends TestSuite, ProcessAidTest(using ActorContext.system)
