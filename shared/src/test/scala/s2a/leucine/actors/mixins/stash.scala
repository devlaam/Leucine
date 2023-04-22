package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred, Helpers}
import s2a.leucine.actors.PlatformContext.Platform

trait StashAidTest(using ac: ActorContext) :

  class Stack(val writeln: String => Unit, val done: () => Unit) extends StandardActor(Stack), StashAid :
    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      writeln(s"stop:$complete")
      done()
    def receive[Sender <: Accept](letter: Stack.Letter[Sender], sender: Sender): (State => State) = (state: State) => letter match
      case  Stack.Write(value)  => if state.block && value%2==0 then { Stash.store(); state } else state.copy(value::state.values)
      case  Stack.Push(value)   => Stash.store(Stack.Write(value),sender); state
      case  Stack.Pop           => Stash.flush(); state.copy(block=false)
      case  Stack.Print         => writeln(state.values.toString); Stack.initial

  object Stack extends StandardDefine :
    type Accept = Actor
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case class Write(value: Int) extends Letter[Accept]
    case class Push(value: Int) extends Letter[Accept]
    case object Pop extends Letter[Accept]
    case object Print extends Letter[Accept]
    case class State(values: List[Int], block: Boolean) extends Actor.State
    val initial = Stack.State(Nil,true)

  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, mix by pausing."){
      val expect = List(List(24,23,22,21,20,18,16,15,14,13,12,10,8,7,6,5,4,2,19,17,11,9,3,1).toString,"stop:true")
      val deferred = Deferred(buffer.readlns)
      val stack = new Stack(buffer.writeln,deferred.done)
      (1 to 4).foreach(i => stack ! Stack.Write(i))
      (5 to 8).foreach(i => stack ! Stack.Push(i))
      (9 to 12).foreach(i => stack ! Stack.Write(i))
      (13 to 16).foreach(i => stack ! Stack.Push(i))
      (17 to 20).foreach(i => stack ! Stack.Write(i))
      stack ! Stack.Pop
      (21 to 24).foreach(i => stack ! Stack.Write(i))
      stack ! Stack.Print
      stack.stop(Actor.Stop.Finish)
      deferred.await()
      deferred.compare(_ ==> expect) } }


object StashAidTestSystem extends TestSuite, StashAidTest(using ActorContext.system)

object StashAidTestEmulationNJS extends TestSuite, StashAidTest(using Helpers.emulatedContext)
