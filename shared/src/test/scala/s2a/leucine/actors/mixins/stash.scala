package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.actors.PlatformContext.Platform

object StashActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Stack(val writeln: String => Unit, val done: () => Unit) extends StateActor[Stack.Letter,Actor,Stack.State], StashActor :
    val name = "Stack"
    override protected def stopped(complete: Boolean) =
      writeln(s"stop:$complete")
      done()
    protected def initial = Stack.State(Nil,true)
    def receive(letter: Stack.Letter, sender: Sender, state: Stack.State) = letter match
      case  Stack.Write(value)  => if state.block && value%2==0 then { Stash.store(); state } else state.copy(value::state.values)
      case  Stack.Push(value)   => Stash.store(Stack.Write(value),sender); state
      case  Stack.Pop           => Stash.flush(); state.copy(block=false)
      case  Stack.Print         => writeln(state.values.toString); initial

  object Stack :
    sealed trait Letter extends Actor.Letter
    case class Write(value: Int) extends Letter
    case class Push(value: Int) extends Letter
    case object Pop extends Letter
    case object Print extends Letter

    case class State(values: List[Int], block: Boolean) extends Actor.State

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
      stack.stopFinish()
      deferred.await()
      deferred.compare(_ ==> expect) } }

