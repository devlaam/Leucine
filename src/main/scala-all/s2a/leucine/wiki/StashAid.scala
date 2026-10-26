package s2a.leucine.wiki.StashAid

// Scastie: https://scastie.scala-lang.org/devlaam/twqeqasbTbCq8abTuDfrtQ

import s2a.leucine.actors.*

given actorContext: ActorContext = ActorContext.system


class Stack extends AcceptActor(Stack), StashAid, StateAid(Stack.initial) :

  def receive(letter: Letter, sender: Sender): Receive = letter match
    /* Handle the case a new number arrives. */
    case  Stack.Write(value)  =>
      /* If we are blocked and the number is even ... */
      if state.block && value%2==0
      /* ... store away this message*/
      then { Stash.store(); state }
      /* otherwise add it to the Stack .*/
      else state.copy(value::state.values)
    /* The block on the stash is released */
    case  Stack.Release  =>
      /* reprocess the stashed messages. */
      Stash.flush();
      /* Unblock the Stack */
      state.copy(block=false)
    /* We want to know the contents */
    case  Stack.Spool =>
      /* Show what we have got. */
      println(s"Stack content: ${state.show}")
      /* Return to the initial Stack State */
      Stack.initial


object Stack extends AcceptDefine :
  sealed trait Letter extends Actor.Letter[Actor]
  /* Write one integer to the stack.  */
  case class Write(value: Int) extends Letter
  /* Release the blocking of even numbers */
  case object Release extends Letter
  /* Print and clean the current Stack. */
  case object Spool extends Letter
  /* The State contains the Stack and the block situation.*/
  case class State(values: List[Int], block: Boolean) extends Actor.State :
    def show: String = values.mkString(",")
  /* We must always specify how to start. */
  val initial = Stack.State(Nil,true)


/* Main entry point for the demo. */
object Main :
  /* We must provide a default sender. */
  given Actor.Anonymous = Actor.Anonymous
  /* Create the Stack. */
  private val stack = new Stack

  def main(args: Array[String]): Unit =
    ( 1 to  8).foreach(i => stack ! Stack.Write(i))
    stack ! Stack.Release
    ( 9 to 12).foreach(i => stack ! Stack.Write(i))
    stack ! Stack.Spool
    stack.stop(Actor.Stop.Finish)
    Thread.sleep(200)
