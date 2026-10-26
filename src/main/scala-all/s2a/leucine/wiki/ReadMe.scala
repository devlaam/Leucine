package s2a.leucine.wiki.ReadMe

// Scastie: https://scastie.scala-lang.org/devlaam/HM4tZZDNQEWG1EBUzapjkA/1

import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*

given actorContext: ActorContext = ActorContext.system

/* Use an actor that accepts letters from anyone. */
class MyActor(name: String) extends AcceptActor(MyActor,name) :
  println(s"Actor $name started.")

  /* Handle all incoming letters. */
  protected def receive(letter: Letter, sender: Sender): Unit = letter match
    case MyActor.Text(data)   => println(s"Received text: $data.")
    case MyActor.Number(data) => println(s"Received number: $data.")

/* In the companion object define the base types. */
object MyActor extends AcceptDefine :
  /* Base type of all MyActor Letters, sealed to see if we handled them all. */
  sealed trait Letter extends Actor.Letter[Actor]
  /* Letter that sends some text */
  case class Text(data: String) extends Letter
  /* Letter that sends a number */
  case class Number(data: Int) extends Letter


object Main :
  def complete(): Unit = println("Demo Complete.")
  val myActor = MyActor("Test")

  def main(args: Array[String]): Unit =
    myActor ! MyActor.Text("Hello World")
    myActor ! MyActor.Number(42)
    myActor.stop(Actor.Stop.Finish)
    ActorGuard.watch(false,1.second,complete)
