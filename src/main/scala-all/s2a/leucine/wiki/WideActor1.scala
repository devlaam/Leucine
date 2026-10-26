package s2a.leucine.wiki.WideActor1

// Scastie: https://scastie.scala-lang.org/devlaam/4XPhF7bjQNCeZynC6VkuPg

import s2a.leucine.actors.*

given ActorContext = ActorContext.system


object Letters :
  case object Ping  extends Actor.Letter[Actor]
  case object Pong  extends Actor.Letter[Actor]
  case object Start extends Actor.Letter[Actor]
  case object Stop  extends Actor.Letter[Actor]


class PingActor extends WideActor(PingActor), StateAid(PingActor.State.WaitForStart) :
  import PingActor.State.{WaitForStart,WaitForPong,WaitForStop}
  def receive(letter: Letter, sender: Sender): Receive = (letter,sender) match
    case (Letters.Start, source: PongActor) if state == WaitForStart =>
      println("Ping received Start")
      source ! Letters.Ping
      WaitForPong
    case (Letters.Pong, source: PongActor) if state == WaitForPong =>
      println("Ping received Pong")
      source ! Letters.Ping
      WaitForStop
    case (Letters.Stop, _) if state == WaitForStop =>
      println("Ping received Stop")
      stop(Actor.Stop.Direct)
      WaitForStart
    case _ =>
      println(s"Unhandled letter: $letter, from $sender, while: $state")
      state

object PingActor extends WideDefine :
  enum State extends Actor.State { case WaitForStart, WaitForPong, WaitForStop }


class PongActor extends WideActor(PongActor) :
  def receive(letter: Letter, sender: Sender): Receive = (letter,sender) match
    case (Letters.Ping, source: PingActor) =>
      println("Pong received Ping")
      source ! Letters.Pong
    case (Letters.Stop,_) =>
      println("Pong received Stop")
      stop(Actor.Stop.Direct)

object PongActor extends WideDefine


object Main :
  given Actor.Anonymous = Actor.Anonymous
  val pingActor = new PingActor
  val pongActor = new PongActor

  def main(args: Array[String]): Unit =
    val success = pingActor.send(Letters.Start,pongActor)
    if !success then println("Letter not accepted by pingActor")
    Thread.sleep(200)
    pingActor ! Letters.Stop
    pongActor ! Letters.Stop
