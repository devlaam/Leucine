package s2a.leucine.wiki.WideActor2

// Scastie: https://scastie.scala-lang.org/devlaam/4XPhF7bjQNCeZynC6VkuPg/1

import s2a.leucine.actors.*

given ActorContext = ActorContext.system

class PingActor extends RestrictActor(PingActor), StateAid(PingActor.State.WaitForStart) :
  import PingActor.State.{WaitForStart,WaitForPong,WaitForStop}
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Receive = (letter,sender) match
    case (PingActor.Start, source: PongActor) if state == WaitForStart =>
      println("Ping received Start")
      source ! PongActor.Ping
      WaitForPong
    case (PingActor.Pong, source: PongActor) if state == WaitForPong =>
      println("Ping received Pong")
      source ! PongActor.Ping
      WaitForStop
    case (PingActor.Stop, _) if state == WaitForStop =>
      println("Ping received Stop")
      stop(Actor.Stop.Direct)
      WaitForStart
    case _ =>
      println(s"Unhandled letter: $letter, from $sender, while: $state")
      state

object PingActor extends RestrictDefine :
  type Accept = PongActor | Anonymous
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  case object Pong  extends Letter[PongActor]
  case object Start extends Letter[PongActor]
  case object Stop  extends Letter[Anonymous]
  enum State extends Actor.State { case WaitForStart, WaitForPong, WaitForStop }


class PongActor extends RestrictActor(PongActor) :
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Receive = (letter,sender) match
    case (PongActor.Ping, source: PingActor) =>
      println("Pong received Ping")
      source ! PingActor.Pong
    case (PongActor.Stop,_) =>
      println("Pong received Stop")
      stop(Actor.Stop.Direct)
    case (_,_) => unmatched(letter,sender)

object PongActor extends RestrictDefine :
  type Accept = PingActor | Anonymous
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  case object Ping extends Letter[PingActor]
  case object Stop extends Letter[Anonymous]


object Main :
  given Actor.Anonymous = Actor.Anonymous

  val pingActor = new PingActor
  val pongActor = new PongActor

  def main(args: Array[String]): Unit =
    val success = pingActor.send(PingActor.Start,pongActor)
    if !success then println("Letter not accepted by pingActor")
    Thread.sleep(200)
    pingActor ! PingActor.Stop
    pongActor ! PongActor.Stop
