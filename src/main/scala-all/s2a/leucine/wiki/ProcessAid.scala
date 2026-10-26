package s2a.leucine.wiki.ProcessAid

// Scastie: https://scastie.scala-lang.org/devlaam/mFXyFOzlSdu5uyzwCw4KFw

import s2a.leucine.actors.*

given actorContext: ActorContext = ActorContext.system


class PingActor extends RestrictActor(PingActor), RestrictProcess :

  /* It is good custom to give each partial function a descriptive name
   * otherwise every method is called process somehow ;) */
  def process[Sender <: Accept]: Process[Sender] = waitForStart

  def waitForStart[Sender <: Accept]: Process[Sender] =
    case (PingActor.Start, source: PongActor) =>
      println("Ping received Start")
      source ! PongActor.Ping
      switch(new Process.Replace { def process[Sender <: Accept] = waitForPong } )

  def waitForPong[Sender <: Accept]: Process[Sender] =
    case (PingActor.Pong, source: PongActor) =>
      println("Ping received Pong")
      source ! PongActor.Ping
      switch(new Process.Replace { def process[Sender <: Accept] = waitForStop } )

  def waitForStop[Sender <: Accept]: Process[Sender] =
    case (PingActor.Stop, _: Anonymous) =>
      println("Ping received Stop")
      stop(Actor.Stop.Direct)

object PingActor extends RestrictDefine :
  type Accept = PongActor | Anonymous
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  case object Pong  extends Letter[PongActor]
  case object Start extends Letter[PongActor]
  case object Stop  extends Letter[Anonymous]


class PongActor extends RestrictActor(PongActor) :
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Receive = (letter,sender) match
    case (PongActor.Ping, source: PingActor) =>
      println("Pong received Ping")
      source ! PingActor.Pong
    case (PongActor.Stop, _) =>
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
    pingActor.send(PingActor.Start,pongActor):Unit
    Thread.sleep(200)
    pingActor ! PingActor.Stop
    pongActor ! PongActor.Stop
