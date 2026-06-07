package s2a.leucine.wiki.logger.basic
/* https://scastie.scala-lang.org/devlaam/rnMaYfopQYaxAGF8ENR8Bg/18 */

import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*

given ActorContext = ActorContext.system

class PingActor extends WideActor(PingActor,"Ping"), LogAid(Logger) :
  def receive(letter: Letter, sender: Sender): Receive = (letter,sender) match
    case (Letters.Start, source: PongActor) =>
      Logger.info("Ping received Start")
      source ! Letters.Ping
    case (Letters.Pong, source: PongActor) =>
      Logger.info("Ping received Pong")
      source ! Letters.Stop
    case (Letters.Stop, _) =>
      Logger.warn("Ping received Stop")
      stop(Actor.Stop.Direct)

class PongActor extends WideActor(PongActor,"Pong"), LogAid(Logger) :
  def receive(letter: Letter, sender: Sender): Receive = (letter,sender) match
    case (Letters.Ping, source: PingActor) =>
      Logger.info("Pong received Ping")
      source ! Letters.Pong
    case (Letters.Stop,source: PingActor) =>
      source ! Letters.Stop
      Logger.warn("Pong received Stop")
      stop(Actor.Stop.Direct)

object PongActor extends WideDefine, Stateless
object PingActor extends WideDefine, Stateless

object Letters :
  case object Ping  extends Actor.Letter[Actor]
  case object Pong  extends Actor.Letter[Actor]
  case object Start extends Actor.Letter[Actor]
  case object Stop  extends Actor.Letter[Actor]

object Logger extends ActorLogger, DefaultLoggerProcessing, ProductionLoggerSettings

object Main :
  ActorGuard.register(Logger)

  val pingActor = new PingActor
  val pongActor = new PongActor

  def main(args: Array[String]): Unit =
    pingActor.send(Letters.Start,pongActor):Unit
    ActorGuard.watch(false,1.seconds)
