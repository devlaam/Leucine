package s2a.leucine.wiki.LogAid4

// Scastie: (old version) https://scastie.scala-lang.org/devlaam/rnMaYfopQYaxAGF8ENR8Bg/17

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

object PongActor extends WideDefine
object PingActor extends WideDefine

object Letters :
  case object Ping  extends Actor.Letter[Actor]
  case object Pong  extends Actor.Letter[Actor]
  case object Start extends Actor.Letter[Actor]
  case object Stop  extends Actor.Letter[Actor]

object Logger extends ActorLogger, DefaultLoggerProcessing, ProductionLoggerSettings :
  import ActorLogger.Entry
  final override def process(entry: Entry) =
  JulLogSink.process(entry)


object JulLogSink :
  import java.util.logging.{ConsoleHandler, Formatter}
  import java.util.logging.Logger    as JulLogger
  import java.util.logging.LogRecord as JulRecord
  import java.util.logging.Level     as JulLevel
  import ActorLogger.Entry

  val logger = JulLogger.getLogger("example.Main")
  logger.setUseParentHandlers(false)
  logger.setLevel(JulLevel.ALL)

  final class ExtRecord(entry: Entry) extends JulRecord(JulLevel.parse(entry.level.julName),entry.message) :
    setSequenceNumber(entry.index)
    setMillis(entry.timestamp / 1000000)
    entry.thrown.foreach(throwable => setThrown(throwable))
    private val fields  = entry.getFields
    val counter = fields.getOrElse("counter", "")
    val channel = fields.getOrElse("channel", "")
    val thread  = fields.getOrElse("thread", "")
    val actor   = fields.getOrElse("actor", "")
    val kind    = fields.getOrElse("kind", "")
    val path    = fields.getOrElse("path", "")

  final class EntryFormatter extends Formatter:
    private def extendedFormat(record: ExtRecord): String =
      val idx = record.getSequenceNumber
      val msg = record.getMessage
      val ist = record.getInstant
      val lvl = s"%7s".format(record.getLevel.getName)
      val cnt = record.counter
      val chl = record.channel
      val thd = record.thread
      val act = record.actor
      val knd = record.kind
      val pth = record.path
      s"JUL($idx; $lvl; #$cnt; $ist; Channel($chl); Thread($thd); Actor($act); $knd($pth); $msg)%n".formatted()

    private def basicFormat(record: JulRecord): String =
      s"JUL(%7s; %s; %s)%n".format(record.getLevel,record.getInstant,record.getMessage).formatted()

    override def format(record: JulRecord): String = record match
      case r: ExtRecord => extendedFormat(r)
      case r: JulRecord => basicFormat(r)

  val handler = new ConsoleHandler()
  handler.setLevel(JulLevel.ALL)
  handler.setFormatter(new EntryFormatter)
  logger.addHandler(handler)

  def process(entry: Entry): Unit = logger.log(ExtRecord(entry))


object Main :
  ActorGuard.register(Logger)

  val pingActor = new PingActor
  val pongActor = new PongActor

  def main(args: Array[String]): Unit =
    pingActor.send(Letters.Start,pongActor):Unit
    ActorGuard.watch(false,1.seconds)
    println("Main Done")

