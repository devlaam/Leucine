package s2a.leucine.wiki.LogAid3

// Scastie: (old version) https://scastie.scala-lang.org/devlaam/rnMaYfopQYaxAGF8ENR8Bg/13

import scala.jdk.CollectionConverters._
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
  LogbackLogSink.process(entry)

object LogbackLogSink :
  import ch.qos.logback.classic.LoggerContext
  import ch.qos.logback.classic.Level as LBlevel
  import ch.qos.logback.classic.encoder.PatternLayoutEncoder
  import ch.qos.logback.classic.spi.ILoggingEvent
  import ch.qos.logback.classic.spi.LoggingEvent
  import ch.qos.logback.core.ConsoleAppender

  import ActorLogger.Entry

  private val loggerPattern = """LgB\(%X{index}; %5level; #%X{counter}; %d; Channel\(%X{channel}\); Thread\(%thread\); Actor\(%X{actor}\); %X{kind}\(%X{path}\); %msg\)%n"""

  private val context = new LoggerContext()
  context.start()

  private val encoder = new PatternLayoutEncoder()
  encoder.setContext(context)
  encoder.setPattern(loggerPattern)
  encoder.start()

  private val appender = new ConsoleAppender[ILoggingEvent]()
  appender.setContext(context)
  appender.setEncoder(encoder)
  appender.start()

  private val logger = context.getLogger("example.Main")
  logger.setLevel(LBlevel.TRACE)
  logger.setAdditive(false)
  logger.addAppender(appender)

  def process(entry: Entry): Unit =
    val level = LBlevel.toLevel(entry.level.lgbName)
    val event = new LoggingEvent()
    event.setLoggerContext(context)
    event.setLevel(level)
    event.setSequenceNumber(entry.index)
    event.setMessage(entry.message)
    event.setThreadName(entry.threadName)
    event.setTimeStamp(entry.timestamp / 1000000)
    event.setMDCPropertyMap(entry.getFields.asJava)
    event.prepareForDeferredProcessing()
    logger.callAppenders(event)


object Main :
  ActorGuard.register(Logger)

  val pingActor = new PingActor
  val pongActor = new PongActor

  def main(args: Array[String]): Unit =
    pingActor.send(Letters.Start,pongActor):Unit
    ActorGuard.watch(false,1.seconds)
    println("Main Done")
