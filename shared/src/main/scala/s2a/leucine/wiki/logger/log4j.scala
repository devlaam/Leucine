package s2a.leucine.wiki.logger.log4j
/* https://scastie.scala-lang.org/devlaam/rnMaYfopQYaxAGF8ENR8Bg/16 */

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

object PongActor extends WideDefine, Stateless
object PingActor extends WideDefine, Stateless

object Letters :
  case object Ping  extends Actor.Letter[Actor]
  case object Pong  extends Actor.Letter[Actor]
  case object Start extends Actor.Letter[Actor]
  case object Stop  extends Actor.Letter[Actor]

object Logger extends ActorLogger, DefaultLoggerProcessing, ProductionLoggerSettings :
  import ActorLogger.{Entry,Level,Timing}
  final override def process(entry: Entry) =
  Log4jLogSink.process(entry)


object Log4jLogSink :
  import org.apache.logging.log4j.Level as L4JLevel
  import org.apache.logging.log4j.message.SimpleMessage
  import org.apache.logging.log4j.core.LoggerContext
  import org.apache.logging.log4j.core.config.Configurator
  import org.apache.logging.log4j.core.config.builder.api.ConfigurationBuilderFactory
  import org.apache.logging.log4j.core.impl.ContextDataFactory
  import org.apache.logging.log4j.core.impl.Log4jLogEvent

  import ActorLogger.Entry

  private val loggerName = "example.Main"
  private val loggerPattern = "L4J(%X{index}; %5level; #%X{counter}; %d; Channel(%X{channel}); Thread(%t); Actor(%X{actor}); %X{kind}(%X{path}); %msg)%n"

  private val builder  = ConfigurationBuilderFactory.newConfigurationBuilder()
  builder.setStatusLevel(L4JLevel.ERROR)

  private val layout   = builder.newLayout("PatternLayout").addAttribute("pattern",loggerPattern)
  private val appender = builder.newAppender("Console", "CONSOLE").add(layout)
  builder.add(appender)

  private val logger   = builder.newRootLogger(L4JLevel.TRACE).add(builder.newAppenderRef("Console"))
  builder.add(logger)

  private val context  = Configurator.initialize(builder.build()).asInstanceOf[LoggerContext]
  private val config   = context.getConfiguration.getLoggerConfig(loggerName)

  def process(entry: Entry): Unit =
    val level   = L4JLevel.forName(entry.level.l4jName, entry.level.l4jPrio)
    val data    = ContextDataFactory.createContextData(entry.getFields.asJava)
    val message = new SimpleMessage(entry.message)
    val event = Log4jLogEvent
      .newBuilder()
      .setLoggerName(loggerName)
      .setLevel(level)
      .setMessage(message)
      .setThreadName(entry.threadName)
      .setTimeMillis(entry.timestamp / 1000000)
      .setContextData(data)
      .setIncludeLocation(false)
      .build()
    config.log(event)


object Main :
  ActorGuard.register(Logger)

  val pingActor = new PingActor
  val pongActor = new PongActor

  def main(args: Array[String]): Unit =
    pingActor.send(Letters.Start,pongActor):Unit
    ActorGuard.watch(false,1.seconds)
    println("Main Done")
