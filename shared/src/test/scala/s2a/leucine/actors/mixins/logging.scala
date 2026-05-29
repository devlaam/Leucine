package s2a.leucine.actors


import scala.concurrent.duration.DurationInt
import scala.annotation.unused
import utest.*


import s2a.control.{Buffer, Deferred}

/** Special Settings for this test */
trait TestLoggerSettings :
  import ActorLogger.{Level, Timing, ShowChannels, Spooling, Filter}
  import ActorLogger.Channel.{SysPrd, AppPrd, AppDvl}

  final val fullPath         = false
  final val fullParameters   = true
  final val showConfidential = true
  final val filter           = Filter.Pass
  final val showChannels     = ShowChannels((SysPrd, AppPrd, AppDvl))
  final val spooling         = Spooling.Periodic(10,5.seconds,Level.Error)
  final val timing           = Timing.Nanos
  final val incidentLevel    = Level.Warn
  final val localSettings    = true


/* Processing part of Logger that provides storage for the logs at a fixed log level. */
trait TestLoggerProcessing(level: ActorLogger.Level, buffer: Buffer[String]) :
  import ActorLogger.{Level, Entry, Channel}
  import LogHolder.{Hold}

  private object spoolGuard

  /* Create for every demo a separate group for logging. We shall use this only for tracing. */
  object ChanA  extends Channel
  object ChanB  extends Channel

  def retrieve(): Hold[List[Entry]]
  def spool(@unused completed: Boolean): Unit =
    spoolGuard.synchronized :
      ActorLogger.simpleSpool(retrieve(),process)

  final val passLevel: Level = level
  def process(entry: Entry): Unit = buffer.writeln(entry.message)
  def appFatal(message: String): Unit = buffer.writeln(message.toUpperCase())
  def sysFatal(message: String): Unit = buffer.writeln(message.toUpperCase())

object ActorLoggerTest extends TestSuite :
  import ActorLogger.Level

  implicit val ac: ActorContext = ActorContext.system

  /* Logger object for testing the basics. With a type setting for the level at compile time and
   * a parameter setting for the level at runtime. */
  abstract class BaseLogger(rtLevel: Level, buffer: Buffer[String]) extends ActorLogger, TestLoggerSettings, TestLoggerProcessing(rtLevel,buffer)

  /* Loggers with different settings for the fixed level. These have all to be spelled out, for the
   * the fixLevel must be a constant type in order for the inline expressions to work. */
  class FatalLogger(rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Fatal }
  class ErrorLogger(rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Error }
  class WarnLogger (rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Warn }
  class InfoLogger (rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Info }
  class BetaLogger (rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Beta }
  class DebugLogger(rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Debug }
  class TraceLogger(rtLevel: Level, buffer: Buffer[String]) extends BaseLogger(rtLevel,buffer) { final val fixPassLevel = Level.Trace }

  /* Base writer to handle the common writer tasks. */
  abstract class BaseWriter[Logger <: BaseLogger](name: String, writeln: String => Unit, done: () => Unit, logger: Logger) extends AcceptActor(Writer,name), LogAid(logger) :
    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      logger.stop(true)
      /* The call to done MUST be after after the spooling in logger.stop. The most easy way to
       * achieve this is to push the task to the same executor as the spool. */
      context.sequential(done())
    override protected def except(letter: MyLetter[Accept], sender: Sender, cause: Exception, size: Int) = writeln(s"except(${cause.getMessage()},$size)")
    inline def process(letter: Writer.Letter): Unit = letter match
      case  Writer.Fatal   => logger.fatal(s"fatal($name)")
      case  Writer.Error   => logger.error(s"error($name)")
      case  Writer.Warn    => logger.warn(s"warn($name)")
      case  Writer.Info    => logger.info(s"info($name)")
      case  Writer.Beta    => logger.beta(s"beta($name)")
      case  Writer.Debug   => logger.debug(s"debug($name)")
      case  Writer.Trace   => logger.trace(s"trace($name)")

  /* Also here, to test we must construct a different writer for each FixPassLevel in order to be able to
   * inline the logger calls. */
  class FatalWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: FatalLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  class ErrorWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: ErrorLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  class WarnWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: WarnLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  class InfoWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: InfoLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  class BetaWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: BetaLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  class DebugWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: DebugLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  class TraceWriter(name: String, val writeln: String => Unit, val done: () => Unit, logger: TraceLogger) extends BaseWriter(name,writeln,done,logger):
    def receive(letter: Writer.Letter, sender: Sender): Unit = process(letter)

  object Writer extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Fatal extends Letter
    case object Error extends Letter
    case object Warn  extends Letter
    case object Info  extends Letter
    case object Beta  extends Letter
    case object Debug extends Letter
    case object Trace extends Letter

  def sendAllMessages(writer: BaseWriter[?]) =
    writer ! Writer.Fatal
    writer ! Writer.Error
    writer ! Writer.Warn
    writer ! Writer.Info
    writer ! Writer.Beta
    writer ! Writer.Debug
    writer ! Writer.Trace
    writer.stop(Actor.Stop.Finish)

  val tests = Tests :

    test("send a all log messages on fatal/trace level.") :
      val buffer = Buffer[String]("fatal/trace")
      val deferred = Deferred(buffer.readlns)
      val logger = FatalLogger(Level.Trace,buffer)
      val writer = FatalWriter("fatal/trace",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(FATAL/TRACE)","fatal(fatal/trace)"))

    test("send a all log messages on error/trace level.") :
      val buffer = Buffer[String]("error/trace")
      val deferred = Deferred(buffer.readlns)
      val logger = ErrorLogger(Level.Trace,buffer)
      val writer = ErrorWriter("error/trace",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(ERROR/TRACE)","error(error/trace)","fatal(error/trace)"))

    test("send a all log messages on error/fatal level.") :
      val buffer = Buffer[String]("error/fatal")
      val deferred = Deferred(buffer.readlns)
      val logger = ErrorLogger(Level.Fatal,buffer)
      val writer = ErrorWriter("error/fatal",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(ERROR/FATAL)","fatal(error/fatal)"))

    test("send a all log messages on warn/trace level.") :
      val buffer = Buffer[String]("warn/trace")
      val deferred = Deferred(buffer.readlns)
      val logger = WarnLogger(Level.Trace,buffer)
      val writer = WarnWriter("warn/trace",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(WARN/TRACE)","warn(warn/trace)","error(warn/trace)","fatal(warn/trace)"))

    test("send a all log messages on warn/error level.") :
      val buffer = Buffer[String]("warn/error")
      val deferred = Deferred(buffer.readlns)
      val logger = WarnLogger(Level.Error,buffer)
      val writer = WarnWriter("warn/error",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(WARN/ERROR)","error(warn/error)","fatal(warn/error)"))

    test("send a all log messages on info/trace level.") :
      val buffer = Buffer[String]("info/trace")
      val deferred = Deferred(buffer.readlns)
      val logger = InfoLogger(Level.Trace,buffer)
      val writer = InfoWriter("info/trace",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(INFO/TRACE)","info(info/trace)","warn(info/trace)","error(info/trace)","fatal(info/trace)"))

    test("send a all log messages on info/error level.") :
      val buffer = Buffer[String]("info/error")
      val deferred = Deferred(buffer.readlns)
      val logger = InfoLogger(Level.Error,buffer)
      val writer = InfoWriter("info/error",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(INFO/ERROR)","error(info/error)","fatal(info/error)"))

    test("send a all log messages on beta/trace level.") :
      val buffer = Buffer[String]("beta/trace")
      val deferred = Deferred(buffer.readlns)
      val logger = BetaLogger(Level.Trace,buffer)
      val writer = BetaWriter("beta/trace",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(BETA/TRACE)","beta(beta/trace)","info(beta/trace)","warn(beta/trace)","error(beta/trace)","fatal(beta/trace)"))

    test("send a all log messages on beta/warn level.") :
      val buffer = Buffer[String]("beta/warn")
      val deferred = Deferred(buffer.readlns)
      val logger = BetaLogger(Level.Warn,buffer)
      val writer = BetaWriter("beta/warn",buffer.writeln,deferred.done,logger)
      logger.start(true)
      sendAllMessages(writer)
      deferred.await()
      deferred.compare(_.toSet ==> Set("FATAL(BETA/WARN)","warn(beta/warn)","error(beta/warn)","fatal(beta/warn)"))

