package s2a.leucine.demo

/**
 * MIT License
 *
 * Copyright (c) 2023 Ruud Vlaming
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

import s2a.leucine.actors.*


/**
 * LogData provides context specific information about the location where
 * the log entry was made. This can be anything like the class name, actor path
 * etc. Extend to your own liking. */
private class LogData(val value: () => String)


private object LogData :
  /* In case such information is not available, empty is used. */
  val empty = LogData(() => "---")


/* This is the trait you mixin with your class if you need extra context
 * information in the logging. Note, The Logger will also work without. */
trait LogInfo :

  /* data contains the function the gathers the information on request.
   * This is needed for two reasons:
   * (1) The info may not be available at the time of construction of this instance.
   * (2) If the logging is ditched, we can prevent its execution.  */
  private val data = this match
    case ba: BareActor => () => s"actor:${ba.path}"
    case _             => () => s"class:${getClass.getSimpleName}"

  /* Finally this is the object picked up by the Logger methods. */
  given LogData(data)


/**
 * The logger will only receive messages and never send one. Also, we do not
 * care from whom the message is originating. */
private class Logger extends AcceptActor(Logger,"logger") :
  import Logger.*

  /* Soft level of the messages that are printed. Can be changed by during runtime. */
  private var level: Level = Level.Debug

  /* Report that this logger has been disabled. */
  final protected override def stopped(cause: Actor.Stop, complete: Boolean) = println("Stopped Logger")

  /* Report that this logger has started. */
  println("Started Logger")

  /* receive method that handles the incoming logger and control messages. */
  final protected def receive(letter: Letter, sender: Sender): Unit = letter match
    case msg: Message => if msg.level <= this.level then println(msg.show)
    case Switch(level: Level) => this.level = level
    case Stop                 => stop(Actor.Stop.Direct)


object Logger extends AcceptDefine, Stateless:
  import PlatformContext.Platform
  import Auxiliary.toUnit

  /* This are the levels the logger is able to handle. */
  enum Level extends EnumOrder[Level] :
    case Error, Warning, Info, Debug

  /* Make this class sealed so that the compile can check at the receiver method if
   * we were complete in the implementation of all message types */
  sealed trait Letter extends Actor.Letter[Actor]

  /* See if we are running in a multithreaded environment. */
  val multithreaded: Boolean = actorContext.platform match
    case Platform.JVM    => true
    case Platform.JS     => false
    case Platform.Native => false

  /* Class to send a log message to the logger. */
  private case class Message(level: Level, data: LogData, text: String) extends Letter :
    /* Automatically timestamp the message at creation. */
    private val timeStamp = Time.nowUTC
    /* Find in which thread this was running if relevant. */
    private val threadStamp = if multithreaded then Thread.currentThread().getName() else ""
    /* Make a reasonable entry for this log message. This is done in the Logger Actor context. */
    def show: String = s"$timeStamp; $threadStamp; ${data.value()}; $level; $text"

  /* Message to dynamically switch the level of the messages. Note that this is
   * expensive since the messages are send to the logger actor anyway. Better is
   * to set this at program start. */
  private case class Switch(level: Level) extends Letter

  /* Object to stop the logger. */
  private case object Stop extends Letter

  /* The actual logger is hidden from the user. */
  private val logger = new Logger

  /* The logger may stop if there is nothing left to log. */
  logger.stop(Actor.Stop.Final)

  /* Hard wired minimum level for logging. This is handy to quickly decide what to ignore.
   * This can be changed in something that is decided at program start. */
  val level: Level = Level.Debug

  /* Below are the methods that you use in your program. So for the user these feel just like
   * ordinary log calls. Under the hood they are handled by the actor so that the normal flow
   * of the program can continue.  */

  /** Level to report severe problems. */
  def error(text: => String)(using data: LogData = LogData.empty): Unit =
    if Level.Error <= level then logger.send(Message(Level.Error,data,text)).toUnit

  /** Level to report problems that require attention. */
  def warn(text: => String)(using data: LogData = LogData.empty): Unit =
    if  Level.Warning <= level then logger.send(Message(Level.Warning,data,text)).toUnit

  /** Level to just inform want is going down. */
  def info(text: => String)(using data: LogData = LogData.empty): Unit =
    if Level.Info <= level then logger.send(Message(Level.Info,data,text)).toUnit

  /** Level to report inside information to correct and improve your code. */
  def debug(text: => String)(using data: LogData = LogData.empty): Unit =
    if Level.Debug <= level then logger.send(Message(Level.Debug,data,text)).toUnit

  /** Change the log level */
  def switch(level: Level): Unit = logger.send(Switch(level)).toUnit

  /** Stop the logger */
  def stop(): Unit = logger.send(Stop).toUnit




