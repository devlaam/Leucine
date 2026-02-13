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

import scala.concurrent.duration.DurationInt
import scala.collection.mutable.Buffer
import s2a.leucine.actors.*

/* Note: The examples are given to illustrate how the actors could be used, and are
 * not meant complete or even suited any particular situation. Just use these as a
 * starting point for you own application. */

/* The default actor context for these examples */
given actorContext: ActorContext = ActorContext.system

/* This is our logging object to be used for all demo applications. Experiment with mixing
 * in the different LoggerSettings, or changing some settings below. */
object Logger extends ActorLogger, DevelopmentLoggerSettings, DefaultLoggerProcessing :
  import ActorLogger.{Entry, Level, ShowGroups, GroupBase}

  /* Create for every demo a separate group for logging. We shall use this only for tracing. */
  object GroupChat    extends GroupBase
  object GroupClock   extends GroupBase
  object GroupCollatz extends GroupBase
  object GroupCrawler extends GroupBase

  /* Experiment here to see the effects of including and excluding groups. */
  transparent inline def showGroups = ShowGroups((GroupChat,GroupClock,GroupCollatz,GroupCrawler))

  /* Experiment here to see the effects of filter defined on the sourcePath or actorPath */
  def sourcePathFilter(level: Level, path: String): Boolean = true
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** Set DirectSpool to false to ensure all logs pass the thread local entry collectors. */
  inline def directSpool = false

  /* We want to set the used pass level at the start of the application via its arguments. But we
   * also want the logger to be available as global object. So we must manipulate the value after
   * construction for this demonstration code. We may therefore have a mismatch of levels at the
   * very start, for it is uncertain when the change will be picked up. We start with "Level.Trace"
   * so we don't miss any entries, but some Trace messages will also appear before the new level is
   * accepted. For a demo, this is acceptable. */
  private var level: Level = Level.Trace
  def setLevel(level: Level): Unit = this.level = level

  /** This is called to obtain the current logging level. */
  override def passLevel: Level = level

  /* In this demo we want all the logging to appear at the end. So we buffer it until the
   * application is about to close. Then it is displayed. This prohibits mixing the output
   * of the demo with logging. */
  private val logEntries: Buffer[String] = Buffer.empty

  /* For each call to the logger, the entry is stored. */
  override def process(entry: Entry): Unit = logEntries += entry.toString

  /* Called to display all the entries, preferably at the end. */
  def printEntries(): Unit = logEntries.foreach(println)




object Init :
  import ActorLogger.Level
  Logger.trace(Logger.AllGroups)

  /** When you arrive here, you can be certain all actors are done */
  def complete(): Unit =
    Logger.printEntries()
    println("Demo complete")

  /** Entry point of the demo application. */
  def main(args: Array[String]): Unit =
    /* Trace the main entry. Note that we may use the logger before it is started below. Starting
     * the logger only starts the spooling, log entries are always recorded. */
    Logger.trace(Logger.AllGroups)
    /* Set the logger level based upon arguments at startup. Note that, due to the required platform
     * independence, we must perform some extra magic to get the arguments. CLI.argsOf(..) takes care
     * of that. Without arguments, no logging, otherwise try to interpret the first argument as log
     * level. If this fails: no logging. */
    Logger.setLevel(CLI.argsOf(args).headOption.flatMap(Level.fromString).getOrElse(Level.Disable))
    /* Register the Logger so that the ActorGuard can start (and stop) it, if not disabled. */
    if Logger.passLevel > Level.Disable then ActorGuard.register(Logger)
    /* Register the Monitor so that the ActorGuard can start (and stop) it. */
    ActorGuard.register(Monitor)
    Logger.info("Main called")
    println(s"Started Actor examples on the ${actorContext.platform} platform.")
    /* Define a handler for unhandled messages */
    ActorGuard.failed(post => println(s"FAILED MESSAGE: ${post.full}"))
    /* Open the console as an actor to get commands from the user. */
    new Console
    /* Watch the actors for completion. Note that this blocks for the JVM and Native platforms
     * but not on JS. There blocking is not possible and this call returns immediately. Normally
     * you do not call watch on JS, for this does have not much added value. Except if you use
     * stop(Silent) somewhere. */
    ActorGuard.watch(false,3.seconds,complete)
    /* The application should exit when all work is done. */
