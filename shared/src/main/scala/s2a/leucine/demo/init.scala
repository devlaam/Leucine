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
import s2a.leucine.actors.*

/* Note: The examples are given to illustrate how the actors could be used, and are
 * not meant complete or even suited any particular situation. Just use these as a
 * starting point for you own application. */

/* The default actor context for these examples */
given actorContext: ActorContext = ActorContext.system

/* This is our logging object to be used for all demo applications. Experiment with mixing
 * in the different LoggerSettings, or changing some settings below. */
object Logger extends ActorLogger, DevelopmentLoggerSettings, DefaultLoggerProcessing :
  import ActorLogger.{Level, ShowGroups, GroupBase}

  /* Create for every demo a separate group for logging. We shall use this only for tracing. */
  object GroupChat    extends GroupBase
  object GroupClock   extends GroupBase
  object GroupTicker  extends GroupBase
  object GroupCrawler extends GroupBase

  /* Experiment here to see the effects of including and excluding groups. */
  transparent inline def showGroups = ShowGroups((GroupChat,GroupClock,GroupTicker,GroupCrawler))

  /* Experiment here to see the effects of filter defined on the sourcePath or actorPath */
  def sourcePathFilter(level: Level, path: String): Boolean = true
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** Set DirectSpool to false to ensure all logs pass the thread local entry collectors. */
  inline def directSpool = false

  /**  Per default the pass level is Trace, you can define a new one by overriding this: */
  final override val passLevel: Level = Level.Info


object Init :
  Logger.trace(Logger.AllGroups)
  /* When you arrive here, you can be certain all actors are done */
  def complete(): Unit = println("Demo complete")

  @main
  def main(): Unit =
    /* Trace the main entry. Note that we may use the logger before it is started below. Starting
     * the logger only starts the spooling, log entries are always recorded. */
    Logger.trace(Logger.AllGroups)
    /* Register the Logger and Monitor so that the ActorGuard can start and stop them. */
    ActorGuard.register(Logger)
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
