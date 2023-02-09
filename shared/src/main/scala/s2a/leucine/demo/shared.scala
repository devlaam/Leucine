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

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

import s2a.leucine.actors.*

/* Note: The examples are given to illustrate how the actors could be used, and are
 * not meant complete or even suited any particular situation. Just use these as a
 * starting point for you own application. */

/* The default actor context for these examples */
given actorContext: ActorContext = ActorContext.system


object Main extends LogInfo:
  import PlatformContext.Platform

  def startTicker(): Unit =
    val ticker = new Ticker
    /* Put it under guard. Since the Provider actors are in the Server family we do not need a guard for these. */
    ActorGuard.add(ticker)

  def startServer(): Unit =
    /* Create the root of the family */
    val server = new Server
    /* Put it under guard. Since the Provider actors are in the Server family we do not need a guard for these. */
    ActorGuard.add(server)

  def startCrawler(): Unit =
    println("To be implemented!")

  def runtimeDef(args: Array[String]): Unit =
    args.map(_.toLowerCase) match
     case Array("ticker")  => startTicker()
     case Array("server")  => startServer()
     case Array("crawler") => startCrawler()
     case _                => println("Please specify 'ticker', 'server' or 'crawler' as argument at startup.")

  def compiletimeDef(): Unit =
    println("Platform does not support command line parameters.")
    println("If you want an other demo, change the source.")
    /* Choose your demo here. */
    startTicker()

  def main(args: Array[String]): Unit =
    val welcome = s"Started Actor examples on the ${actorContext.platform} platform."
    Logger.info(welcome)
    println(welcome)
    /* See on which platform we are. */
    actorContext.platform match
      /* On the JVM you can specify the demo at runtime */
      case Platform.JVM    => runtimeDef(args)
      /* On JS, no command line parameters are possible, specify at compile time. */
      case Platform.JS     => compiletimeDef()
      /* On the JVM you can specify the demo at runtime */
      case Platform.Native => runtimeDef(args)

    /* Let the user choose one of the demo's. */
    /* Wait until the actor system stops it activity. The application closes after this. */
    ActorGuard.watch(false,10.seconds)

