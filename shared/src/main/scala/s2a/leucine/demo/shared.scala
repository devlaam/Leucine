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



private class Console(val name: String) extends BasicActor[Console.Letter] :

  CLI.talk("Please state the demo you want to run (ticker, server or crawler): ",answer => this ! Console.Read(answer))

  def stop(goodbye: String = ""): Unit =
    if !goodbye.isEmpty then println(goodbye)
    CLI.close()
    stopDirect()

  def start(actor: Actor.Any): Unit =
    ActorGuard.add(actor)
    stop()

  def receive(letter: Console.Letter) = letter match
    case Console.Read("ticker")  =>  start(new Ticker)
    case Console.Read("server")  =>  start(new Server)
    case Console.Read("crawler") =>  start(new Tree("F0",None))
    case Console.Read(unknown)   =>  stop(s"Unknown demo '$unknown', closing ...")


object Console :
  sealed trait Letter extends Actor.Letter
  case class Read(text: String) extends Letter



object Init extends LogInfo:
  import PlatformContext.Platform

  /* When you arrive here, you can be certain all actors are done */
  def complete(): Unit = println("Demo complete")

  @main
  def main(): Unit =
    println(s"Started Actor examples on the ${actorContext.platform} platform.")
    val console = Console("Console")
    ActorGuard.add(console)
    /* Watch the actors for completion. Note that this blocks for the JVM and Native platforms
     * but not on JS. There blocking is not possible and this call returns immedeately. Normally
     * you do not call watch on JS, for this does have not much added value. */
    ActorGuard.watch(false,complete,10.seconds)
    /* The application should exit, but sometimes on the JVM this is needed, i do not no why */
    //System.exit(0)

