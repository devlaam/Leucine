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

/* The console is also organised as actor, which makes sense, since it must run independently from the application. */
private class Console(val name: String) extends BasicActor[Console.Letter] :

  /* The welcome message. You may choose your demo. */
  CLI.talk("Please state the demo you want to run (ticker, server or crawler): ",answer => this ! Console.Read(answer))

  /* Completing this console. Note that the demo may still run. */
  def stop(goodbye: String = ""): Unit =
    if !goodbye.isEmpty then println(goodbye)
    CLI.close()
    stopDirect()

  /* Method to start the demo of choice. */
  def start(actor: Actor): Unit =
    /* Put the new actor under guard, so we monitor when all actors are done.  */
    ActorGuard.add(actor)
    /* stop the console, this is okay, cause the demo is already running. */
    stop()

  /* Start the demo of choice but staring its corresponding actor. */
  def receive(letter: Console.Letter) = letter match
    case Console.Read("ticker")  =>  start(new Ticker)
    case Console.Read("server")  =>  start(new Server)
    case Console.Read("crawler") =>  start(new Tree("F0",None))
    case Console.Read(unknown)   =>  stop(s"Unknown demo '$unknown', closing ...")


object Console :
  /* The letters that are part of this actor. Best practice, derive them from a sealed trait. */
  sealed trait Letter extends Actor.Letter
  case class Read(text: String) extends Letter
