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

/* The console is also organized as actor, which makes sense, since it must run independently from the application.
 * There is no need to specify a name. Just as an example, and since we need this actor only for a brief time,
 * we define it to be a worker */
private class Console extends BasicActor[Console.Letter](!#) :

  /* The welcome message. You may choose your demo. As soon as you type the answer, a message is constructed
   * and send to this actor itself for processing. Note that, on the JVM and Native this is a blocking service
   * so it also blocks the actor. On JS it works with a callback. Normally you should not program it this way,
   * but since we are here at the start of the demo, it does not hurt. */
  CLI.talk("Please state the demo you want to run (ticker, server or crawler): ", answer => this ! Console.Read(answer) )

  override protected def stopped(cause: Actor.Stop, complete: Boolean) =
    /* CIS must be closed, otherwise the application cannot terminate. */
    CLI.close()

  /* Completing this console. Note that the demo may still run. */
  def stop(goodbye: String = ""): Unit =
    if !goodbye.isEmpty then println(goodbye)
    /* If the user may a choice, this actor is no longer required. */
    stop(Actor.Stop.Direct)

  /* Method to start the demo of choice. */
  def start(actor: Actor): Unit =
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
