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

/* The console is also organized as actor, which makes sense, since it must run independently from the application.
 * There is no need to specify a name. Just as an example, and since we need this actor only for a brief time,
 * we define it to be a worker */
private class Console extends AcceptActor(Console,!#), TimingAid :

  /* Send a letter to yourself */
  def selfie(letter: String => Console.Letter): String => Unit = message => this ! letter(message)

  /* The welcome message. You may choose your demo. As soon as you type the answer, a message is constructed
   * and send to this actor itself for processing. Note that, on the JVM and Native this is a blocking service
   * so it also blocks the actor. On JS it works with a callback. Normally you should not program it this way,
   * but since we are here at the start of the demo, it does not hurt. */
  CLI.talk("Please state the demo you want to run (ticker, server, crawler or chatgrt): ", selfie(Console.Demo(_)))

  override protected def stopped(cause: Actor.Stop, complete: Boolean) =
    /* CIS must be closed, otherwise the application cannot terminate. */
    CLI.close()

  /* Completing this console. Note that the demo may still run. */
  def stop(goodbye: String = ""): Unit =
    if !goodbye.isEmpty then println(goodbye)
    /* If the user made a choice, this actor is no longer required. */
    stop(Actor.Stop.Direct)

  /* Method to start the demo of choice for one time */
  def once(actor: Actor): Unit =
    /* stop the console, this is okay, cause the demo is already running. */
    stop()

  /* Start the demo of choice but staring its corresponding actor. */
  def receive(letter: Letter): Unit = letter match
    case Console.Demo("ticker")  =>  once(new Ticker)
    case Console.Demo("server")  =>  once(new Server)
    case Console.Demo("crawler") =>  once(new Tree("F0",None))
    case Console.Demo("chatgrt") =>  post(Console.Cli,100.millis)
    case Console.Demo(unknown)   =>  stop(s"Unknown demo '$unknown', closing ...")
    case Console.Cmd("exit")     =>  Chatgrt.stop(); stop();
    case Console.Cmd(command)    =>  Chatgrt.process(command); post(Console.Cli,100.millis)
    case Console.Cli             =>  Chatgrt.request(selfie(Console.Cmd(_)))


object Console extends AcceptDefine, Stateless :
  /* The letters that are part of this actor. Best practice, derive them from a sealed trait. */
  sealed trait Letter extends Actor.Letter[Actor]
  case class Demo(text: String) extends Letter
  case class Cmd(text: String) extends Letter
  case object Cli extends Letter
