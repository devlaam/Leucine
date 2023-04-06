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

/* We of course also need some code to let the logger do its job. At the same time this serves as
 * a minimal example of Stateful actors. Since this actor is the main motor of this 'application' it
 * does not accept any letters from the outside world. (Actors always accept letters send to themselves) */
class Ticker extends StateActor[Ticker.Letter,Actor,Ticker.State](), LogInfo :

  /* The initial state of of a state actor must be defined. */
  def initial = Ticker.Tick(0)

  /* We just log the fact that this actor stops. */
  override protected def stopped(cause: Actor.Stop, complete: Boolean) = Logger.error(s"stopped ticker, complete=$complete")

  /* In order to set the machinery in motion, a first tick must be send. */
  this ! Ticker.Work

  /* Log that the ticker has commenced its operations. */
  Logger.warn("Ticker Actor created")

  /* In receive we handle the incoming letters. */
  def receive(letter: Ticker.Letter, sender: Sender, state: Ticker.State): Ticker.State =
    /* In this example, we do not care about the letters that much, but more
     * about the state. */
    state match
      case Ticker.Tick(value: Int) =>
        /* Report that we are in the 'tick' state*/
        Logger.debug(s"tick = $value")
        /* Send a new letter to myself to continue the work */
        this ! Ticker.Work
        /* Change the state to a new one. This is obligatory. */
        Ticker.Tock(value+1)
      case Ticker.Tock(value: Int) =>
        /* Report that we are in the 'tock' state*/
        Logger.info(s"tock = $value")
        /* As long as we are below 10 we continue the work, otherwise we send ourselves
         * the 'last letter'. Note that in this case this is not really needed, nobody
         * else sends messages to Ticker, so the letter queue empties itself anyway, but
         * to actually quit the application, we need it to stop itself. */
        if value<10 then this ! Ticker.Work else stop(Actor.Stop.Finish)
        /* After a few ticks we know the app is working, and set the logger level
         * to debug. Note, this is a soft switch. So some work is done, even for
         * debug level Debug. Change Logger.level at compile time for production. */
        if value == 5 then Logger.switch(Logger.Level.Info)
        /* Change the state to a new one. This is obligatory. */
        Ticker.Tick(value+1)

object Ticker :
  /* The ticker only excepts one letter */
  sealed trait Letter extends Actor.Letter
  object Work extends Letter
  /* This actor can be in two 'states' */
  sealed trait State extends Actor.State
  case class Tick(value: Int) extends State
  case class Tock(value: Int) extends State
