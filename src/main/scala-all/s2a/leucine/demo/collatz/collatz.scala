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

import scala.util.Random
import scala.concurrent.duration.DurationInt
import scala.collection.immutable.{SortedMap, SortedSet}
import s2a.leucine.actors.*

/* We of course also need some code to let the logger do its job. At the same time this serves as
 * a minimal example of Stateful actors. Since this actor is the main motor of this 'application' it
 * does not accept any letters from the outside world. (Actors always accept letters send to themselves) */
class Collatz(debug: Boolean) extends AcceptActor(Collatz), MonitorAid(new LocalMonitor(2.seconds)), LogAid(Logger) :
  import Actor.Post
  import MonitorAid.{Sample, Trace, Tracing}
  import ActorLogger.Level

  Logger.trace(Logger.Collatz)

  /* Allow tracing in this actor. */
  final override def tracing = Tracing.Enabled

  /* We just log the fact that this actor stops. */
  final protected override def stopped(cause: Actor.Stop, complete: Boolean) =
    Logger.trace(Logger.Collatz)
    Printer.blue("Stopped Collatz Actor")

  /* Define report functions for each capability */
  private def sampled(samples: List[Sample]): Unit      = samples.foreach(sample => println(s"== SAMPLE ==> ${sample.show}"))
  private def posted(posts: SortedMap[Post,Long]): Unit = posts.foreach((post,i) => println(s"== POST ====> ${post.short}: $i"))
  private def traced(traces: SortedSet[Trace]): Unit    = traces.foreach(trace => println(s"== TRACE ===> ${trace.show}"))

  /* Register the report functions so they are called by the local monitor.*/
  monitor.register(sampled)
  monitor.register(posted)
  monitor.register(traced)

  /* Only activate the monitor when we are in the debug mode. */
  probing(debug)

  /* Log that the collatz has commenced its operations. */
  Printer.blue("Started Collatz Actor")

  /* Hmm, lets switch to ansi colors: */
  Printer.switch(Printer.Device.ANSI)
  Logger.debug(s"Switching to ANSI terminal.")

  /* In order to set the machinery in motion, a first tick must be send. */
  // TODO: Sending a message to yourself inside the constructor may be dangerous.
  // The JIT compiler could move it and its possible the receive method is called
  // Before the actor is completely constructed. How to prohibit
  // Check in the send method if sender==receiver and then if actor is complete?
  this ! Collatz.Work

  /* In receive we handle the incoming letters. */
  final protected def receive(letter: Letter, sender: Sender): (State => State) = (state: State) => {
    Logger.trace(Logger.Collatz)
    /* In this example, we do not care about the letters that much, but more
     * about the state. */
    state match
      case Collatz.Odd(current,full,odd,evens,ceil) =>
        /* Report that we are in the 'odd' state */
        Printer.red(s"Odd  value = $current")
        /* As long as we are above 1 we continue the work, otherwise we send ourselves
         * the 'last letter'. Note that in this case this is not really needed, nobody
         * else sends messages to Collatz, so the letter queue empties itself anyway, but
         * to actually quit the application, we need it to stop itself. */
        if current>1 then this ! Collatz.Work else
          Printer.blue(s"the value = ${Collatz.initial.current}")
          Printer.blue(s"all steps = $full")
          Printer.blue(s"odd steps = $odd")
          Printer.blue(s"max value = $ceil")
          Printer.blue(s"max evens = $evens")
          stop(Actor.Stop.Finish)
        /* Calculate the next value in the Collatz sequence. */
        val next = 3 * current + 1
        /* Lets log the event that we find a new maximum value. */
        if next > ceil then Logger.info(s"New max value = $next")
        /* Demonstration of how you can dynamically adjust the log level in the actor. */
        if next > 4000 then logSettings(level = Level.Trace)
        /* Change the state to a new one. This is obligatory. */
        if next % 2 == 0
        then Collatz.Even(next,full+1,odd,1,evens,next max ceil)
        else Collatz.Odd(next,full+1,odd+1,evens,next max ceil)
      case Collatz.Even(current,full,odd,size,evens,ceil) =>
        /* Report that we are in the 'even' state*/
        Printer.green(s"Even value = $current")
        /* When the value is even, we must always continue. */
        this ! Collatz.Work
        /* Calculate the next value in the Collatz sequence. */
        val next = current / 2
        /* Demonstration of how you can dynamically restore the log level in the actor. */
        if next < 3500 then logSettings(level = Logger.runLevel)
        /* Lets log the event that we find a new longest even value. */
        if size > evens then Logger.info(s"New max evens = $size")
        /* Change the state to a new one. This is obligatory. */
        if next % 2 == 0
        then Collatz.Even(next,full+1,odd,size+1,size max evens,ceil)
        else Collatz.Odd(next,full+1,odd+1,size max evens,ceil) }

object Collatz extends AcceptDefine :
  Logger.trace(Logger.Collatz)
  type Accept = Actor
  /* The Collatz only excepts one letter */
  sealed trait Letter extends Actor.Letter[Actor]
  case object Work extends Letter
  /* The actor can be in two 'states' Even or Odd. Both carry the relevant parameters:
   *  current:  Current value in the sequence, updated on every step.
   *  full:     Counter of all steps in the sequence.
   *  odd:      Counter of all steps that produce an odd number in the sequence.
   *  size:     Counter of the number of consecutive even numbers.
   *  evens     Maximum number of consecutive even numbers so far.
   *  ceil:     Maximum value in the sequence so far. */
  sealed trait State extends Actor.State
  case class Odd(current: Int, full: Int, odd: Int, evens: Int, ceil: Int) extends State
  case class Even(current: Int, full: Int, odd: Int, size: Int, evens: Int, ceil: Int) extends State
  /* The initial state of of a state actor must be defined. */
  final val initial = Collatz.Odd(Random.nextInt(100)*2+1,0,0,0,0)
