package s2a.manual

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
import s2a.leucine.extensions.*


given actorContext: ActorContext = ActorContext.system

val monitor = new ActorMonitor {
  override def change(path: String, action: ActorMonitor.Action, actors: Map[String,ActorMonitor.Record]): Unit =
    actors.get(path).foreach(record => println(s"$action: '$path'; ${record.show}")) }

class Logger extends BasicActor[Logger.Letter] :

  val name = "logger"
  override protected def stopped()   = println("stopped logger")


  def receive(letter: Logger.Letter) = letter match
    case  Logger.Text(text: String)   =>
      println(s"text = $text")
    case  Logger.Number(int: Int)     =>
      println(s"number = $int")
      if int==7 then stopNow()

object Logger :
  sealed trait Letter extends Actor.Letter
  case class Text(text: String) extends Letter
  case class Number(int: Int)   extends Letter



class Ticker(val parent: Driver) extends StateActor[Ticker.Letter,Ticker.State], FamilyLeaf[Driver]:

  val name = "ticker"

  def initial = Ticker.Tick(0)

  override protected def stopped()   = println("stopped ticker")

  def receive(letter: Ticker.Letter, sender: Sender, state: Ticker.State) =

    sender match
      case logger: Logger => println("Sender(Ticker) = LOGGER")
      case ticker: Ticker => println("Sender(Ticker) = TICKER")
      case anonymous: Anonymous => println("Sender(Ticker) = Anonymous")
      /* The case below can never match, but this is not recognised by the compiler. */
      case driver: Driver => println("Sender(Ticker) = Driver??")
      case _  => println(s"Sender(Ticker) = UNMATCHED $sender")

    (letter,state) match
      case (Ticker.Message(text),Ticker.Tick(value: Int)) =>
        println(s"tick = $value, msg=$text")
        Ticker.Tock(value+1)
      case (Ticker.Message(text),Ticker.Tock(value: Int)) =>
        if value>6 then
          this.send(Actor.Letter.Finish)
          println(s"My name was ${path}")
        println(s"tock = $value, msg=$text")
        Ticker.Tick(value+1)


object Ticker :
  type Accept = Logger.Letter | Actor.Anonymous
  sealed trait Letter extends Actor.Letter
  case class Message(text: String) extends Letter
  sealed trait State extends Actor.State
  case class Tick(value: Int) extends State
  case class Tock(value: Int) extends State


/* This actor is just a source for timing events. It does not respond to external messages. */
class Driver(val name: String) extends BasicActor[Driver.Letter], TimingActor, FamilyRoot[Ticker.Letter], FamilyChildExtra :

  val logger = new Logger
  val ticker = new Ticker(this)

  adopt(ticker)
  ActorGuard.add(logger)

  post(Driver.Event(1),1.second)

  def receive(letter: Driver.Letter): Unit = letter match
    case Driver.Event(nr) =>
      logger.send(Logger.Number(nr))
      logger.send(Logger.Text("x"*nr))
      ticker.send(Ticker.Message(s"Hello-$nr"),logger.self)
      //ticker.send(Ticker.Message(s"Hello-$nr"),self)
      if nr<10 then post(Driver.Event(nr+1),1.second) else
        stopNow()

object Driver :
  sealed trait Letter extends Actor.Letter
  case class Event(nr: Int) extends Letter


class TestTiming(val name: String) extends BasicActor[TestTiming.Letter], TimingActor, MonitorActor(monitor) :
  import TestTiming.Event



  class Anchor
  val anchor1 = new Anchor
  val anchor2 = new Anchor
  val anchor3 = new Anchor

  post(Event(0),8.second)
  post(Event(1),1.second,anchor1)
  post(Event(2),2.second,anchor2)
  post(Event(3),3.second,anchor3)
  post(Event(4),4.second,anchor2)
  post(Event(5),5.second,anchor3)

  def receive(letter: TestTiming.Letter): Unit = letter match
    case Event(x) =>
      println(s"Got $x")
      if x==0 then stopNow()

object TestTiming :
  sealed trait Letter extends Actor.Letter
  case class Event(nr: Int) extends Letter


object test_actors :

  @main
  def main() =

    val driver = new Driver("driver")
    ActorGuard.add(driver)

    //val test = new TestTiming("timing")
    //ActorGuard.add(test)

    /* Block until the last thread has finished if needed.  */
    ActorGuard.watch(false,10.seconds)
