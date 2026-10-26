package s2a.leucine.wiki.MonitorAid2

// Scastie: https://scastie.scala-lang.org/devlaam/cJjSCe97RwKkaWCcINiUNQ/1

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*

given ActorContext = ActorContext.system

/* Simple example to illustrate debugging capabilities with a local monitor. */
class Ticker(debug: Boolean) extends AcceptActor(Ticker,"Ticker"), StateAid(Ticker.initial), MonitorAid(LocalMonitor(1.second)) :
  import Actor.{Post, Stop}
  import MonitorAid.{Sample, Trace, Tracing}

  /* We allow full tracing for this actor. */
  final override def tracing = Tracing.Enabled

  /* Define report functions for each capability */
  private def sampled(samples: List[Sample]): Unit      = samples.foreach(sample => println(s"== SAMPLE ==> ${sample.show}"))
  private def posted(posts: SortedMap[Post,Long]): Unit = posts.foreach((post,i) => println(s"== POST ====> ${post.short}: $i"))
  private def traced(traces: SortedSet[Trace]): Unit    = traces.foreach(trace   => println(s"== TRACE ===> ${trace.show}"))

  /* Register the report functions so they are called by the local monitor. */
  monitor.register(sampled)
  monitor.register(posted)
  monitor.register(traced)

  /* Only activate the monitor when we are in the debug mode. */
  probing(debug)

  /* In receive we handle the incoming letters. */
  final protected def receive(letter: Letter, sender: Sender): Receive =
    /* In this example, we do not care about the letters that much, but more about the state. */
    state match
      case Ticker.Tick(value: Int) =>
        /* Report that we are in the 'tick' state*/
        println(s"tick = $value")
        /* Send a new letter to myself to continue the work */
        this ! Ticker.Work
        /* Change the state to a new one. This is obligatory. */
        Ticker.Tock(value+1)
      case Ticker.Tock(value: Int) =>
        /* Report that we are in the 'tock' state*/
        println(s"tock = $value")
        /* As long as we are below 10 we continue the work, otherwise we stop. */
        if value<10 then this ! Ticker.Work else stop(Stop.Finish)
        /* Change the state to a new one. This is obligatory. */
        Ticker.Tick(value+1)

object Ticker extends AcceptDefine :
  /* The ticker only excepts one letter */
  sealed trait Letter extends Actor.Letter[Actor]
  case object Work extends Letter
  /* This actor can be in two 'states' */
  sealed trait State extends Actor.State
  case class Tick(value: Int) extends State
  case class Tock(value: Int) extends State
  /* The initial state of of a state actor */
  val initial: State = Ticker.Tick(0)


/* Main entry point for the demo. */
object Main :
  /* Create the root of the tree. */
  private val ticker = Ticker(true)
  /* Main code entrance point */
  def main(args: Array[String]): Unit =
    /* Set the ticker to work. */
    ticker ! Ticker.Work
    /* Give it some time to complete */
    Thread.sleep(200)
    ActorGuard.watch(false,1.seconds)
