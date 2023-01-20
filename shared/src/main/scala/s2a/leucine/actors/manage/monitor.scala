package s2a.leucine.actors

import java.io.PrintWriter

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt



/* Extend and Instantiate this class to get a custom made monitor */
class ActorMonitor :
  import ActorMonitor._
  import MonitorActor.Sample

  /* Holds all the actors by path. Worker actors are all stored under the same path per family level. */
  private var actors = Map[String,Record]()

  /* update methods of the actors map. */
  private[actors] def addActor(path: String): Unit =
    synchronized { actors = actors.updatedWith(path)(_.map(_.inc).orElse(Record.start)) }
    change(path,Action.Created,actors)

  private[actors] def delActor(path: String): Unit =
    synchronized { actors = actors.updatedWith(path)(_.map(_.off)) }
    change(path,Action.Removed,actors)

  private[actors] def setSamples(path: String, samples: List[Sample]): Unit =
    synchronized { actors = actors.updatedWith(path)(_.map(_.probe(samples))) }
    change(path,Action.Changed,actors)


  /** Take a snapshot of the current actor map in the monitor. */
  def snapshot: Map[String,Record] = actors

  /** Callback function that reports each change in state of the actors.
    * Implement this to make these visible. */
  def change(path: String, action: Action, actors: Map[String,Record]): Unit = ()

  /** Create a basic report to a given string writer */
  def report(target: PrintWriter): Unit =
    /* Take a snapshot*/
    val actors = this.actors
    /* Construct a basic String representation */
    actors.foreach((path,record) => target.println(s"'$path': ${record.show}"))

  /* Default probe interval. Override for other value. */
  def probeInterval = 5.seconds

/* Use this Object to directly start monitoring with default functionality. */
object ActorMonitor  :
  import MonitorActor.Sample

  enum Action :
    case Created,Changed,Removed

  case class Record(val incarnations: Int, val active: Boolean, val samples: List[Sample]) :
    def inc: Record = copy(incarnations+1,true)
    def off: Record = copy(active = false)
    def probe(samples: List[Sample]): Record = copy(samples = samples)
    private def samplesStr = samples.map(_.show).mkString("; ")
    def show = s"incarnations=$incarnations, active=$active, $samplesStr"

  object Record :
    val start = Some(Record(1,true,Nil))
