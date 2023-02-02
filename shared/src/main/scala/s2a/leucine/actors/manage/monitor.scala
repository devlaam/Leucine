package s2a.leucine.actors

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


import java.io.PrintWriter
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt


/** Extend and Instantiate this class to get a custom made monitor */
class ActorMonitor :
  import ActorMonitor.*
  import MonitorActor.Sample

  /** Holds all the actors by path. Worker actors are all stored under the same path per family level. */
  private var actors = Map[String,Record]()

  /** Add one actor to the actors map to be monitored. */
  private[actors] def addActor(path: String): Unit =
    synchronized { actors = actors.updatedWith(path)(_.map(_.inc).orElse(Record.start)) }
    change(path,Action.Created,actors)

  /** Delete one actor to the actors map, will not be monitored any longer */
  private[actors] def delActor(path: String): Unit =
    synchronized { actors = actors.updatedWith(path)(_.map(_.off)) }
    change(path,Action.Removed,actors)

  /** Update the samples gathered from the specific actor. */
  private[actors] def setSamples(path: String, samples: List[Sample]): Unit =
    synchronized { actors = actors.updatedWith(path)(_.map(_.probe(samples))) }
    change(path,Action.Changed,actors)


  /** Take a snapshot of the current actor map in the monitor. */
  def snapshot: Map[String,Record] = actors

  /**
   * Callback function that reports each change in state of the actors.
   * Implement this to make these visible. */
  def change(path: String, action: Action, actors: Map[String,Record]): Unit = ()

  /** Create a basic report to a given string writer */
  def report(target: PrintWriter): Unit =
    /* Take a snapshot*/
    val actors = this.actors
    /* Construct a basic String representation */
    actors.foreach((path,record) => target.println(s"'$path': ${record.show}"))

  /** Default probe interval. Override for other value. */
  def probeInterval = 5.seconds


/** Use this Object to directly start monitoring with default functionality. */
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
