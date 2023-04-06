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
import scala.collection.immutable.{Map, SortedMap, SortedSet}


/** Extend and Instantiate this class to get a custom made monitor */
abstract class ActorMonitor[Config] :
  import MonitorAid.{Action, Sample, Trace, Post, Tracing}
  import ActorMonitor.Record

  /** Holds all the actors by path. Worker actors are all stored under the same path per family level. */
  private var samples: SortedMap[String,Record] = SortedMap.empty

  /** Holds the posts (for traceCount) and tracks their occurrences. */
  private var posts: SortedMap[Post,Long] = SortedMap.empty

  /** Holds all trace (for traceFull) entries. This can grow very fast. Purge when needed. */
  private var traces: SortedSet[Trace] = SortedSet.empty

  /** Start of this monitor. To be used as the time baseline for tracing. */
  private[actors] val baseline = System.nanoTime

  /** Add one actor to the actors map to be monitored. */
  private[actors] def addActor(path: String): Unit =
    val result = synchronized { samples = samples.updatedWith(path)(_.map(_.inc).orElse(Record.start)); samples }
    added(path,result)

  /** Delete one actor to the actors map, will not be monitored any longer */
  private[actors] def delActor(path: String): Unit =
    val result = synchronized { samples = samples.updatedWith(path)(_.map(_.off)); samples }
    removed(path,result)

  /** Update the samples gathered from the specific actor. */
  private[actors] def setSamples(path: String, gathered: List[Sample]): Unit =
    val result = synchronized { samples = samples.updatedWith(path)(_.map(_.probe(gathered))); samples }
    sampled(path,result)

  /** Integrate the posts gathered from the specific actor. */
  private[actors] def setPosts(path: String, gathered: Iterable[Trace]): Unit =
    def add(col: SortedMap[Post,Long], trace: Trace): SortedMap[Post,Long] =
      if trace.action == Action.Accepted
      then col.updatedWith(trace.post)(_.map(_+1)orElse(Some(1)))
      else col
    val result = synchronized { posts = gathered.foldLeft(posts)(add); posts }
    posted(path,result)

  /** Integrate the traces gathered from the specific actor. */
  private[actors] def setTraces(path: String, gathered: Iterable[Trace]): Unit =
    var minTime = Long.MaxValue
    def add(col: SortedSet[Trace], trace: Trace): SortedSet[Trace] =
      minTime = minTime min trace.time
      col + trace
    val result = synchronized { traces = gathered.foldLeft(traces)(add); traces }
    traced(path,minTime,result)

  /**
   * This is the public setting of tracing.  Every actor has it personal setting as well.
   * If tracing is active for this actor depends on both settings, in a symmetric manner.
   * There are two levels of tracing. TraceFull and TraceCount. TraceFull traces every
   * message with timestamp and action. This may be memory intensive. TraceCount just
   * counts the messages from sender to receiver per letter. Usually this number is limited.
   * However, both forms should only be used at debugging, since it requires a lot of
   * synchronized operations.
   * If both are Enabled or one is Enabled and the other is Default, the TraceFull is
   * active. In all other cases it is not. TraceCount is active when both are at Default
   * or one of the settings is Enabled. This implies that you can enable/disable
   * the tracing TraceFull here as long as the personal tracing is Default or Enabled.
   * Setting this to Disabled will always prohibit tracing TraceFull of this actor,
   * and setting it to Default/Enabled leaves the fate in the hands of the personal setting. */
  def tracing: Tracing = Tracing.Disabled

  /** Default probe interval. Override for other value. */
  def probeInterval: FiniteDuration = 5.seconds

  /** Clear the actor samples table. */
  def clearSamples(): Unit = synchronized { samples = SortedMap.empty }

  /** Keep only active actors in the samples table, i.e. all inactive actors are removed. */
  def purgeSamples(): Unit = synchronized { samples = samples.filter( (_,r) => r.active) }

  /** Clear the traces log. Only needed when tracing activated full tracing. */
  def clearTraces(): Unit = synchronized { traces = SortedSet.empty }

  /** Keep only the given last 'period' of traces, i.e. all older traces are removed. */
  def purgeTraces(time: FiniteDuration): Unit =
    val past = System.nanoTime - baseline - time.toNanos
    synchronized { traces = traces.rangeFrom(Trace.empty(past)) }

  /**
   * Callback function that reports that a new actor was added to the table. Returns a snapshot
   * of the table, directly after this event. Implement this method to make this event visible. */
  def added(path: String, samples: Map[String,Record]): Unit

  /**
   * Callback function that reports that an actor was removed from the table. Returns a snapshot
   * of the table, directly after this event. Implement this method to make this event visible. */
  def removed(path: String, samples: Map[String,Record]): Unit

  /**
   * Callback function that reports that new samples of an actor were added to the table. Returns a
   * snapshot of the table, directly after this event. Implement this method to make this event visible. */
  def sampled(path: String, samples: SortedMap[String,Record]): Unit

  /**
   * Callback function that reports that posts of an actor were added to the table. Returns a snapshot
   * of the counting on the posts, directly after this event. Implement this method to make this event
   * visible. */
  def posted(path: String, posts: SortedMap[Post,Long]): Unit

  /**
   * Callback function that reports that new traces of an actor were integrated to the traces log. They
   * do not need to be consecutive. minTime is the lowest time index to change. Returns a snapshot
   * of the whole trace log, directly after this event. Below minTime, there will be no changes.
   * Implement this method to make this event visible. */
  def traced(path: String, minTime: Long, traces: SortedSet[Trace]): Unit

  /** Create a full report to a given string writer */
  def report(target: PrintWriter, samples: Boolean, posts: Boolean, traces: Boolean): Unit =
    /* Take snapshot */
    val (samplesSS,postsSS,tracesSS) = synchronized { (this.samples,this.posts,this.traces) }
    /* Construct a basic String representation from the samples. */
    if samples then
      target.println("All Samples:")
      samplesSS.foreach((path,record) => target.println(s"'$path': ${record.show}"))
    /* Construct a basic String representation from the posts. */
    if posts then
      target.println("All Posts:")
      postsSS.foreach((post,count) => target.println(s"${post.show}: $count"))
    /* Construct a basic String representation for all traced.*/
    if traces then
      target.println("All Traces:")
      tracesSS.foreach(trace => target.println(trace.show))

  /** Use this method to extract dynamical information contained here called from your actor. */
  def show(config: Config): Unit


/** Use this Object to directly start monitoring with default functionality. */
object ActorMonitor  :
  import MonitorAid.Sample

  case class Record(val incarnations: Int, val active: Boolean, val samples: List[Sample]) :
    def inc: Record = copy(incarnations+1,true)
    def off: Record = copy(active = false)
    def probe(samples: List[Sample]): Record = copy(samples = samples)
    private def samplesStr = samples.map(_.show).mkString("; ")
    def show = s"incarnations=$incarnations, active=$active, $samplesStr"

  object Record :
    val start = Some(Record(1,true,Nil))
