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


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.collection.immutable.{Map, SortedMap, SortedSet}


/** Extend and Instantiate this class to get a custom made monitor */
abstract class GlobalMonitor extends ActorMonitor :
  import Actor.Post
  import MonitorAid.{Action, Sample, Trace, Tracing}
  import ActorMonitor.Record

  /** Holds all the actors by path. Worker actors are all stored under the same path per family level. */
  private var samples: SortedMap[String,Record] = SortedMap.empty

  /** Holds the posts (for traceCount) and tracks their occurrences. */
  private var posts: SortedMap[Post,Long] = SortedMap.empty

  /** Holds all trace (for traceFull) entries. This can grow very fast. Purge when needed. */
  private var traces: SortedSet[Trace] = SortedSet.empty

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
  private[actors] def setPosts(path: String, gathered: List[Trace]): Unit =
    val result = synchronized { posts = gathered.foldLeft(posts)(postAdd); posts }
    posted(path,result)

  /** Integrate the traces gathered from the specific actor. */
  private[actors] def setTraces(path: String, gathered: List[Trace]): Unit =
    var minTime = Long.MaxValue
    def add(col: SortedSet[Trace], trace: Trace): SortedSet[Trace] =
      minTime = minTime min trace.time
      col + trace
    val result = synchronized { traces = gathered.foldLeft(traces)(add); traces }
    traced(path,minTime,result)

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
   * of the table, directly after this event. Override this method to make this event visible. */
  def added(path: String, samples: Map[String,Record]): Unit = ()

  /**
   * Callback function that reports that an actor was removed from the table. Returns a snapshot
   * of the table, directly after this event. Override this method to make this event visible. */
  def removed(path: String, samples: Map[String,Record]): Unit = ()

  /**
   * Callback function that reports that new samples of an actor were added to the table. Returns a
   * snapshot of the table, directly after this event. Override this method to make this event visible. */
  def sampled(path: String, samples: SortedMap[String,Record]): Unit = ()

  /**
   * Callback function that reports that posts of an actor were added to the table. Returns a snapshot
   * of the counting on the posts, directly after this event. Override this method to make this event
   * visible. */
  def posted(path: String, posts: SortedMap[Post,Long]): Unit = ()

  /**
   * Callback function that reports that new traces of an actor were integrated to the traces log. They
   * do not need to be consecutive. minTime is the lowest time index to change. Returns a snapshot
   * of the whole trace log, directly after this event. Below minTime, there will be no changes.
   * Override this method to make this event visible. */
  def traced(path: String, minTime: Long, traces: SortedSet[Trace]): Unit = ()

  /** Create a full report to a given string writer */
  def report(writeln: String => Unit, samples: Boolean, posts: Boolean, traces: Boolean): Unit =
    /* Take snapshot */
    val (samplesSS,postsSS,tracesSS) = synchronized { (this.samples,this.posts,this.traces) }
    /* Construct a basic String representation from the samples. */
    if samples then
      writeln("All Samples:")
      samplesSS.foreach((path,record) => writeln(s"'$path': ${record.show}"))
    /* Construct a basic String representation from the posts. */
    if posts then
      writeln("All Posts:")
      postsSS.foreach((post,count) => writeln(s"${post.short}: $count"))
    /* Construct a basic String representation for all traced.*/
    if traces then
      writeln("All Traces:")
      tracesSS.foreach(trace => writeln(trace.show))
