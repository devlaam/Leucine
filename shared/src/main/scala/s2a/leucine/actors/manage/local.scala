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


/**
 * The local monitor is a monitor that is created for each class separately. This can be handy
 * if you just want to inspect one or a few actors. It quick to set up, and remove after the
 * debugging is done. */
class LocalMonitor(val probeInterval: FiniteDuration) extends ActorMonitor :
  import Actor.Post
  import MonitorAid.{Action, Sample, Trace, Tracing}

  /* Start value for building the posts. */
  private val noPosts: SortedMap[Post,Long] = SortedMap.empty

  /* Start value for building the traces. */
  private val noTraces: SortedSet[Trace] = SortedSet.empty

  /** Selection of the output can completely be done in the actor. */
  final val tracing: Tracing = Tracing.Default

  /** It is not possible to add actors in the local monitor */
  private[actors] final def addActor(path: String): Unit = ()

  /** It is not possible to delete actors in the local monitor */
  private[actors] final def delActor(path: String): Unit = ()

  /** Update the samples gathered from the specific actor. */
  private[actors] def setSamples(path: String, gathered: List[Sample]): Unit = sampled(gathered)

  /** Integrate the posts gathered from the specific actor. */
  private[actors] def setPosts(path: String, gathered: List[Trace]): Unit = posted(gathered.foldLeft(noPosts)(postAdd))

  /** Integrate the traces gathered from the specific actor. */
  private[actors] def setTraces(path: String, gathered: List[Trace]): Unit = traced(noTraces ++ gathered)

  /**
   * Callback function that reports that new samples of an actor were added to the table. Returns a
   * snapshot of the table, directly after this event. Override this method to make this event visible. */
  def sampled(gathered: List[Sample]): Unit = ()

  /**
   * Callback function that reports that posts of an actor were added to the table. Returns a snapshot
   * of the counting on the posts, directly after this event. Override this method to make this event
   * visible. */
  def posted(posts: SortedMap[Post,Long]): Unit = ()

  /**
   * Callback function that reports that new traces of an actor were integrated to the traces log. They
   * do not need to be consecutive. minTime is the lowest time index to change. Returns a snapshot
   * of the whole trace log, directly after this event. Below minTime, there will be no changes.
   * Override this method to make this event visible. */
  def traced(traces: SortedSet[Trace]): Unit = ()
