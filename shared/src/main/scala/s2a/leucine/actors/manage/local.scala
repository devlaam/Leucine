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


import scala.annotation.targetName
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

  /** Callback function that reports that new samples of this actor as a list. */
  private var sampled: List[Sample] => Unit = _ => ()

  /** Callback function that reports that posts of this actor as a table. */
  private var posted: SortedMap[Post,Long] => Unit =  _ => ()

  /** Callback function that reports that new traces of this actor as a Sorted Set. */
  private var traced: SortedSet[Trace] => Unit =  _ => ()

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

  /** Register a callback function for the collected samples. Set this in your actor constructor. */
  @targetName("registerSampled")
  def register(sampled: List[Sample] => Unit): Unit = this.sampled = sampled

  /** Register a callback function for the collected posts. Set this in your actor constructor. */
  @targetName("registerPosted")
  def register(posted: SortedMap[Post,Long] => Unit): Unit = this.posted = posted

  /** Register a callback function for the collected traces. Set this in your actor constructor. */
  @targetName("registerTraced")
  def register(traced: SortedSet[Trace] => Unit): Unit = this.traced = traced
