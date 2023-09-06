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
import scala.collection.immutable.{SortedMap, SortedSet}


/**
 * The local monitor is a monitor that is created for each class separately. This can be handy
 * if you just want to inspect one or a few actors. It quick to set up, and remove after the
 * debugging is done. You must set the probeInterval for each instant by hand. */
class LocalMonitor(val probeInterval: FiniteDuration)(using context: ActorContext) extends ActorMonitor :
  import Actor.Post
  import MonitorAid.{Trace, Tracing, Sample}

  /* Holds the host actor to be probed for this monitor. */
  private var actor: Option[ProbableActor] = None

  /* Start value for building the posts. */
  private val noPosts: SortedMap[Post,Long] = SortedMap.empty

  /* Start value for building the traces. */
  private val noTraces: SortedSet[Trace] = SortedSet.empty

  /** Callback function that reports that new samples of this actor as a list. */
  private var sampled: List[Sample] => Unit = _ => ()

  /** Callback function that reports that posts of this actor as a table. */
  private var posted: SortedMap[Post,Long] => Unit =  _ => ()

  /** Callback function that reports that new traces of this actor as a Sorted Set. */
  private var traced: SortedSet[Trace] => Unit =  _ => ()

  /** Add the  underlying actor to this local monitor. */
  private[actors] final def addActor(actor: ProbableActor): Unit = this.actor = Some(actor)

  /** Remove the  underlying actor from this local monitor. */
  /* Not implemented for the local monitor knows just one actor, and after stop the last probe
   * must still be made. This does not lead to a memory leak, since the instance of LocalMonitor
   * should be contained as parameter in the Actor class itself. */
  private[actors] final def delActor(actor: ProbableActor): Unit = ()

  /** Update the samples gathered from the specific actor. */
  private[actors] def setSamples(path: String, gathered: List[Sample]): Unit = sampled(gathered)

  /** Integrate the posts gathered from the specific actor. */
  private[actors] def setPosts(gathered: List[Trace]): Unit = posted(gathered.foldLeft(noPosts)(postAdd))

  /** Integrate the traces gathered from the specific actor. */
  private[actors] def setTraces(gathered: List[Trace]): Unit = traced(noTraces ++ gathered)

  /** Probe the underlying actor, if present. */
  private[actors] protected def probeNow(): Unit = actor.foreach(_.probe())

  /** Selection of the output can completely be done in the actor. */
  final def tracing: Tracing = Tracing.Default

  /** Tells you the current monitor is local. */
  final def isLocal: Boolean = true

  /** Register a callback function for the collected samples. Set this in your actor constructor. */
  @targetName("registerSampled")
  def register(sampled: List[Sample] => Unit): Unit = this.sampled = sampled

  /** Register a callback function for the collected posts. Set this in your actor constructor. */
  @targetName("registerPosted")
  def register(posted: SortedMap[Post,Long] => Unit): Unit = this.posted = posted

  /** Register a callback function for the collected traces. Set this in your actor constructor. */
  @targetName("registerTraced")
  def register(traced: SortedSet[Trace] => Unit): Unit = this.traced = traced
