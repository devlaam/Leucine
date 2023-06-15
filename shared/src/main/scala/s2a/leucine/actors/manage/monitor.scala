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


/** Basic interface for each ActorMonitor. */
trait ActorMonitor extends Probing:
  import Actor.Post
  import MonitorAid.{Action, Trace, Tracing, Sample}


  private[actors] def postAdd(col: SortedMap[Post,Long], trace: Trace): SortedMap[Post,Long] =
    if trace.action == Action.Accepted
    then col.updatedWith(trace.post)(_.map(_+1)orElse(Some(1)))
    else col

  private[actors] def traceAdd(col: SortedSet[Trace], trace: Trace): SortedSet[Trace] = col + trace

  /** Start of this monitor. To be used as the time baseline for tracing. */
  private[actors] val baseline: Long = System.nanoTime

  /** Add one actor to the actors map to be monitored. */
  private[actors] def addActor(actor: ProbableActor): Unit

  /** Delete one actor to the actors map, will not be monitored any longer */
  private[actors] def delActor(actor: ProbableActor): Unit

  /** Update the samples gathered from the specific actor. */
  private[actors] def setSamples(path: String, gathered: List[Sample]): Unit

  /** Integrate the posts gathered from the specific actor. */
  private[actors] def setPosts(gathered: List[Trace]): Unit

  /** Integrate the traces gathered from the specific actor. */
  private[actors] def setTraces(gathered: List[Trace]): Unit

  /** Tells you if the current monitor is local, if it is not, it is global. */
  def isLocal: Boolean

  /**
   * This is the public setting of tracing. Every actor has it personal setting as well.
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
   * and setting it to Default/Enabled leaves the fate in the hands of the personal setting.
   * The personal setting defaults to Tracing.Default. You must define this setting in
   * you monitor. If unsure what to do, try Tracing.Disabled first. */
  def tracing: Tracing

  /**
   * Default probe interval. Set this to a reasonable value, say 5 seconds for short running
   * applications and maybe 1 minute for servers. */
  def probeInterval: FiniteDuration


/** Use this Object to directly start monitoring with default functionality. */
object ActorMonitor  :
  import MonitorAid.Sample

  case class Record(val incarnations: Int, val worker: Boolean, val active: Boolean, val samples: List[Sample]) :
    def inc: Record = copy(incarnations+1,true)
    def off: Record = copy(active = false)
    def probe(samples: List[Sample]): Record = copy(samples = samples)
    private def samplesStr = samples.map(_.show).mkString("; ")
    def show = s"incarnations=$incarnations, worker=$worker, active=$active; $samplesStr"

  object Record :
    def start(worker: Boolean) = Some(Record(1,worker,true,Nil))
