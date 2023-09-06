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

import scala.collection.immutable.{SortedMap, SortedSet}


/** Extend and Instantiate this class to get a custom made monitor */
abstract class GlobalMonitor(using context: ActorContext) extends ActorMonitor :
  import GlobalMonitor.{Probed, Purge, Trigger}
  import ActorMonitor.Record
  import MonitorAid.{Trace, Sample}

  /** Holds all the started actors for synchronous integration. */
  private var started: List[ProbableActor] = Nil

  /** Holds all the stopped actors for synchronous integration. */
  private var stopped: List[ProbableActor] = Nil

  /** Holds all the running actors for synchronous integration. */
  private var running: Set[ProbableActor] = Set.empty

  /** Holds all the running actor samples by path.  */
  private var samples: SortedMap[String,Record] = SortedMap.empty

  /** Holds the posts (for traceCount) and tracks their occurrences. */
  private var posts: SortedMap[Actor.Post,Long] = SortedMap.empty

  /** Holds all trace (for traceFull) entries. */
  private var traces: SortedSet[Trace] = SortedSet.empty

  /** Holds all triggers, which are entities that get reports after each probe. */
  private var triggers: Set[Trigger] = Set.empty

  /* Separate object to synchronize started/stopped actors. */
  private val guard: Object = new Object

  /** Add one actor to the actors map to be monitored. */
  private[actors] def addActor(actor: ProbableActor): Unit = guard.synchronized { started = actor :: started }

  /** Delete one actor to the actors map, will not be monitored any longer */
  private[actors] def delActor(actor: ProbableActor): Unit = guard.synchronized { stopped = actor :: stopped }

  /** Update the samples gathered from the specific actor. */
  private[actors] def setSamples(path: String, gathered: List[Sample]): Unit = samples = samples.updatedWith(path)(_.map(_.probe(gathered)))

  /** Integrate the posts gathered from the specific actor. */
  private[actors] def setPosts(gathered: List[Trace]): Unit = posts = gathered.foldLeft(posts)(postAdd)

  /** Integrate the traces gathered from the specific actor. */
  private[actors] def setTraces(gathered: List[Trace]): Unit = traces = gathered.foldLeft(traces)(traceAdd)

  /** Variable holding the cancel object for probing the actor. */
  private var cancelProbe = Cancellable.empty

  /** Clear pieces of the collected data based on the given purge flags. */
  private def clearData(purges: Set[Purge]): Unit =
    /* Removal of Traces and Posts are orthogonal, so remove them independently */
    if purges.contains(Purge.Traces) then  traces = SortedSet.empty
    if purges.contains(Purge.Posts)  then  posts  = SortedMap.empty
    /* Removal of Samples implies inactive, and inactive implies inactive workers, so remove them dependently */
    if      purges.contains(Purge.Samples)   then samples = SortedMap.empty
    else if purges.contains(Purge.Inactive)  then samples = samples.filter( (_,r) => r.active)
    else if purges.contains(Purge.Workers)   then samples = samples.filter( (_,r) => !r.worker || r.active)

  /** Copy the traces and collect the samples of all enabled actors. */
  private[actors] protected def probeNow(): Unit =
    val (started,stopped) = guard.synchronized {
      val result = (this.started,this.stopped)
      this.started = Nil
      this.stopped = Nil
      result }
    started.foreach(actor => samples = samples.updatedWith(actor.path)(_.map(_.inc).orElse(Record.start(actor.isWorker))))
    stopped.foreach(actor => samples = samples.updatedWith(actor.path)(_.map(_.off)))
    running = running ++ started
    running.foreach(_.probe())
    running = running -- stopped
    clearData(triggers.flatMap(_(Probed(samples,posts,traces))).toSet)

  /** Tells you the current monitor is global. */
  final def isLocal: Boolean = false

  /**
   * Add a new trigger to the trigger event list. When new probe data on the system becomes available
   * all triggers are called in random order. Each trigger receives the same copy of the Probed,
   * containing the latest snapshot of data. Each trigger may return one or more purge flags to
   * cleanup the collected data in the monitor. The combination of these flags (or-ed) is applied.
   * If tracing is active, it is advised to at least clear all tracing each time. */
  def register(trigger: Trigger): Unit = guard.synchronized { triggers += trigger }

  /** Remove one or more triggers from the trigger set. */
  def remove(trigger: Trigger *): Unit = guard.synchronized { triggers --= trigger }

  /** Remove all triggers from the trigger set. */
  def removeAll(): Unit = guard.synchronized { triggers = Set.empty }

  /**
   * Start the global monitor. You must call this at least once, the global
   * monitor does not start automatically. This can be done for example at the
   * end of the constructor in the derived class, or at the start of you application. */
  def start(): Unit = probeStart(true)

  /**
   * Stop the global monitor. You may stop and start the global monitor at will, which
   * can be useful to catch a specific part of the action and not get overwhelmed by
   * the trace data. The parameter withProbe determines if you want to make a final
   * last probe. */
  def stop(withProbe: Boolean): Unit = probeStop(withProbe)

  /**
   * Create a full report to a given string writer. Contains only the present data. Note, since this
   * action is performed outside the the probe loop, data may not be entirely consistent. If this is
   * needed, act upon the Trigger callbacks. This is typically used for small actor systems at the
   * end of the applications lifetime. */
  def report(writeln: String => Unit, showSamples: Boolean, showPosts: Boolean, showTraces: Boolean): Unit =
    /* Take semi snapshot, note, this may still contain inconsistencies. */
    val samples = this.samples
    val posts   = this.posts
    val traces  = this.traces
    /* Construct a basic String representation from the samples. */
    if showSamples then
      writeln("All Samples:")
      samples.foreach((path,record) => writeln(s"'$path': ${record.show}"))
    /* Construct a basic String representation from the posts. */
    if showPosts then
      writeln("All Posts:")
      posts.foreach((post,count) => writeln(s"${post.short}: $count"))
    /* Construct a basic String representation for all traced.*/
    if showTraces then
      writeln("All Traces:")
      traces.foreach(trace => writeln(trace.show))


object GlobalMonitor :

  /**
   * Type used for triggers. Each trigger receives a snapshot of all collected data
   * and must return its intentions for clearing that data. */
  type Trigger = Probed => Iterable[Purge]

  /** Case class returned to you which contains a snapshot of all collected data. */
  case class Probed(samples: SortedMap[String,ActorMonitor.Record], posts: SortedMap[Actor.Post,Long], traces: SortedSet[MonitorAid.Trace])

  /** Possible cleaning operations after processing the trigger. */
  enum Purge :
    /* Clears all actors from the samples list active or not. Active actors will quickly be reloaded.  */
    case Samples
    /* Clears all inactive actors from the samples list. This resets the incarnation counter on non worker actors. */
    case Inactive
    /* Clears all inactive workers from the samples list. Since workers are never reused, their number can increase endlessly. */
    case Workers
    /* Clear all posts. A post is a counter for a message. The number of different posts usually does not increase endlessly, the counters do. */
    case Posts
    /* Clear all traces. A trace reports a certain action in the system. The number of traces keeps increasing. */
    case Traces
