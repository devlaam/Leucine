package s2a.leucine.demo

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
import scala.collection.immutable.{Map, SortedMap, SortedSet}
import s2a.leucine.actors.*


case class Config(samples: Boolean = false, posts: Boolean = false, traces: Boolean = false)

/* If you want to use the monitor, you must implement some callback methods. Here we keep
 * that simple and only report the after the application has completed. In bigger applications
 * you might need to export the situation from time to time and purge the monitor to prevent
 * data structures from getting to large. */
val monitor = new ActorMonitor[Config] {
  import MonitorAid.{Trace, Post, Action, Tracing}
  import ActorMonitor.Record
  /* This callback is directly called in case an actor is added. Not used in this example. */
  def added(path: String, actors: Map[String,Record]): Unit = ()
  /* This callback is directly called in case an actor is removed. Not used in this example. */
  def removed(path: String, actors: Map[String,Record]): Unit = ()
  /* This callback is periodically called on the actor to update the actor matrics. Not used in this example. */
  def sampled(path: String, actors: SortedMap[String,Record]): Unit = ()
  /* This callback is periodically called on the actor collect all posts. Not used in this example. */
  def posted(path: String, posts: SortedMap[Post,Long]): Unit = ()
  /* This callback is periodically called on the actor collect all tracing. Not used in this example. */
  def traced(path: String, minTime: Long, traces: SortedSet[Trace]): Unit = ()
  /* Method you can implement to show the results obtained sofar. Since this example only has one short
   * run the results are show an the end. */
  def show(config: Config): Unit =
    val writer: PrintWriter = new PrintWriter(System.out)
    report(writer,config.samples,config.posts,config.traces)
    writer.flush()
  /* Global setting of tracing. Here we enable is for all actors. Since the personal setting is Default
   * this should activate TraceCount and TraceFull. */
  override def tracing = Tracing.Enabled }


