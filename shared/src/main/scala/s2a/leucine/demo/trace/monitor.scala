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

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*


/* If you want to use the monitor, you must implement some callback methods. Here we keep
 * that simple and only report the after the application has completed. In bigger applications
 * you might need to export the situation from time to time and purge the monitor to prevent
 * data structures from getting to large. */
class Monitor extends GlobalMonitor :
  /* Custom method to show the results obtained so far. Since this example only has one short
   * run the results are show at the end. */
  def show(samples: Boolean = false, postsAndTraces: Boolean = false): Unit = report(println,samples,postsAndTraces,postsAndTraces)
  /* This is a short running application, probe often */
  def probeInterval: FiniteDuration = 5.seconds
  /* Global setting of tracing. Here we enable is for all actors. Since the personal setting is Default
   * this should activate TraceCount and TraceFull. */
  def tracing = MonitorAid.Tracing.Enabled
  /* Start the monitor directly. */
  start()

/* This can be used in the different demonstrations */
val globalMonitor = new Monitor

