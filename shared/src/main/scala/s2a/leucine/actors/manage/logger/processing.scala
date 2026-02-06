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


/**
 * Default logger you may use to simply send your logs to the console via the main thread.
 * It contains reasonable defaults for all obligatory definitions of the settings. */
trait DefaultLoggerProcessing :
  import ActorLogger.Entry
  import LogHolder.{Hold, Store}

  /* Access to spooling must be strictly sequential. This object guards the entry. */
  private object spoolGuard

  /* Keep log entries that could not yet be processed. */
  private var store: Store = Store.empty

  /** Get the max number of logs defined in your settings. */
  def maxLogs: Int

  /** Signature of the retrieve() method that returns all the log entries so far in random order. */
  def retrieve(): Hold[List[Entry]]

  /**
   * Example implementation for the spool method. This implementation makes use of a temporary store that
   * delays log entries to be spooled as long as there are entries missing. Makes a best effort to produce
   * a dense and continuous log entry numbering. Access is protected by spoolGuard for strict sequential entry. */
  def spool(completed: Boolean): Unit = spoolGuard.synchronized :
    store = ActorLogger.stichedSpool(retrieve(),store,10*maxLogs,completed,process)

  /** Simply say in the console that a fatal event occurred. */
  def handleFatal(message: String): Unit = println(s"THERE WAS A FATAL EVENT: $message")

  /** Pass all logs to the console. */
  def process(entry: Entry): Unit = println(entry)

