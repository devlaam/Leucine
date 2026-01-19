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
 * This object keeps a holder that accumulates all the log entries that could not be stored in a holder
 * present on any actor. This can be the main thread, but also from other temporary used threads, for example
 * futures started from within an actor. It can even be on an actor, if that actor does not have logging
 * enabled. All actions are thread save, but of course require synchronization. */
private object LogGlobal :
  import ActorLogger.{Level, Entry}

  /**
   * Temporary contains all log entries that could not be logged via a threadLocal collection. Usually
   * these are logs in the main thread. Logs from other threads are allowed, but delay execution due
   * to the necessary synchronization. */
  private def holder: LogHolder = ActorGuard.logger.logHolder

  /** Get a thread save copy of the global logs and clear the container. */
  private[actors] def retrieve(): List[Entry] = synchronized :
    val copy = holder.get
    holder.clear()
    copy

  /** Construct a new log entry based on the given information. */
  private[actors] def feed(level: Level, className: String, message: => String): Unit =
    if holder.pass(level) then
      /* Construct the entry on the holder. */
      val entry = holder.make(level,className,message)
      /* Add the entry to the log queue. Synchronize this call if needed */
      synchronized { holder.add(entry) }
