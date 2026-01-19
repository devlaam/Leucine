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

import java.lang.ThreadLocal


private object LogLocal :
  import ActorLogger.{Level, Entry}

  /** Contains all of the logs collected from within actors (via ThreadLocal LogHolders) */
  private var accuEntries: List[List[Entry]] = Nil

  /** Add some entries to the full collection. */
  private def addToAccu(entries: List[Entry]): Unit = synchronized { accuEntries ::= entries }

  /** Get a thread save copy of all local logs and clear the container */
  private[actors] def retrieve(): List[List[Entry]] = synchronized :
    val copy = accuEntries
    accuEntries = Nil
    copy

  /** A per thread logHolder for logs that are produced by the actors. */
  private val threadedHolder: ThreadLocal[LogHolder] = ThreadLocal[LogHolder]()

  /** Places a new container (LogHolder) for logs on this thread. */
  private[actors] def fill(logHolder: LogHolder): Unit = threadedHolder.set(logHolder)

  /**
   * Try to empty and remove the threadLocal logHolder. Afterwards, the holder is removed from the thread for it
   * may be reused for other purposes. This can be an actor, but also a future or other manual use. */
  private[actors] def empty(): Unit =
    /* Get the logHolder for this thread */
    val holder = threadedHolder.get()
    /* If it is not there (null) there is nothing to do. If it is we ... */
    if holder != null then
      /* ... and it has some content then ... */
      if !holder.isEmpty then
        /* ... copy the entries to mainEntries if any ... */
        addToAccu(holder.get)
        /* .. remove the content from the holder for reuse. */
        holder.clear()
      /* and remove the holder to ensure it is not re- or misused by an other actor on the thread. */
      threadedHolder.remove()

  /* Try to store the log data onto the holder of the current thread. This may fail, then return false. */
  private[actors] def tryFeed(level: Level, className: String, message: => String): Boolean =
    /* Try to obtain the local logHolder in this thread. Since this is a Java call it may return null. */
    val holder: LogHolder = threadedHolder.get()
    /* If so, we do not have a container and we must try the globalHolder as fallback (thus we return false.
     * Otherwise we use the obtained holder for thread local handling. Since we
     * are in this thread, no synchronize needed. */
    if holder == null then false else
      if holder.pass(level) then
        /* Construct the entry on the holder. */
        val entry = holder.make(level,className,message)
        /* Add the entry to the log queue. Synchronize this call if needed */
        holder.add(entry)
      /* Return that localHolder did exist.  */
      true

