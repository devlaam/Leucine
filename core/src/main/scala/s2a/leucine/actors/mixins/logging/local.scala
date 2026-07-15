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

/**
 * This object gives access to the thread local logHolders and accumulates its entries
 * and boundary values. To be used next to LogGlobal. */
private class LogLocal() :
  import ActorLogger.{Entry, Capture}
  import LogHolder.{Hold, minStart, maxStart}

  private var min: Long = minStart
  private var max: Long = maxStart

  /** Contains all of the logs collected from within actors (via ThreadLocal LogHolders) */
  private var accuEntries: List[List[Entry]] = Nil

  /**
   * Add some entries to the full collection. This method is synchronized since it may be called
   * from different threads. */
  private def addToAccu(hold: Hold[Entry]): Unit = synchronized :
    /* Recalculate the boundaries */
    if min > hold.min then min = hold.min
    if max < hold.max then max = hold.max
    /* Note that, since this all runs thread local the entries should be
     * ordered but not necessarily directly sequential. We can add them like this. */
    accuEntries ::= hold.entries

  /** Get a thread save copy of all local logs and clear the container. */
  private[actors] def retrieve(): Hold[List[Entry]] = synchronized :
    val copy = Hold[List[Entry]](min,accuEntries,max)
    min = minStart
    max = maxStart
    accuEntries = Nil
    copy

  /* A per thread logHolder for logs that are produced by the actors. Note that for JS or the
   * context emulator this simply stores the variable on the one an only thread. This is okay,
   * since the actor that is currently running will remove it from before it relinquishes control
   * to the next actor. To be on the safe side, a check is implemented with an assert at fill. */
  private val threadedHolder: ThreadLocal[LogHolder] = ThreadLocal[LogHolder]()

  /** Places a new container (LogHolder) for logs on this thread. */
  private[actors] def fill(logHolder: LogHolder): Unit =
    assert(logHolder.isEmpty,"Replaced logHolder should be empty before reuse.")
    assert(threadedHolder.get == null,"No logHolder should be present when a new one is stored on the thread.")
    threadedHolder.set(logHolder)

  /**
   * Try to empty and remove the threadLocal logHolder. Afterwards, the holder is removed from the thread for it
   * may be reused for other purposes. This can be an actor, but also a future or other manual use. Returns the
   * number of incidents in this run. */
  private[actors] def empty(): Unit =
    /* Get the logHolder for this thread */
    val holder = threadedHolder.get()
    /* If it is not there (null) there is nothing to do. If it is we ... */
    if holder != null then
      /* ... and it has some content then ... */
      if !holder.isEmpty then
        /* ... get the content and copy it to mainEntries ... */
        addToAccu(holder.get)
        /* .. remove the content from the holder for reuse. */
        holder.clear()
      /* and remove the holder to ensure it is not re- or misused by an other actor on the thread. */
      threadedHolder.remove()

  /**
   * Try to make an entry of the log data of the current thread. If the holder is not present, we return
   * Left(false) to indicate we could not handle this call. If the holder is present, but the level is
   * not sufficient for action return Left(true) to indicate the situation has been dealt with. When feed
   * is true, the entry will be directly stored on the log queue. Return the required entry instance. */
  private[actors] def entry(feed: Boolean, capture: Capture): Either[Boolean,Entry] =
   /* Try to obtain the local logHolder in this thread. Since this is a Java call it may return null. */
    val holder = threadedHolder.get()
    /* If so, we do not have a container and we must try the globalHolder as fallback (thus we return false).
     * Otherwise we use the obtained holder for thread local handling. Since we are always in one thread, (for
     * empty, which removes data and entry which adds data) synchronize is not needed. */
    if holder == null then Left(false) else
      /* In the holder we test if the actor path and level fulfill the runtime requirements and if so,
       * we finally test the message filter. This can now not be longer postponed. In case of slow messages
       * this is the place where the message string gets created. */
      if !holder.pass(capture) || !capture.passMessage then Left(true) else
        /* Construct the entry on the holder. */
        val entry = holder.make(capture)
        /* Add the entry to the log queue.  */
        if feed then holder.add(entry)
        /* Construct the entry on the holder. */
        Right(entry)
