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
private class LogGlobal(logHolder: LogHolder) :
  import ActorLogger.{Level, Entry}
  import Static.Kind
  import LogHolder.{Hold, ActorFilter}

  /**
   * Temporary contains all log entries that could not be logged via a threadLocal collection. Usually
   * these are logs in the main thread. Logs from other threads are allowed, but delay execution due
   * to the necessary synchronization. */
  //private def holderOpt: Option[LogHolder] = ActorGuard.logger.map(_.logHolder)
    /* Contains the logHolder for the main and other threads without local logHolders. */

  //TODO: We willen de incidents die in deze context gebeuren ook nog ergens melden. WAAR?
  // Note: De incidents die binnen actors plaatsvinden kunnen met lokale en globale monitors worden gespot.
  // Waarschijnlijk is het het beste als we deze accumuleren in een AtomicLong, en opvragen via de GlobalMonitor.
  // Doen als we de logger uit de guard halen.
  private[actors] def getIncidents: Int = logHolder.getIncidents


  /** Get a thread save copy of the global logs and clear the container. */
  private[actors] def retrieve(): Hold[Entry] = synchronized :
    val hold = logHolder.get
    logHolder.clear()
    hold

  /**
   * Construct a new log entry based on the given information. The holder is guaranteed to be present but the
   * level may not be sufficient for action, then return Left(true) to indicate the situation has been dealt
   * with. If feed is true, the entry will be directly stored on the log queue. Return the required entry
   * instance packed in right. */
  private[actors] def entry(feed: Boolean, level: Level, actorFilter: ActorFilter, kind: Kind, path: String, message: => String): Either[Boolean,Entry] =
    if !logHolder.pass(level,actorFilter) then Left(true) else
      /* Construct the entry on the holder. Note that, due to the fact that this instruction is outside
       * the synchronization protection below, entries can be placed in any order in the holder. */
      val entry = logHolder.make(level,kind,path,message)
      /* Add the entry to the log queue if requested to do so. */
      if feed then synchronized { logHolder.add(entry) }
      /* Construct the entry on the holder. */
      Right(entry)

