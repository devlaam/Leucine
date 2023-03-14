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


/* Methods stub for when there is no timing mixin used. */
private[actors] trait ProtectDefs :
  private[actors] def protectHigh(size: Int): Unit = ()
  private[actors] def protectEmpty(): Unit = ()


/** Mixin which guards the level of the mailbox and generates events when it gets to full. */
trait ProtectActor(using context: ActorContext) extends ActorDefs :

  /* This variable makes sure the alarm calls are never repeated.
   * Once an alarm is issued (sizeAlarm(true) called) it will not
   * happen again, unless the mailbox was completely empty at least
   * once before, which will trigger the call sizeAlarm(false) */
  private var alarmIssued: Boolean = false

  /** Execute an action later on the context. */
  private[actors] def deferred(action: => Unit): Unit

  /** Test the current mailbox size for the high water mark. */
  private[actors] override def protectHigh(size: Int): Unit =
    /* We only need to raise an alarm if we did not already do so and if the size exceeds the limit. */
    if !alarmIssued && size >= alarmSize then
      /* Make sure we do not raise the alarm for each message. */
      alarmIssued = true
      /* Call the event handler and report that we have too many mails. Note this is in a separate thread,
       * since we are currently in a synchronized environment. */
      deferred(sizeAlarm(true))

  /** Report that the mailbox is empty again and the actor becomes idle. */
  private[actors] override def protectEmpty(): Unit =
    /* We do not report an empty mailbox when we did not report a full one before. */
    if alarmIssued then
      /* Reset the alarm */
      alarmIssued = false
      /* Call the event handler and report that the mailbox is empty and we are ready for new work. */
      deferred(sizeAlarm(false))

  /**
   * The number of letters in the mailbox that issues an alarm. This must be a constant, since
   * it is called in a synchronised environment, and is called on every letter posted. */
  protected val alarmSize: Int

  /**
   * Implement an event handler for the situation the mailbox exceeds the limit set in alarmSize,
   * (full = true) and for when mailbox, stash and event queues are all depleted (full = false).
   * Each call happens only once, and you only receive an call signalling empty queues after a
   * a call with full=true was made.  */
  protected def sizeAlarm(full: Boolean): Unit
