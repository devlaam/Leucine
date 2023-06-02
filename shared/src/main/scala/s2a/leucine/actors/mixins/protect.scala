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
  private[actors] def protectRaise(size: Int): Unit = ()
  private[actors] def protectReset(): Unit = ()
  private[actors] def protectCheck(): Unit = ()
  private[actors] def protectIdle: Boolean = true


/** Mixin which guards the level of the mailbox and generates events when it gets to full. */
trait ProtectAid(using context: ActorContext) extends ActorInit, ActorDefs :
  import ProtectAid.Alarm

  /* This variable makes sure the alarm calls are never repeated.
   * Once an alarm is issued (sizeAlarm(true) called) it will not
   * happen again, unless the mailbox was completely empty at least
   * once before, which will trigger the call sizeAlarm(false) */
  private var alarm: Alarm = Alarm.Idle

  /* Variable counts the total number of alarms issued. */
  private var alarms: Int = 0

  /** Test the current mailbox size for the high water mark. */
  private[actors] override def protectRaise(size: Int): Unit = synchronized {
    /* We only need to raise an alarm if we did not already do so and if the size exceeds the limit. */
    if (alarm == Alarm.Idle) && (size >= protectLevel) then alarm = Alarm.Raised }

  /** See if we must issue an alarm and do so only if an alarm was raised before. */
  private[actors] override def protectCheck(): Unit =
    val call = synchronized { alarm match
      case Alarm.Idle    => false
      case Alarm.Raised  => alarm = Alarm.Issued; true
      case Alarm.Issued  => false }
    if call then
      /* We issue the alarm, increase the counter */
      alarms = alarms + 1
      /* Make the call. */
      protectAlarm(true,alarms)


  /**
   * See if we must reset an alarm and do so only if an alarm was issued before.
   * A raised but not yet issued alarm is cleared. */
  private[actors] override def protectReset(): Unit =
    val call = synchronized { alarm match
      case Alarm.Idle    => false
      case Alarm.Raised  => alarm = Alarm.Idle; false
      case Alarm.Issued  => alarm = Alarm.Idle; true }
    if call then protectAlarm(false,alarms)

  /** See if an alarm was raised of issued. If neither is the case this returns true. */
  private[actors] override def protectIdle: Boolean = synchronized { alarm == Alarm.Idle }

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeProtect(): Option[MonitorAid.Protect] = Some(MonitorAid.Protect(alarms))

  /**
   * The number of letters in the mailbox that issues an alarm. This must be a constant, since
   * it is called in a synchronized environment, and is called on every letter posted. */
  protected val protectLevel: Int


  /**
   * Implement an event handler for the situation the mailbox exceeds the limit set in protectLevel,
   * (full = true) and for when mailbox, stash and event queues are all depleted (full = false).
   * Each call happens only once, and you always receive an call signalling empty queues after a
   * a call with full=true was made and before the next call with full=true is made. When stop(Finish)
   * is called the call sizeAlarm(false) will come after the last letter is processed and but before
   * stopped() is called. When stop(Direct) is called, the sizeAlarm(true/false) may not come at all.
   * The size parameter reflects the total number of size alarms during the lifetime of the actor
   * up to now, this one included. */
  protected def protectAlarm(full: Boolean, size: Int): Unit

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


object ProtectAid :

  /* Possible states of the alarm. */
  enum Alarm :
    /* There was no alarm, or all alarms are handled and reported. */
    case Idle
    /* The mailbox got to full at least once. This has not yet been reported. */
    case Raised
    /* The mailbox got to full at least once and this was reported to the user. */
    case Issued
