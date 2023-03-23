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


/** The BareActor implements all methods needed for basic actor operation. It should not be instantiated by the user. */
abstract class BareActor(using context: ActorContext) extends ControlActor, NameActor :

  if context.actorTracing then println(s"In actor=$path: Constructed")

  /** Use this inside the actor to test for an anonymous sender */
  type Anonymous = Actor.Anonymous.type

  /* Pack the letter with the sender. Here the sender is ignored.
   * //TODO For the moment, this is only used in at message relaying among
   * children. This is not ideal design, since we do not want to know anything
   * about the sender in the BareActor. However, i see no solution right now. */
  private[actors] def pack(letter: MyLetter, sender: Sender): Env

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeBare(): Option[MonitorAid.Bare] =
    val result = MonitorAid.Bare(phase,mailbox.sum,mailbox.max,excepts,userLoad)
    mailbox.reset()
    Some(result)

  /** Used as sender for all messages send from this actor without explicit sender. */
  given this.type = this


object BareActor :

  /* The actor passes different phases in its lifetime; not called states, for these are external to the handling.
   * After construction the phase is Start. When the first message comes in, it calls beforeStart and advances to Play.
   * From there it may oscillate between Play and Pause. The phase is Pause when the message queue is empty and Play
   * as long as there are letters on the queue. If a Finish letter arrives while the loop is running, the phase moves
   * to Finish, and the current queue is fisished. If stopDirect() is called, the phase advances to Stop, which terminates
   * the loop asap. Subsequently afterStop() is called, and the pahse becomes Done. It may never be reactivated again.
   * The first three phases are active (so the actor may accept letters, the last ones are not, and describe the state
   * in various ways of tearing down. */
  private[actors] enum Phase(val active: Boolean) :
    /** The first phase the actor is in after creation. */
    case Start  extends Phase(true)
    /** The active phase when the actor is processing a letter. */
    case Play   extends Phase(true)
    /** A passive phase were the actor waits for new letters to process. */
    case Pause  extends Phase(true)
    /** A phase in which the actor will terminate the current queue and then stop. */
    case Finish extends Phase(false)
    /** A phase in which the actor will terminate the current letter and then stop. */
    case Stop   extends Phase(false)
    /** The last phase were the actor will not accept any new letters. */
    case Done   extends Phase(false)

  /** The Envelope is responsible for holding the letter and the sender together. */
  private[actors] class Envelope[L,S](val letter: L, val sender: S)
