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
 * The RefuseActor does not accept messages from any other actor. In fact, there is not even a 'send' method. It does not
 * make sense to mixin any aids. This actor is solely meant to offload work from on other actor. That work should not be
 * defined in the constructor, but in the process() callback. This to prevent to that the work is actually done in the
 * thread of the starting actor. Note that it is well possible to send a message from this actor to an other fixed actor though,
 * if the actor itself is known. You can divide your work into batches, so that it may be interleaved with other work.
 * The process() loop is being called as long as you do not call done() inside it. Just as with regular actors you
 * may update a state on each loop. If you do not need to define a state or name, no class parameters are required.
 * If no name is given, an unique name is generated, but the actor is not indexed to be retrieved on the base of its name.
 * Supply !# as name to define this a worker actor. There is no need for the (companion) object which contains the necessary
 * type aliases, for there are none. */
abstract class RefuseActor[Define <: RefuseDefine](private[actors] val actorDefine: Define = RefuseStateless, prename: String = "")(using val context: ActorContext) extends BareActor, ActorShare(prename):
  /* We only want to be able to send messages to myself. */
  type Accept = this.type
  type Common = this.type
  type State = actorDefine.State
  /* The letters are not further exposed. */
  type Letter = Actor.Letter[Accept]
  private[actors] type MyLetter[Sender >: Common <: Accept] = Letter

  /* The work package is initiated by this dummy letter send to myself. */
  private val work = pack(new Actor.Letter[Accept]{},this)

  /* Deliver the letter in the envelope. */
  private[actors] final def deliverEnvelope[Sender >: Common <: Accept](envelope: Env[Sender], state: State): State =
    /* Let the letter be processed */
    val processed = process()
    /* Post a work message for the next process run. */
    sendEnvelope(work)
    /* The state remains unchanged, if we work stateless, otherwise compute the new state.
     * TODO: Can this also be solved compile time? In an elegant manner?
     * Based on this: https://scastie.scala-lang.org/13dD1LD8Q3OUpLrn89oLqw? */
    if processed.isInstanceOf[Unit] then state else processed.asInstanceOf[State => State](state)

  /* Deliver the exception to the user. The state remains unchanged. */
  private[actors] final def deliverException[Sender >: Common <: Accept](envelope: Env[Sender], state: State, exception: Exception, exceptionCounter: Int): State =
    except(exception,exceptionCounter)
    state

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: State = actorDefine.initial

  /**
   * Implement this method in your actor to process the workload in parts. As long as you do not
   * explicitly stop the actor from the in- or outside, this method will continue to be called. */
  protected def process(): Receive

  /**
   * Override this in your actor to process exceptions that occur while processing. The default implementation
   * is to ignore the exception and pass. The size is the total number of exceptions this actor
   * experienced. You may decide to:
   * (1) Stop the actor, by calling stop(Actor.Stop.Direct) inside the handler.
   * (2) Continue for all or certain types of exceptions.
   * (3) Inform the parent if part of a family...
   * This can all be defined in this handler, so there is no need to configure some general actor behavior. If actors
   * can be grouped with respect to the way exceptions are handled, you may define this in your CustomAid mixin, for
   * example, just log the exception. Runtime errors cannot be caught and bubble up. */
  protected def except(cause: Exception, size: Int): Unit = ()

  /**
   * Call done() to stop this actor from processing. It is equivalent with stop(Actor.Stop.Direct), which can
   * also be used. Other forms of stopping have the same or non predictable effects, so it is advised refrain
   * from using them. May also be called from the outside. */
  final def done() = stop(Actor.Stop.Direct)

  /* Start the work in this actor. */
  sendEnvelope(work)




/** Derive your companion object from this trait, so you can define your own typed letters. */
trait RefuseDefine :
  /** Define the State you want to modify. Note: if you do not want/have this, mixin Stateless. */
  type State <: Actor.State
  /** Define the initial value of the state. */
  def initial: State

/**
 * Default object for Refuse actors that don't need any state. Since no other types need to be
 * defined in the RefuseDefine this default implementation is provided. It also serves as the
 * default value in the RefuseActor class define parameter.
 */
object RefuseStateless extends RefuseDefine, Stateless
