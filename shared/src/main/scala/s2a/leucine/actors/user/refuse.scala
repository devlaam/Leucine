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
 * The RefuseActor does not accepts messages from any other actor. In fact, there is no even a 'send' method. It does not
 * make sense to mixin any aids. This actor is solely meant to offload work from on other actor. That work should not be
 * defined in the constructor, but in the process() callback. This to prevent to that the work is actually done in the
 * thread of the starting actor. The work is scheduled after calling start() on the new actor. This is now a requirement,
 * since the actor cannot receive any letters that start it. Note that it is still possible to send a message to
 * a fixed actor though, if the actor itself is known. The started(), stopped() and except() callbacks are not available.
 * If no name is given, an unique name is generated, but the actor is not indexed to be retrieved on the base of its name.
 * Supply !# as name to define this a worker actor. There is no need for the (companion) object which contains the necessary
 * type aliases, for there are none. */
abstract class RefuseActor(prename: String = "")(using val context: ActorContext) extends BareActor:

  type Accept = Nothing
  type Common = Nothing
  type State  = Actor.State
  private[actors] type MyLetter[Sender >: Common <: Accept] = Nothing

  /* Deliver the letter in the envelope, but this will never happen for this actor. */
  private[actors] final def deliverEnvelope[Sender >: Common <: Accept](envelope: Env[Sender], state: State): State = state

  /* Deliver the exception to the user. The state remains unchanged. This will also never happen. */
  private[actors] final def deliverException[Sender >: Common <: Accept](envelope: Env[Sender], state: State, exception: Exception, exceptionCounter: Int): State = state

  /* The started() call back is used to process the single work package in this actor. */
  private[actors] final def deliverStarted(): Unit =
    /* Execute the users work */
    process()
    /* Directly stop the actor afterwards. */
    stop(Actor.Stop.Direct)

  /* Delivers the stopped() callback to the user. This call is made, but not relayed to the user. */
  private[actors] def deliverStopped(cause: Actor.Stop, complete: Boolean): Unit = ()

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: State = Actor.State.Default

  /** This is now obligatory to implement. */
  protected def process(): Unit

  /** The final name of this actor. It will be the name given, or a generated name for unnamed actors and workers */
  final val name = register(prename)
