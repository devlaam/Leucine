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
 * The StateActor is able to respond to messages, and keeps state between all calls. You are obliged to return the same
 * or a new state upon every call. This is better than using vars.  */
abstract class StateActor[ML <: Actor.Letter, AS <: Actor.State](using val context: ActorContext) extends BareActor[ML,AS] :

  /* The Env type now holds the Letter as well as the Sender type */
  private[actors] type Env = BareActor.Envelope[MyLetter,Sender]

  /* Pack the letter with the sender into one envellope */
  private[actors] final def pack(letter: MyLetter, sender: Sender): Env = BareActor.Envelope(letter,sender)

  private[actors] def repack(env: Env): BareActor.Envelope[MyLetter,Sender] = env

  /* Deliver the letter in the enveloppe. The state may also be changed by the user. */
  private[actors] final def deliverEnveloppe(envelope: Env, state: ActState): ActState =
    receive(envelope.letter,envelope.sender,state)

  /* Deliver the exception to the user, which may return a new state. */
  private[actors] final def deliverException(envelope: Env, state: ActState, exception: Exception, exceptionCounter: Int): ActState =
    except(envelope.letter,envelope.sender,state,exception,exceptionCounter)

  /* Call the user implemented initial state. */
  private[actors] final def initialState: ActState = initial

  /** Implement this method in your actor to define the first state when calling receive. Can also be implemented by a val. */
  protected def initial: ActState

  /**
   * Implement this method in your actor to process the letters send to you. There sender contains a reference
   * to the actor that send the message. To be able to return an answer, you must know the original actor type.
   * This can be obtained by a runtime type match. Use the send method on the senders matched type.
   * You also have to return the new state, which may contain any values that change between each call.
   * That way, you can steer away from var's in the actors defintion, which should not leak into the open. */
  protected def receive(letter: MyLetter, sender: Sender, state: ActState): ActState

  /**
   * Override this in your actor to process exceptions that occur while processing the letters. The default implementation
   * is to ignore the exception and pass on to the next letter. The size is the total number of exceptions this actor
   * experienced. You may decide to:
   * (1) Stop the actor, by calling stopDirect() inside the handler.
   * (2) Continue for all or certain types of exceptions.
   * (3) Continue but chanche the state to an other one, or even the initial state.
   * (4) Inform the parent if part of a family...
   * This can all be defined in this handler, so there is no need to configure some general actor behaviour. If actors
   * can be grouped with respect to the way exceptions are handled, you may define this in your CustomActor mixin, for
   * example, just log the exception. Runtime errors cannot be caught and blubble up. */
  protected def except(letter: MyLetter, sender: Sender, state: ActState, cause: Exception, size: Int): ActState = state

  /**
   * Send a letter, with the option to say who is sending it. Defaults to anonymous outside the context of an actor
   * and to self inside an actor. Returns if the letter was accepted for delivery. Note, this does not mean it also
   * processed. In the mean time the actor may stop. */
  def send(letter: MyLetter)(using sender: Sender = Actor.Anonymous): Boolean = sendEnvelope(pack(letter,sender))

  /** Send a letter with the 'tell' operator. For compatibility with Akka. */
  def ! (letter: MyLetter)(using sender: Sender = Actor.Anonymous): Unit = sendEnvelope(pack(letter,sender))
