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
 * The BasicActor accepts messages from any other actor, but is not able to return an answer, because the sender is not tracked.
 * This simplifies the use, for not all possible return types need to be specified. It is still possible to send a message to
 * a fixed actor though, if the actorRef is known. */
abstract class BasicActor[L <: Actor.Letter](using val context: ActorContext) extends BareActor[L,Actor.State]:

  /* The basic actor does not allow any messages to be returned to the sender, so we do not need to store the sender.
   * The letter is just a postcard ;) */
  private[actors] type Env = MyLetter

  /* Pack the letter with the sender. Here the sender is ignored. */
  private[actors] final def pack(letter: MyLetter, sender: Sender): Env = letter

  /* Process the letter in the enveloppe. The state remains unchanged. */
  private[actors] final def processEnveloppe(envelope: Env, state: ActState): ActState =
    receive(envelope)
    state

  /* Process the exception by the user. The state remains unchanged. */
  private[actors] final def processException(envelope: Env, state: ActState, exception: Exception, exceptionCounter: Int): ActState =
    except(envelope,exception,exceptionCounter)
    state

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: ActState = Actor.State.Default

  /**
   * Implement this method in your actor to process the letters send to you. There is no sender, so it is not
   * possible to see who send the letter, except from the content of the letter itself. Likewise it is not
   * possible to send back an answer. */
  protected def receive(letter: MyLetter): Unit

  /**
   * Override this in your actor to process exceptions that occur while processing the letters. The default implementation
   * is to ignore the exception and pass on to the next letter. The size is the total number of exceptions this actor
   * experienced. You may decide to:
   * (1) Stop the actor, by calling stopDirect() inside the handler.
   * (2) Continue for all or certain types of exceptions.
   * (3) Inform the parent if part of a family...
   * This can all be defined in this handler, so there is no need to configure some general actor behaviour. If actors
   * can be grouped with respect to the way exceptions are handled, you may define this in your CustomActor mixin, for
   * example, just log the exception. Runtime errors cannot be caught and blubble up. */
  protected def except(letter: MyLetter, cause: Exception, size: Int): Unit = ()

  /** Send a letter to the actor, no need to specify the sender. Returns if the letter was accepted
   * for delivery. Note, this does not mean it also processed. In the mean time the actor may stop. */
  def send(letter: MyLetter): Boolean = sendEnvelope(letter)

  /** Send a letter with the 'tell' operator. For compatibility with Akka. */
  def ! (letter: MyLetter): Unit = sendEnvelope(letter)


