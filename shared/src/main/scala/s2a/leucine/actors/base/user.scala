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

/** The UserActor contains all methods the user of the library must implement or may override. */
private transparent trait UserActor(using context: ActorContext) extends Actor, ActorDefs :
  import Actor.Stop

  /** Pack the letter with the sender into an envelope.  */
  private[actors] final def pack[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Env[Sender] = BareActor.Envelope(letter,sender)

  /** The maximum number of letters this actor accepts. Override to change its value. */
  protected def maxMailboxSize: Int = context.maxMailboxSize

  /**
   * This defines the initial state that is used before the first letter is processed if needed.
   * The related definition must be in the actor constructor of the user code. */
  private[actors] def initialState: State

  /**
   * This calls an implementation by the user. It typically holds a handler that acts according the content of the letter.
   * If you want to work with actor states, override this receive method. Make sure your state is completely immutable. */
  private[actors] def deliverEnvelope[Sender >: Common <: Accept](envelope: Env[Sender], state: State): State

  /**
   * This calls an implementation by the user. The default implementation is to ignore the exception and pass on to the
   * next letter. Errors are not caught and bubble up. Now, this follows the Java style. */
  private[actors] def deliverException[Sender >: Common <: Accept](envelope: Env[Sender], state: State, exception: Exception, exceptionCounter: Int): State

  /**
   * This calls an implementation by the user. Called after actor construction and guaranteed before the
   * first message is processed. */
  private[actors] def deliverStarted(): Unit

  /**
   * Called before actor deactivation and guaranteed after the last message is processed. If there were any
   * unprocessed letters in this actor at tear down, complete is false. These could be in the normal mailbox
   * or on the stash, if present. Cause returns the last stop mode, so the cause of stopping this actor is
   * known. */
  private[actors] def deliverStopped(cause: Stop, complete: Boolean): Unit
