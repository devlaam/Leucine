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
 * The StandardActor is able to respond to messages, but does not keep state. You can of course keep your own in var's.
 * If you do, make sure these are private, so there is no risk the leak to the outside world. All possible return types
 * must be specified. If no name is given, an unique name is generated, but the actor is not indexed to be retrieved
 * on the base of its name. Supply !# as name to define this a worker actor.*/
abstract class StandardActor[ML <: Actor.Letter, SD <: Actor](prename: String = "")(using val context: ActorContext) extends BareActor :

  private[actors] type MyLetter = ML
  private[actors] type ActState = Actor.State
  type Sender = SD

  /* The Env type now holds the Letter as well as the Sender type */
  private[actors] type Env = BareActor.Envelope[MyLetter,Sender]

  /* Pack the letter with the sender into one envellope */
  private[actors] final def pack(letter: MyLetter, sender: Sender): Env = BareActor.Envelope(letter,sender)

  private[actors] def repack(env: Env): BareActor.Envelope[MyLetter,Sender] = env

  /* Deliver the letter in the enveloppe. The state remains unchanged. */
  private[actors] final def deliverEnveloppe(envelope: Env, state: ActState): ActState =
    receive(envelope.letter,envelope.sender)
    state

  /* Process the exception to the user. The state remains unchanged. */
  private[actors] final def deliverException(envelope: Env, state: ActState, exception: Exception, exceptionCounter: Int): ActState =
    except(envelope.letter,envelope.sender,exception,exceptionCounter)
    state

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: ActState = Actor.State.Default

  /* Use to distinguish between basic and other actors. BasicActors does not have sender as parameter. */
  extension (fc: FamilyChild {type ChildLetter <: Actor.Letter; type ChildSender <: Actor} )
     /**
     * Forward a message to children of which the name passes the test 'include'.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (chidren that were not given an explicit name) or workers. */
    protected def relay(letter: fc.ChildLetter, sender: fc.ChildSender, include: String => Boolean): Int =
      fc.relayEnvFilter(letter,sender,include)
    /**
     * Forward a message to children that are indexed and/or workers and or children that were given
     * an automatic name, i.e. children that were not given an explicit name.
     * Returns the number of children that accepted the letter.  */
    protected def relay(letter: fc.ChildLetter, sender: fc.ChildSender, toIndexed: Boolean = true, toWorkers: Boolean = false, toAutoNamed: Boolean = false): Int =
      fc.relayEnvGrouped(letter,sender,toIndexed, toWorkers,toAutoNamed)
    /**
     * Forward a message to one specific child on the basis of its name. Returns true if successful and
     * false if that child is not present or does not accept the letter. */
    protected def pass(letter: fc.ChildLetter, sender: fc.ChildSender, name: String): Boolean = fc.passEnv(letter,sender,name)

  extension (stash: StashOps)
    /**
     * Store a letter and sender manually on the stash. With this method, you may replace one
     * letter with an other, or spoof the sender, and reprocess later. If the actor was asked to
     * finish, store will still work, since the letter was from before that request. */
    protected def store(letter: MyLetter, sender: Sender): Unit = stash.storeEnv(pack(letter,sender))

  /**
   * Implement this method in your actor to process the letters send to you. There sender contains a reference
   * to the actor that send the message. To be able to return an answer, you must know the original actor type.
   * This can be obtained by a runtime type match. Use the send method on the senders matched type.  */
  protected def receive(letter: MyLetter, sender: Sender): Unit

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
  protected def except(letter: MyLetter, sender: Sender, cause: Exception, size: Int): Unit = ()

  /**
   * Send a letter, with the option to say who is sending it. Defaults to anonymous outside the context
   * of an actor and to self inside an actor. Returns if the letter was accepted for delivery. Note, this
   * does not mean it also processed. In the mean time the actor may stop. */
  def send(letter: MyLetter, sender: Sender): Boolean = sendEnvelope(pack(letter,sender))

  /** Send a letter with the 'tell' operator. For compatibility with Akka. */
  def ! (letter: MyLetter)(using sender: Sender): Unit = sendEnvelope(pack(letter,sender))

  /** The final name of this actor. It will be the name given, or a generated name for unnamed actors and workers */
  final val name = register(prename)
