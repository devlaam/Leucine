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
 * The StandardActor is able to respond to messages, but does not keep state. You can of course keep your own in variables.
 * If you do, make sure these are private, so there is no risk the leak to the outside world. All possible return types
 * must be specified. If no name is given, an unique name is generated, but the actor is not indexed to be retrieved
 * on the base of its name. Supply !# as name to define this a worker actor. Supply the (companion) object which
 * contains the necessary type aliases as first parameter. */
abstract class StandardActor[Define <: StandardDefine](private[actors] val actorDefine: Define, prename: String = "")(using val context: ActorContext) extends BareActor :

  type Accept = actorDefine.Accept
  type Common = Nothing
  private[actors] type MyLetter[T >: Common <: Accept] = actorDefine.Letter[T]
  private[actors] type ActState = Actor.State
  type Letter[T <: Accept] = MyLetter[T]

  /* Deliver the letter in the envelope. The state remains unchanged. */
  private[actors] final def deliverEnvelope[T >: Common <: Accept](envelope: Env[T], state: ActState): ActState =
    receive(envelope.letter,envelope.sender)
    state

  /* Process the exception to the user. The state remains unchanged. */
  private[actors] final def deliverException[T >: Common <: Accept](envelope: Env[T], state: ActState, exception: Exception, exceptionCounter: Int): ActState =
    except(envelope.letter,envelope.sender,exception,exceptionCounter)
    state

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: ActState = Actor.State.Default

  /* Use to distinguish between basic and other actors. BasicActors does not have sender as parameter. */
  extension (fc: FamilyChild)
     /**
     * Forward a message to children of which the name passes the test 'include'.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (children that were not given an explicit name) or workers. */
    protected def relay[T >: fc.ChildCommon <: fc.ChildSender](letter: fc.ChildLetter[T], sender: T, include: String => Boolean): Int =
      fc.relayEnvFilter(letter,sender,include)
    /**
     * Forward a message to children that are indexed and/or workers and or children that were given
     * an automatic name, i.e. children that were not given an explicit name.
     * Returns the number of children that accepted the letter.  */
    protected def relay[T >: fc.ChildCommon <: fc.ChildSender](letter: fc.ChildLetter[T], sender: T, toIndexed: Boolean = true, toWorkers: Boolean = false, toAutoNamed: Boolean = false): Int =
      fc.relayEnvGrouped(letter,sender,toIndexed,toWorkers,toAutoNamed)
    /**
     * Forward a message to one specific child on the basis of its name. Returns true if successful and
     * false if that child is not present or does not accept the letter. */
    protected def pass[T >: fc.ChildCommon <: fc.ChildSender](letter: fc.ChildLetter[T], sender: T, name: String): Boolean = fc.passEnv(letter,sender,name)

  extension (stash: StashOps)
    /**
     * Store a letter and sender manually on the stash. With this method, you may replace one
     * letter with an other, or spoof the sender, and reprocess later. If the actor was asked to
     * finish, store will still work, since the letter was from before that request. */
    protected def store[T <: Accept](letter: Letter[T], sender: T): Unit = stash.storeEnv(pack(letter,sender))

  /**
   * Implement this method in your actor to process the letters send to you. There sender contains a reference
   * to the actor that send the message. To be able to return an answer, you must know the original actor type.
   * This can be obtained by a runtime type match. Use the send method on the senders matched type.  */
  protected def receive[T <: Accept](letter: Letter[T], sender: T): Unit

  /**
   * Override this in your actor to process exceptions that occur while processing the letters. The default implementation
   * is to ignore the exception and pass on to the next letter. The size is the total number of exceptions this actor
   * experienced. You may decide to:
   * (1) Stop the actor, by calling stopDirect() inside the handler.
   * (2) Continue for all or certain types of exceptions.
   * (3) Inform the parent if part of a family...
   * This can all be defined in this handler, so there is no need to configure some general actor behavior. If actors
   * can be grouped with respect to the way exceptions are handled, you may define this in your CustomActor mixin, for
   * example, just log the exception. Runtime errors cannot be caught and bubble up. */
  protected def except[T <: Accept](letter: Letter[T], sender: T, cause: Exception, size: Int): Unit = ()

  /**
   * Send a letter, with the option to say who is sending it. Defaults to anonymous outside the context
   * of an actor and to self inside an actor. Returns if the letter was accepted for delivery. Note, this
   * does not mean it also processed. In the mean time the actor may stop. */
  def send[T <: Accept](letter: Letter[T], sender: T): Boolean = sendEnvelope(pack(letter,sender))

  /** Send a letter with the 'tell' operator. For compatibility with Akka. */
  def ![T <: Accept](letter: Letter[T])(using sender: T): Unit = sendEnvelope(pack(letter,sender))

  /** The final name of this actor. It will be the name given, or a generated name for unnamed actors and workers */
  final val name = register(prename)


/** Derive your companion object from this trait, so you can define your own typed letters. */
trait StandardDefine :
  /** Your class should contain a union of types you will accept as valid Senders. */
  type Accept <: Actor
  /** Your class should contain a sealed trait Letter[T<: Accept] derived from Actor.Letter[T]. */
  type Letter[T <: Accept] <: Actor.Letter[T]
  /** Use this inside the actor to allow the anonymous sender in Accept */
  type Anonymous = Actor.Anonymous.type
