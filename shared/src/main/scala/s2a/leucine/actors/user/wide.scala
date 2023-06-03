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
 * The WideActor accepts messages from any other actor, and any message that is derived from Actor.Letter, also when they
 * are defined or meant for other actors. In fact, this renders the typed aspect for typed actors a little useless. But
 * it is mean to make the transition from non typed to typed actor design easier. Senders are tracked.
 * If no name is given, an unique name is generated, but the actor is not indexed
 * to be retrieved on the base of its name. Supply !# as name to define this a worker actor. Supply the (companion) object which
 * contains the necessary type aliases as first parameter. */
abstract class WideActor[Define <: WideDefine](private[actors] val actorDefine: Define, prename: String = "")(using val context: ActorContext) extends BareActor, ActorShare(prename) :

  type Accept = Actor | this.type
  type Common = Actor
  type State  = actorDefine.State
  type Letter = MyLetter[Actor]
  type Sender = Actor
  private[actors] type MyLetter[Sender >: Common <: Accept] = Actor.Letter[Actor]

  /* Deliver the letter in the envelope. The state remains unchanged. */
  private[actors] final def deliverEnvelope[Sender >: Common <: Accept](envelope: Env[Sender], state: State): State =
    /* Let the letter be processed */
    val received = receive(envelope.letter,envelope.sender)
    /* The state remains unchanged, if we work stateless, otherwise compute the new state.
     * TODO: Can this also be solved compile time? In an elegant manner?
     * Based on this: https://scastie.scala-lang.org/13dD1LD8Q3OUpLrn89oLqw? */
    if received.isInstanceOf[Unit] then state else received.asInstanceOf[State => State](state)

  /* Process the exception to the user. The state remains unchanged. */
  private[actors] final def deliverException[Sender >: Common <: Accept](envelope: Env[Sender], state: State, exception: Exception, exceptionCounter: Int): State =
    except(envelope.letter,envelope.sender,exception,exceptionCounter)
    state

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: State = actorDefine.initial

  /* Use to distinguish between basic and other actors. AcceptActors does not have sender as parameter. */
  extension (fc: FamilyRelay {  type FamilyAccept = Actor; type Sender = Actor;  } )
    /**
     * Forward a message to all children with indexed name, workers and auto named.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (children that were not given an explicit name) or workers. */
    protected def relayAll(letter: fc.MyFamilyLetter[fc.FamilyAccept], sender: fc.Sender): Int =
      fc.relayEnvGrouped(letter,sender,true,true,true)
     /**
     * Forward a message to children of which the name passes the test 'include'.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (children that were not given an explicit name) or workers. */
    protected def relayFilter(letter: fc.MyFamilyLetter[fc.FamilyAccept], sender: fc.Sender, include: String => Boolean): Int =
      fc.relayEnvFilter(letter,sender,include)
    /**
     * Forward a message to children per group: indexed and/or workers and/or children that were given
     * an automatic name, i.e. children that were not given an explicit name.
     * Returns the number of children that accepted the letter.  */
    protected def relayGrouped(letter: fc.MyFamilyLetter[fc.FamilyAccept], sender: fc.Sender, toIndexed: Boolean, toWorkers: Boolean, toAutoNamed: Boolean): Int =
      fc.relayEnvGrouped(letter,sender,toIndexed,toWorkers,toAutoNamed)
    /**
     * Forward a message to one specific child on the basis of its name. Returns true if successful and
     * false if that child is not present or does not accept the letter. */
    protected def pass(letter: fc.MyFamilyLetter[fc.FamilyAccept], sender: fc.Sender, name: String): Boolean = fc.passEnv(letter,sender,name)

  extension (stash: StashOps)
    /**
     * Store a letter and sender manually on the stash. With this method, you may replace one
     * letter with an other, or spoof the sender, and reprocess later. If the actor was asked to
     * finish, store will still work, since the letter was from before that request. */
    protected def store(letter: Letter, sender: Sender): Unit = stash.storeEnv(pack(letter,sender))

  /**
   * Implement this method in your actor to process the letters send to you. There sender contains a reference
   * to the actor that send the message. To be able to return an answer, you must know the original actor type.
   * This can be obtained by a runtime type match. Use the send method on the senders matched type.  */
  protected def receive(letter: Letter, sender: Sender): Receive

  /**
   * Override this in your actor to process exceptions that occur while processing the letters. The default implementation
   * is to ignore the exception and pass on to the next letter. The letter itself will be filed as Unreadable at the ActorGuard.
   * The size is the total number of exceptions this actor experienced. You may decide to:
   * (1) Stop the actor, by calling stop(Actor.Stop.Direct) inside the handler.
   * (2) Continue for all or certain types of exceptions.
   * (3) Inform the parent if part of a family...
   * This can all be defined in this handler, so there is no need to configure some general actor behavior. If actors
   * can be grouped with respect to the way exceptions are handled, you may define this in your CustomAid mixin, for
   * example, just log the exception. Runtime errors cannot be caught and bubble up. */
  protected def except(letter: Letter, sender: Sender, cause: Exception, size: Int): Unit =
    /* This counts as a failed message, if this exception is not handled. */
    synchronized { failed += 1 }
    /* If this exception is not handled, the letter is registered as unreadable. */
    ActorGuard.fail(Actor.Post(Actor.Mail.Unreadable,path,letter,sender))

  /**
   * The user may, as a precaution, end each match statement of the letter/sender type with an
   * catch all, and pass the result to unmatched, if the compiler is unable to verify that all
   * possible cases have been covered. */
  def unmatched(letter: Letter, sender: Sender): Unit =
    /* This counts as a failed message */
    synchronized { failed += 1 }
    /* Report the message as failed */
    ActorGuard.fail(Actor.Post(Actor.Mail.Unmatched,path,letter,sender))

  /**
   * Send a letter, with the option to say who is sending it. Defaults to anonymous outside the context
   * of an actor and to self inside an actor. Returns if the letter was accepted for delivery. Note, this
   * does not mean it also processed. In the mean time the actor may stop. */
  def send(letter: Letter, sender: Sender): Boolean = sendEnvelope(pack(letter,sender))

  /** Send a letter with the 'tell' operator. For compatibility with Akka. */
  def !(letter: Letter)(using sender: Sender): Unit = sendEnvelope(pack(letter,sender))


/** Derive your companion object from this trait, so you can define your own typed letters. */
trait WideDefine :
  /** Define the State you want to modify. Note: if you do not want/have this, mixin Stateless. */
  type State <: Actor.State
  /** Your class may contain a sealed trait Letter derived from Actor.Letter[Accept]. */
  type Letter <: Actor.Letter[Actor]
  /** Use this inside the actor to allow the anonymous sender in Accept */
  type Anonymous = Actor.Anonymous.type
  /** Define the initial value of the state. */
  def initial: State

