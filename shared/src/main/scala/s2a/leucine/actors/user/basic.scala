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
 * a fixed actor though, if the actorRef is known. If no name is given, an unique name is generated, but the actor is not indexed
 * to be retrieved on the base of its name. Supply !# as name to define this a worker actor. */
//abstract class BasicActor[ML <: Actor.Letter](prename: String = "")(using val context: ActorContext) extends BareActor:
abstract class BasicActor[Define <: BasicDefine](val define: Define, prename: String = "")(using val context: ActorContext) extends BareActor:

  // private[actors] type MyLetter = ML
  // private[actors] type ActState = Actor.State
  // type Sender = Actor

  type Sender = Actor
  private[actors] type MyLetter[T <: Sender] = define.Letter
  private[actors] type ActState = Actor.State

 /* The basic actor does not allow any messages to be returned to the sender, so we do not need to store the sender.
   * The letter is just a postcard ;) */
  //private[actors] type Env = MyLetter
  //private[actors] type Env[T <: Sender] = MyLetter[Sender]

  /* Pack the letter with the sender. Here the sender is ignored. */
  //private[actors] final def pack(letter: MyLetter[Sender], sender: Sender): Env = letter
  //private[actors] final def pack(letter: MyLetter[Sender], sender: Sender): Env[Sender] = letter
  private[actors] final def pack[T <: Sender](letter: MyLetter[T], sender: T): Env[T] = BareActor.Envelope(letter,sender)

  //private[actors] def repack(env: Env): BareActor.Envelope[MyLetter,Sender] = BareActor.Envelope(env,Actor.Anonymous)
  //private[actors] def repack(env: Env[Sender]): BareActor.Envelope[Sender,Sender,MyLetter] = BareActor.Envelope(env,Actor.Anonymous)
  //private[actors] def repack[T <: Sender](env: Env[T]): BareActor.Card = BareActor.Card(env,Actor.Anonymous)
  private[actors] def repack[T <: Sender](env: Env[T]): BareActor.Card = BareActor.Card(env.letter,env.sender)

  /* Deliver the letter in the envelope. The state remains unchanged. */
  //private[actors] final def deliverEnvelope(envelope: Env, state: ActState): ActState =
  private[actors] final def deliverEnvelope[T <: Sender](envelope: Env[T], state: ActState): ActState =
    //receive(envelope)
    receive(envelope.letter)
    state

  /* Deliver the exception to the user. The state remains unchanged. */
  //private[actors] final def deliverException(envelope: Env, state: ActState, exception: Exception, exceptionCounter: Int): ActState =
  private[actors] final def deliverException[T <: Sender](envelope: Env[T], state: ActState, exception: Exception, exceptionCounter: Int): ActState =
    //except(envelope,exception,exceptionCounter)
    except(envelope.letter,exception,exceptionCounter)
    state

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: ActState = Actor.State.Default

  /* Use to distinguish between basic and other actors. BasicActors does not have sender as parameter. */
  extension (fc: FamilyChild {type ChildLetter <: Actor.Letter; type ChildSender = Actor} )
    /**
     * Forward a message to children of which the name passes the test 'include'.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (children that were not given an explicit name) or workers. */
    protected def relay(letter: fc.ChildLetter, include: String => Boolean): Int =
      fc.relayEnvFilter(letter,Actor.Anonymous,include)
    /**
     * Forward a message to children that are indexed and/or workers and or children that were given
     * an automatic name, i.e. children that were not given an explicit name.
     * Returns the number of children that accepted the letter.  */
    protected def relay(letter: fc.ChildLetter, toIndexed: Boolean = true, toWorkers: Boolean = false, toAutoNamed: Boolean = false): Int =
      fc.relayEnvGrouped(letter,Actor.Anonymous,toIndexed, toWorkers,toAutoNamed)
    /**
     * Forward a message to one specific child on the basis of its name. Returns true if successful and
     * false if that child is not present or does not accept the letter. */
    protected def pass(letter: fc.ChildLetter, name: String): Boolean = fc.passEnv(letter,Actor.Anonymous,name)

  /* Use to distinguish between basic and other actors. BasicActors does not have sender as parameter. */
  extension (stash: StashOps)
    /**
     * Store a letter manually on the stash. With this method, you may replace one
     * letter with an other and reprocess later. If the actor was asked to
     * finish, store will still work, since the letter was from before that request. */
    //protected def store(letter: MyLetter): Unit = stash.storeEnv(letter)
    //protected def store(letter: MyLetter[Sender]): Unit = stash.storeEnv(letter)
    protected def store(letter: MyLetter[Sender]): Unit = stash.storeEnv(pack(letter,Actor.Anonymous))

  /**
   * Implement this method in your actor to process the letters send to you. There is no sender, so it is not
   * possible to see who send the letter, except from the content of the letter itself. Likewise it is not
   * possible to send back an answer. */
  //protected def receive(letter: MyLetter): Unit
  protected def receive(letter: MyLetter[Sender]): Unit

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
  //protected def except(letter: MyLetter, cause: Exception, size: Int): Unit = ()
  protected def except(letter: MyLetter[Sender], cause: Exception, size: Int): Unit = ()

  /** Send a letter to the actor, no need to specify the sender. Returns if the letter was accepted
   * for delivery. Note, this does not mean it also processed. In the mean time the actor may stop. */
  //def send(letter: MyLetter): Boolean = sendEnvelope(letter)
  //def send(letter: MyLetter[Sender]): Boolean = sendEnvelope(letter)
  def send(letter: MyLetter[Sender]): Boolean = sendEnvelope(pack(letter,Actor.Anonymous))

  /** Send a letter with the 'tell' operator. For compatibility with Akka. */
  //def ! (letter: MyLetter): Unit = sendEnvelope(letter)
  //def ! (letter: MyLetter[Sender]): Unit = sendEnvelope(letter)
  def ! (letter: MyLetter[Sender]): Unit = sendEnvelope(pack(letter,Actor.Anonymous))

  /** The final name of this actor. It will be the name given, or a generated name for unnamed actors and workers */
  final val name = register(prename)


trait BasicDefine :
  type Letter <: Actor.Letter
  trait BaseLetter extends Actor.Letter { type Accept = Actor }
  // type Accept <: Actor
  // type Letter[T <: Accept] <: Actor.Letter
  // trait BaseLetter[BA <: Accept] extends Actor.Letter { type Accept = BA }


