package s2a.leucine.actors



/** The StateActor is able to respond to messages, and keeps state between all calls. You are obliged to return the same
   or a new state upon every call. This is better than using vars.  */
abstract class StateActor[ML <: Actor.Letter, AS <: Actor.State](val name: String)(using context: ActorContext) extends BareActor[ML,AS] :

  private[actors] type Env = BareActor.Envelope[MyLetter,Sender]
  private[actors] final def pack(letter: MyLetter, sender: Sender): Env = BareActor.Envelope(letter,sender)
  private[actors] final def processEnveloppe(envelope: Env, state: ActState): ActState = receive(envelope.letter,envelope.sender,state)
  private[actors] final def initialState: ActState = initial

  /*  Implement this method in your actor to define the first state when calling receive. Can also be implemented by a val. */
  protected def initial: ActState

  /* Implement this method in your actor to process the letters send to you. There sender contains a reference
   * to the actor that send the message. To be able to return an answer, you must know the original actor type.
   * This can be obtained by a runtime type match. Use the send method on the senders matched type.
   * You also have to return the new state, which may contain any values that change between each call.
   * That way, you can steer away from var's in the actors defintion, which should not leak into the open. */
  protected def receive(letter: MyLetter, sender: Sender, state: ActState): ActState

  /** Send a letter, with the obligation to say who is sending it. */
  def send(letter: MyLetter, sender: Sender): Unit = sendEnvelope(pack(letter,sender))
