package s2a.leucine.actors


/**
 * The StandardActor is able to respond to messages, but does not keep state. You can of course keep your own in var's.
 * If you do, make sure these are private, so there is no risk the leak to the outside world.
 * All possible return types must be specified. */
abstract class StandardActor[ML <: Actor.Letter](val name: String)(using context: ActorContext) extends BareActor[ML,Actor.State] :

  /* The Env type now holds the Letter as well as the Sender type */
  private[actors] type Env = BareActor.Envelope[MyLetter,Sender]

  /* Pack the letter with the sender into one envellope */
  private[actors] final def pack(letter: MyLetter, sender: Sender): Env = BareActor.Envelope(letter,sender)

  /* Process the letter in the enveloppe. The state remains unchanged. */
  private[actors] final def processEnveloppe(envelope: Env, state: ActState): ActState = { receive(envelope.letter,envelope.sender); state }

  /* Defines the initialState to be the Default state, the user does not need to implement this. */
  private[actors] final def initialState: ActState = Actor.State.Default

  /**
   * Implement this method in your actor to process the letters send to you. There sender contains a reference
   * to the actor that send the message. To be able to return an answer, you must know the original actor type.
   * This can be obtained by a runtime type match. Use the send method on the senders matched type.  */
  protected def receive(letter: MyLetter, sender: Sender): Unit

  /** Send a letter, with the obligation to say who is sending it. */
  def send(letter: MyLetter, sender: Sender): Unit = sendEnvelope(pack(letter,sender))
