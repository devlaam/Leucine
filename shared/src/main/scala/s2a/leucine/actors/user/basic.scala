package s2a.leucine.actors




/** The BasicActor accepts messages from any other actor, but is not able to return an answer, because the sender is not tracked.
  * This simplifies the use, for not all possible return types need to be specified. It is still possible to send a message to
  * a fixed actor though, if the actorRef is known. */
abstract class BasicActor[L <: Actor.Letter](val name: String)(using context: ActorContext) extends BareActor[L,Actor.Letter,Actor.State]:
  /* The basic actor does not allow any messages to be returned to the sender, so we do not need to store the sender.
   * The letter is just a postcard ;) */
  private[actors] type Env = MyLetter

  private[actors] final def pack(letter: MyLetter, sender: Sender): Env = letter
  private[actors] final def processEnveloppe(envelope: Env, state: ActState): ActState = { receive(envelope); state }
  private[actors] final def initialState: ActState = Actor.State.Default

  /* Implement this method in your actor to process the letters send to you. There is no sender, so it is not
   * possible to see who send the letter, except from the content of the letter itself. Likewise it is not
   * possible to send back an answer. */
  protected def receive(letter: MyLetter): Unit

  /* Send a letter to the actor, no need to specify the sender. */
  def send(letter: MyLetter): Unit = sendEnvelope(letter)

