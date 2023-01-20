package s2a.leucine.actors



/* Used as a type-parameter free base trait for all mixins. */
trait ActorDefs extends StashDefs, FamilyDefs, TimingDefs, MonitorDefs:
  private[actors] type MyLetter <: Actor.Letter
  private[actors] type YrLetter <: Actor.Letter
  private[actors] type ActState <: Actor.State
  private[actors] type Env
  protected def workerPrefix: String = "#"
  protected def familyPathSeparator: Char = '.'
  type Sender = Actor[MyLetter,YrLetter]#Sender
  def name: String
  def path: String
  def self: Actor[MyLetter,YrLetter]


/** The actor (reference) can be used for some general handling, but not to send a message to the
  * actor. You need a reference to the full object for that. This is needed for proper type
  * checking (due to type erasure you cannot check the Actor itself). This can be obtained using
  * matching. */
trait Actor[ML <: Actor.Letter, YL <: Actor.Letter] :
  /** All actors that may send messages to this actor. Note, you may always send a message to yourself. */
  type Sender = Actor.SenderType[ML|YL]

  /** Name of this actor. Note: this is user defined should be unique in a within family among its siblings.
    * It can be anyting, but if possible exclude . and # Dots are used to build name paths in families and
    * # are used to make numberd actors, of whom we do not want to keep names. Both chars can be overriden
    * by your own if needed.  */
  def name: String

  /** Offspring Path (lineage) of this actor when part of a family, otherwise equal to its name. */
  def path: String

  /** Only meant to send the Termination letter. This can be done by anyone that holds a reference, and
   * there is no need to say who send it, for it is never visibly processed within the actor. */
  def send(letter: Actor.Letter.Finish.type): Unit

  /** Stop this actor asap, but complete the running letter. Since this is a synchronized method
   * it is thread safe. However, use it sparsely from the outside, since it may be unclear who is
   * stopping your actors. */
  def stopNow(): Unit

  /* See if this actor is still active. */
  def isActive: Boolean


object Actor :
  type SenderType[L] = Actor[_ <: L, _ <: Actor.Letter]
  type Family = Actor[_,_] with FamilyActor

  /* This is the base type for all your mail. */
  trait Letter
  object Letter :
    /* This is special letter that all actor accept and instructs them to complete
     * the current mailbox and shutdown afterwards. */
    object Finish extends Letter

  /* This is the base type for all your states. */
  trait State

  object State :
    /* If you do not use states, the state that is used internally is the Default state */
    case object Default extends State

  /* The letter type used by the Anonymous sender. It will however not accept mails of
   * this type */
  trait Anonymous extends Letter

  /** Use the Anonymous Actor as a sender if you do not have a context, do want to reveal yourself.
    * it is not possible to return an answer to the Anonymous sender. Also, trying to stop it will
    * fail. The Anonymous actor is also used as the primairy actor in a family tree. */
  object Anonymous extends Actor[Anonymous,Letter] with FamilyActor(Anonymous) :
    private[actors] type MyLetter = Anonymous
    private[actors] type YrLetter = Letter
    def self: Actor[MyLetter,YrLetter] = this
    /* How to call an Anonymous sender? (smiley: no-mouth) */
    val name = ":x"
    /* Anonymous actor itself as parent: generatio spontanea! This prohibits
     * an infinite recursive call to the path. */
    override val path = name
    /* The Anonymous actor is not running, so it be send a letter. */
    def send(letter: Actor.Letter.Finish.type): Unit = ()
    /* The Anonymous actor is not running, so it cannot be stopped. */
    def stopNow(): Unit = ()
    /* The Anonymous actor is not running, so it is never active. */
    def isActive: Boolean = false


