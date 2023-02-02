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


/** Used as a type-parameter free base trait for all mixins. */
private[actors] trait ActorDefs extends StashDefs, FamilyDefs, TimingDefs, MonitorDefs:
  /** The super type for the letters you may receive. */
  private[actors] type MyLetter <: Actor.Letter
  /** The super type for the state the actor can be in. */
  private[actors] type ActState <: Actor.State
  /** The combined type of Letter and Sender (Enveloppe).*/
  private[actors] type Env
  /** The prefix used in actornames for actors that are workers */
  protected def workerPrefix: String = "#"
  /** The character that will be used in the full name definitions of the actors.*/
  protected def familyPathSeparator: Char = '.'
  /** All actors that may send messages to this actor. Note, you may always send a message to yourself. */
  type Sender = Actor[? <: Actor.Letter]
  /** The name of this actor. */
  def name: String
  /** The fullname of this actor, contains the full path to the first ancestor.*/
  def path: String
  /** Public reference to this actor instance. */
  def self: Actor[MyLetter]


/**
 * The actor (reference) can be used for some general handling, but not to send a message to the
 * actor. You need a reference to the full object for that. This is needed for proper type
 * checking (due to type erasure you cannot check the Actor itself). This can be obtained using
 * matching. ML is the super type for the letters you may receive, YL is the super type for the
 * letters you may send. The latter usually is the union of two or more type hyrarchies.  */
trait Actor[ML <: Actor.Letter] :

  /**
   * Name of this actor. Note: this is user defined should be unique in a within family among its siblings.
   * It can be anyting, but if possible exclude . and # Dots are used to build name paths in families and
   * # are used to make numberd actors, of whom we do not want to keep names. Both chars can be overriden
   * by your own if needed.  */
  def name: String

  /** Offspring Path (lineage) of this actor when part of a family, otherwise equal to its name. */
  def path: String

  /**
   * Only meant to send the Termination letter. This can be done by anyone that holds a reference, and
   * there is no need to say who send it, for it is never visibly processed within the actor. */
  def send(letter: Actor.Letter.Finish.type): Unit

  /**
   * Stop this actor asap, but complete the running letter. Since this is a synchronized method
   * it is thread safe. However, use it sparsely from the outside, since it may be unclear who is
   * stopping your actors. */
  def stopNow(): Unit

  /** See if this actor is still active. */
  def isActive: Boolean


object Actor :
  /** The family consist of child, me, and parent, with the appropiate types. */
  type Family[CL <: Letter, ML <: Letter, P <: Actor[?]] = Actor[ML] with FamilyMain[CL,P]

  /** This is the base type for all your mail. */
  trait Letter

  object Letter :
    /**
     * This is special letter that all actor accept and instructs them to complete
     * the current mailbox and shutdown afterwards. */
    object Finish extends Letter

  /** This is the base type for all your states. */
  trait State

  object State :
    /** The Default state is the state that is used internally if you do not need states yourself. */
    case object Default extends State

  /** The Letter type used by the Anonymous sender. It will however not accept mails of this type */
  trait Anonymous extends Letter

  /**
   * Use the Anonymous Actor as a sender if you do not have a context or do want to reveal yourself.
   * It is not possible to return an answer to the Anonymous sender. Also, trying to stop it will fail. */
  object Anonymous extends Actor[Anonymous] :
    private[actors] type MyLetter = Anonymous
    def self: Actor[MyLetter] = this
    /** How to call an Anonymous sender? (smiley: no-mouth) */
    val name = ":x"
    /** Anonymous actor itself as parent: generatio spontanea! */
    override val path = name
    /** The Anonymous actor is not running, so it cannot be send a letter. */
    def send(letter: Actor.Letter.Finish.type): Unit = ()
    /** The Anonymous actor is not running, so it cannot be stopped. */
    def stopNow(): Unit = ()
    /** The Anonymous actor is not running, so it is never active. */
    def isActive: Boolean = false
