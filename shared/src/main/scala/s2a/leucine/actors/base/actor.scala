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
   * Terminate this actor asap, but complete the running letter. Can be called from outside and inside
   * the actor, since the method it is thread safe. If the actor has children, stopDirect() is called
   * on all of them, after the running letter has completed. */
  def stopDirect(): Unit

  /**
   * Stop this actor from accepting new letters, terminate once the mailbox is emptied. Can be called
   * from outside and inside the actor, since the method it is thread safe. If the actor has children,
   * stopFinish() is called on all of them, after the mailbox has completed. This allows for a smooth
   * teardown of the whole family tree. Note that it is not possible for childeren to send results back
   * to the parent in this case. */
  def stopFinish(): Unit

  /**
   * See if this actor is still active. As long as it is active, it will accept letters. Once it
   * cannot longer process any new letters this turns false. Last letter processing and cleanup work
   * my still be ongoing. This is before stopped() is called.
   * This can also be used inside an actor. When it returns false, you know that stopFinish()
   * or stopDirect() was called upon the Actor from the outside. In the latter case, the current
   * letter will be the last. It makes no sense to wait for it turning false inside the actor,
   * for that may never happen, even if the actor is stopped from the outside.
   * Note that in an asynchronous system, the answer may already have changed after the read. Once it
   * turns to false however, it will never return to true again. */
  def isActive: Boolean

  /**
   * See if this actor is completely terminated. All letters, events and callbacks are over. If the
   * the reference is dropped the object will be garbage collected. When this turns true, all children,
   * when present, have already terminated, albeit that their isTerminated may turn to true later.
   * It makes no sense to call this from within an actor, because it will always return false.
   * Note that in an asynchronous system, the answer may already have changed after the read. Once it
   * turns to false however, it will never return to true again. */
  def isTerminated: Boolean

object Actor :
  /** The family consist of child, me, and parent, with the appropiate types. */
  type Family[CL <: Letter, ML <: Letter, P <: Actor[?]] = Actor[ML] with FamilyMain[CL,P]

  /** This is the base type for all your mail. */
  trait Letter

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
    /** Anonymous actor is not part of a familty. */
    def path = name
    /** The Anonymous actor is not running, so it cannot be stopped. */
    def stopDirect(): Unit = ()
    /** The Anonymous actor is not running, so it cannot finish. */
    def stopFinish(): Unit = ()
    /** The Anonymous actor will not accept letters so it is never active. */
    def isActive: Boolean = false
    /** The Anonymous actor was never in action, so technically it is never terminated */
    def isTerminated: Boolean = false
