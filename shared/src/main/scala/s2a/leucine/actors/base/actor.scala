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
private[actors] trait ActorDefs extends StashDefs, FamilyDefs, TimingDefs, ProtectDefs, MonitorDefs:
  /** The super type for the letters you may receive. */
  private[actors] type MyLetter <: Actor.Letter
  /** The super type for the state the actor can be in. */
  private[actors] type ActState <: Actor.State
  /** The combined type of Letter and Sender (Envelope).*/
  private[actors] type Env
  /** All actors that may send messages to this actor. Note, you may always send a message to yourself. */
  type Sender <: Actor
  /** The name of this actor. */
  def name: String
  /** The full name of this actor, contains the full path to the first ancestor.*/
  def path: String


/**
 * The actor (reference) can be used for some general handling, but not to send a message to the
 * actor. You need a reference to the original instance for that. This is needed for proper type
 * checking  */
trait Actor :
  import Actor.{Stop, Activity}

  /**
   * Name of this actor. Note: this is user defined should be unique in a within family among its siblings.
   * It can be anything, but if possible exclude . and # Dots are used to build name paths in families and
   * # are used to make numbered actors, of whom we do not want to keep names. Both chars can be defined
   * by your own if needed.  */
  def name: String

  /** Offspring Path (lineage) of this actor when part of a family, otherwise equal to its name. */
  def path: String

  /***
   * See the current activity of the actor. There are four levels:
   *   Running:    The Actor is working normally and takes letters for processing.
   *   Haltable:   The Actor is working normally but can be stopped by the system.
   *   Stopping:   The Actor is ceasing operations, and will not accept any letters.
   *   Terminated: The Actor has completely stopped and will be garbage collected when possible.
   * Activity has a field 'active' which is true for the first two cases and false for the latter.
   * As long as it is active is true it will accept letters, and you can use it normally. Once it
   * cannot longer process any new letters this turns false. This can also be used inside an actor,
   * but it makes no sense to wait for it turning false inside the actor, for that may never happen,
   * even if the actor is stopped from the outside.  Note that in an asynchronous system, the answer
   * may already have changed after the read. Once active turns to false however, it will never return
   * to true again. In normal circumstances, there is no need for the user to every test the activity. */
  def activity: Activity

  /**
   * Method to inform the actor to stop its activities directly, or later, depending on the circumstances.
   * Can be called from outside and inside the actor, since the method it is thread safe.
   * The possible values are (part of the enum Actor.Stop) :
   *   Direct:  Stop the actor asap, but complete the running letter. Subsequently stop all children likewise. Terminate afterwards.
   *   Finish:  The mailbox is finished, but no more letters are accepted. Subsequently finish all children likewise. Terminate afterwards.
   *   Barren:  Wait until all children have stopped, if present, and finish directly afterwards.
   *   Silent:  Wait until the actor and all children, if present, are silent for some time, then stop directly.
   *   Final:   This actor may be terminated if all other non final actors are terminated by themselves.
   *   Never:   This actor never stops, this is the default setting.
   * Whereas Direct stops an actor family immediately, Finish allows for a smooth tear down of the whole family tree.
   * Note that in both cases is not possible for children to send results back to the parent. To that end, Barren or
   * Silent are the better choice. Silent is also used for actors that wait on external events, but when they do not
   * arrive, the actor is able to stop itself. Lastly, Final is meant for services like logging. They should always
   * be available, but when there are not actors around anymore to send logs, the logger may be closed by the system itself.
   * Once set, the levels Direct and Finish are irrevocable. The other levels can be reset to Never as long as the
   * condition to stop the actor did not kick in. */
  def stop(value: Stop): Unit

  /**
   * Called from the guard to drop a needle. If the number of needles exceeds a threshold,
   * the actor is assumed to be silent. For internal use. */
  private[actors] def dropNeedle(root: Boolean): Unit

  /** Stop this actor asap, but complete the running letter if finish is true. */
  private[actors] def stopWith(finish: Boolean): Unit

object Actor :
  /** Having children defines the parent */
  type Parent = Actor with FamilyChild

  /** This is the base type for all your mail. */
  trait Letter

  /** This is the base type for all your states. */
  trait State

  object State :
    /** The Default state is the state that is used internally if you do not need states yourself. */
    case object Default extends State

  /** The Anonymous Actor type used by the Anonymous sender. */
  type Anonymous = Anonymous.type

  /**
   * Stopping of an actor is organized in levels of severity. The lowest level (Direct) terminates directly, the
   * highest level never terminates. The latter is the default. Levels can always be decreased, increase is only
   * possible if the action was not yet started (never for Direct and Finish). */
  enum Stop extends EnumOrder[Stop] :
    /** Stop the actor asap, but complete the running letter. Subsequently stop all children likewise. Terminate afterwards. */
    case Direct
    /** The mailbox is finished, but no more letters are accepted. Subsequently finish all children likewise. Terminate afterwards. */
    case Finish
    /** Wait until all children have stopped, if present, and finish directly afterwards. */
    case Barren
    /** Wait until the actor and all children, if present, are silent for some time, then stop directly. */
    case Silent
    /** This actor may be terminated if all other non final actors are terminated by themselves. */
    case Final
    /** This actor never stops. */
    case Never

  /**
   * The Actor may be in different states of activity. These are not primary, internal states, but are derived from
   * the Phase and Stop situation the actor is in. For the user, this is informational only, since the state may
   * change any time. However, you may assume the activity state may only increase. */
  enum Activity(val active: Boolean) extends EnumOrder[Activity] :
    /** The Actor is working normally and takes letters for processing. */
    case Running    extends Activity(true)
    /** The Actor is working normally but is stopped by the system when there are no Running actors available any more. */
    case Haltable   extends Activity(true)
    /** The Actor is ceasing operations, and will not accept any letters. */
    case Stopping   extends Activity(false)
    /** The Actor has completely stopped and will be garbage collected once it references are released. */
    case Terminated extends Activity(false)


  /**
   * Use the Anonymous Actor as a sender if you do not have a context or do want to reveal yourself.
   * It is not possible to return an answer to the Anonymous sender. Also, trying to stop it will fail. */
  object Anonymous extends Actor :
    /** How to call an Anonymous sender? (smiley: no-mouth) */
    val name = ":x"
    /** Anonymous actor is not part of a family. */
    def path = name
    /** The Anonymous actor was never in action, so technically it is never terminated, but is certainly not active. */
    def activity: Activity = Activity.Stopping
    /** The Anonymous actor is not running, so it cannot be stopped. */
    def stop(value: Stop): Unit = ()
    /** The Anonymous actor is always silent so dropping has no consequences. */
    private[actors] def dropNeedle(root: Boolean): Unit = ()
    /** The Anonymous actor is not running, so it cannot be stopped. */
    private[actors] def stopWith(finish: Boolean): Unit = ()
