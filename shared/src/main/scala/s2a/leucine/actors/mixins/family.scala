package s2a.leucine.actors

trait FamilyDefs :
  private[actors] def familyStop() = ()
  private[actors] def familyFinish(): Unit = ()
  private[actors] def familyAbandon(name: String) = ()

/** Mixin if you need to create child actors and setup a family tree. Actual creation should be
 * done within the parent, wuthout enclosing any of its variable state. This is your responsibilty.
 * The root actor may be created with Anonymous as parent. Since this actor has no functionality,
 * it cannot act as a real parent, so keep a separate reference to the real root. */
trait FamilyActor[CL <: Actor.Letter, ML <: Actor.Letter, PL <: Actor.Letter] extends ActorDefs :

  this: Actor.Family[CL,ML,PL] =>

  println(s"Enter FamilyActor, parent=$parent")

  protected var parent: Actor.Family[ML,PL,?] = _

  /* Holds all the children of this actor. */
  private var children: Map[String,Actor.Family[?,CL,ML]] = Map.empty

  /* Counter to generate a unique name for the childeren of this actor. */
  private var nameCounter: Long = 0

  /* Take a snapshot of the internals of this actor. */
  private[actors] override def probeFamily(): Option[MonitorActor.Family] = Some(MonitorActor.Family())


  /* Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(name: String): Unit = reject(parent.name)

  /* Recursively stop the whole family tree upwards. The children are stopped before
   * the parent, but it is not guaranteed that they will also finish before the parent. The
   * actual stopping takes place after the current letter have finished processing, and
   * the children may finish after the the parents. This truth also holds recursively.
   * Directly after this call the childeren are removed from the map, so this is
   * the last action for them from this actor. */
  private[actors] override def familyStop(): Unit = synchronized {
    children.values.foreach(_.stopNow())
    children = Map.empty }

  /* Recursively stop the whole family tree, by sending the finish letter to all
   * children. This is done internally, after the parents mailbox reaches the
   * finish letter. All mailboxes will be handled with the letters present. The
   * sequence in which they terminate cannot be predicted, as it is unclear what
   * actions are caused by the letters that are waiting to be processed. But, if the
   * parent is the only actor that sends letters to the children, all mailboxes are
   * cleaned from the bottom up. Afterwards, the actors are removed from the childrens
   * map, by there own doing. So it may take a little while before there references are
   * removed. */
  private[actors] override def familyFinish(): Unit = synchronized {
    children.values.foreach(_.send(Actor.Letter.Finish))
    children = Map.empty }

  /* Adopt a child actor with the given name. You are responsible for giving unique names and
   * prohibiting multiple adoptions per actor yourself. If an actor under this name already
   * exists, it is overwritten. Once adopted, the actor is only removed after it stopped
   * working. This is automatic. Returns if the actor was succesfully adopted. */
  private[actors] def adopt(actor: Actor.Family[?,CL,ML]): Unit = synchronized { children += actor.name -> actor }

  def init(parent: Actor.Family[ML,PL,?]): Unit =
    println(s"init() parent=$parent, this=$this")
    this.parent = parent
    parent.adopt(this)

  /* The path returns the full lineage of this actor: dot separated names of all parents.
   * The dot can be replaced by your own char by overriding the familySepChar. */
  override def path: String = s"${parent.path}$familyPathSeparator$name"

  /* Generates a unique name for a new child actor within its siblings of the structure #<nr>.
   * Every name is quaranteed to be unique for this actor during its entire lifetime. Names that
   * start with a # are expected never to be reused, and thus the number of incarnations of this
   * named actor is not stored. This is ideal for worker actors. However, worker actors can have
   * stable names as well, as long as you know they are created/destroyed sequentially. If you just
   * need a bunch of actors on the fly to solve some tasks and then they are gone, autoname is great. */
  protected def autoname: String =
    nameCounter = nameCounter + 1
    s"$workerPrefix${nameCounter}"

  /* Reject a child with a given name. Normally, there should not be a reason to do so, but when
   * you want to prohibit the termination of an actor when the parent stops, this could be one.  */
  protected def reject(name: String): Unit = synchronized { children -= name }


  println("Exit FamilyActor")

trait FamilyLeaf[ML <: Actor.Letter, PL <: Actor.Letter] extends FamilyActor[Nothing,ML,PL] :
  self: Actor.Family[Nothing,ML,PL] =>
  private[actors] override def familyStop(): Unit = ()
  private[actors] override def familyFinish(): Unit = ()
  private[actors] override def adopt(actor: Actor.Family[?,Nothing,ML]): Unit = ()

trait FamilyRoot[CL <: Actor.Letter, ML <: Actor.Letter] extends FamilyActor[CL,ML,Nothing] :
  self: Actor.Family[CL,ML,Nothing] =>
  //protected override val parent = throw Exception("The FamilyRoot has no parent")
  override val path: String = ""
  private[actors] override def familyAbandon(name: String): Unit = ()



