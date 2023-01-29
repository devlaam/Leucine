package s2a.leucine.actors

trait FamilyDefs :
  private[actors] def familyStop() = ()
  private[actors] def familyFinish(): Unit = ()
  private[actors] def familyAbandon(name: String) = ()

/** Mixin if you need to create child actors and setup a family tree. Actual creation should be
 * done within the parent, wuthout enclosing any of its variable state. This is your responsibilty.
 * The root actor may be created with Anonymous as parent. Since this actor has no functionality,
 * it cannot act as a real parent, so keep a separate reference to the real root. */
trait FamilyActor[ChildLetter <: Actor.Letter, ParentLetter <: Actor.Letter] extends ActorDefs :

  this: Actor.Family[ChildLetter,MyLetter,ParentLetter] =>

  println(s"Enter FamilyActor")

  /* Holds the parent of this actor. */
  private var _parent: Actor.Family[MyLetter,ParentLetter,?] = null
  protected def parent: Actor.Family[MyLetter,ParentLetter,?] =
    if _parent == null then throw Exception(s"The actor $name has no parent.") else _parent

  /* Holds all the children of this actor. */
  private var _children: Map[String,Actor.Family[?,ChildLetter,MyLetter]] = Map.empty
  protected def children = _children

  /* Counter to generate a unique name for the childeren of this actor. */
  private var nameCounter: Long = 0

  private[actors] def pack(letter: MyLetter, sender: Sender): Env

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
    _children = Map.empty }

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
    _children = Map.empty }

  /* Discussion.
   * In the method join we have :
   *   child.parent = this.asInstanceOf[Actor.Family[child.MyLetter,MyLetter,?]]
   * instead of:
   *   child.parent = this
   * The problem with the latter is that the compiler cannot verify that ChildLetter
   * is related to child.MyLetter, or that they are even equal, what always is the case.
   * A solution is to abstract over MyLetter with ML:
   *   type Family[CL,ML,PL] = Actor[ML] with FamilyActor[CL,ML,PL]
   * and
   *   trait FamilyActor[CL <: Actor.Letter, ML <: Actor.Letter, PL <: Actor.Letter] extends ActorDefs :
   * and use ML instead of MyLetter in the FamilyActor implementation. ML is then automatically
   * set to MyLetter (the correct type) in the mixin, but this is just uncessary clutter, and requires
   * the user to specify the MyLetter type twice in the actor definition.
   */

  /* Adopt child actors with the given name. You are responsible for giving unique names and
   * prohibiting multiple adoptions per actor yourself. If an actor under this name already
   * exists, it is overwritten. Once adopted, the actor is only removed after it stopped
   * working. This is automatic. Returns if the actor was succesfully adopted. */
  protected def adopt(children: Actor.Family[?,ChildLetter,MyLetter] *): Unit =
    //TODO: This cast should never fail, but how can we convince the compiler? See disussion above.
    children.foreach(child => child._parent = this.asInstanceOf[Actor.Family[child.MyLetter,MyLetter,?]])
    synchronized { children.foreach(child => _children += child.name -> child) }

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
  protected def reject(name: String): Unit =
    _children.get(name).foreach(child => child._parent = null)
    synchronized { _children -= name }

  protected def relay(letter: ChildLetter, sender: Sender): Unit =
    children.values.collect{ case child: BareActor[ChildLetter,?] => child.sendEnvelope(child.pack(letter,sender))}


  println("Exit FamilyActor")

trait FamilyLeaf[ParentLetter <: Actor.Letter] extends FamilyActor[Nothing,ParentLetter] :
  this: Actor.Family[Nothing,MyLetter,ParentLetter] =>
  private[actors] override def familyStop(): Unit = ()
  private[actors] override def familyFinish(): Unit = ()
  protected override def adopt(child: Actor.Family[?,Nothing,MyLetter] *): Unit = ()

trait FamilyRoot[ChildLetter <: Actor.Letter] extends FamilyActor[ChildLetter,Nothing] :
  this: Actor.Family[ChildLetter,MyLetter,Nothing] =>
  protected override def parent = throw Exception("The FamilyRoot has no parent")
  override val path: String = name
  private[actors] override def familyAbandon(name: String): Unit = ()

trait FamilyNone extends Actor[Actor.Letter], FamilyActor[Nothing,Nothing] :
  this: Actor.Family[Nothing,MyLetter,Nothing] =>



