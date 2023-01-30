package s2a.leucine.actors

trait FamilyDefs :
  private[actors] def familyStop() = ()
  private[actors] def familyFinish(): Unit = ()
  private[actors] def familyAbandon(name: String) = ()

/* Holds all the methods needed for managing the children of the family actor member. */
private trait FamilyChild[ChildLetter <: Actor.Letter, Parent <: Actor[?]] extends ActorDefs :
  this: Actor.Family[ChildLetter,MyLetter,Parent] =>
  println(s"Enter FamilyChild")

  /* Holds all the children of this actor. */
  private var _children: Map[String,BareActor[ChildLetter,?]] = Map.empty
  protected def children = _children

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

  /* Adopt child actors with the given name. You are responsible for giving unique names and
   * prohibiting multiple adoptions per actor yourself. If an actor under this name already
   * exists, it is overwritten. Once adopted, the actor is only removed after it stopped
   * working. This is automatic. Returns if the actor was succesfully adopted. */
  protected def adopt(children: Actor.Family[?,ChildLetter,? <: Actor[MyLetter]] *): Unit =
    synchronized { children.collect{ case child: BareActor[ChildLetter,?] =>_children += child.name -> child } }

  /* Reject a child with a given name. Normally, there should not be a reason to do so, but when
   * you want to prohibit the termination of an actor when the parent stops, this could be one.
   * The parent cannot be removed. */
  protected[actors] def reject(name: String): Unit =
    synchronized { _children -= name }

  protected def relay(letter: ChildLetter, sender: Sender): Unit =
    children.values.foreach(child => child.sendEnvelope(child.pack(letter,sender)))

  println("Exit FamilyChild")

/* Holds all the general methods needed for the family actor member. */
private trait FamilyMain[ChildLetter <: Actor.Letter, Parent <: Actor[?]] extends ActorDefs :
  this: Actor.Family[ChildLetter,MyLetter,Parent] =>
  println(s"Enter FamilyMain")

  /* Counter to generate a unique name for the childeren of this actor. */
  private var nameCounter: Long = 0

  /* Take a snapshot of the internals of this actor. */
  private[actors] override def probeFamily(): Option[MonitorActor.Family] = Some(MonitorActor.Family())

  /* Generates a unique name for a new child actor within its siblings of the structure #<nr>.
   * Every name is quaranteed to be unique for this actor during its entire lifetime. Names that
   * start with a # are expected never to be reused, and thus the number of incarnations of this
   * named actor is not stored. This is ideal for worker actors. However, worker actors can have
   * stable names as well, as long as you know they are created/destroyed sequentially. If you just
   * need a bunch of actors on the fly to solve some tasks and then they are gone, autoname is great. */
  protected def autoname: String =
    nameCounter = nameCounter + 1
    s"$workerPrefix${nameCounter}"

  println("Exit FamilyMain")

/* Holds all the methods needed for managing the parent of the family actor member. */
private trait FamilyParent[ChildLetter <: Actor.Letter, Parent <: Actor[?] with FamilyChild[?,?]] extends ActorDefs :
  this: Actor.Family[ChildLetter,MyLetter,Parent] =>
  println(s"Enter FamilyParent")

  protected def parent: Parent

  /* Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(name: String): Unit = parent.reject(name)

  /* The path returns the full lineage of this actor: dot separated names of all parents.
   * The dot can be replaced by your own char by overriding the familySepChar. */
  override def path: String = s"${parent.path}$familyPathSeparator$name"

  println("Exit FamilyParent")


/** Mixins if you need to create child actors and setup a family tree. Actual creation should be
 * done within the parent (except the root) without enclosing any of its variable state. This is
 * your responsibilty.
 * You need to specify the base type of all Child Letters, as well as the parent actor type. Also,
 * your Actor class needs to implement the parent. The best way to do this is to make it a class
 * parameter. That way you are obliged to define it at creation. */

/** Mixin you need to create the root actor and setup a family tree. You need to specify the base
  * type of all child letters the children of this actor may receive. You may have multiple family
  * trees in your system, each with its own root. */
trait FamilyRoot[ChildLetter <: Actor.Letter] extends FamilyChild[ChildLetter,Nothing], FamilyMain[ChildLetter,Nothing] :
  this: Actor.Family[ChildLetter,MyLetter,Nothing] =>
  override def path: String = name

/** Mixin you need to create child actors. Actual creation should be done within the parent
  * without enclosing any of its variable state. This is your responsibilty. You need to specify the base
  * type of all child letters the children of this actor may receive, as well as the parent actor type.
  * Also, your actor class needs to implement the parent. The best way to do this is to make it a class
  * parameter. That way you are obliged to define it at creation. New children must be adopted by the parent
  * after creation manually. */
trait FamilyBranch[ChildLetter <: Actor.Letter, Parent <: Actor[?] with FamilyChild[?,?]] extends FamilyChild[ChildLetter,Parent], FamilyMain[ChildLetter,Parent], FamilyParent[ChildLetter,Parent] :
  this: Actor.Family[ChildLetter,MyLetter,Parent] =>

/** Mixin that you can use to terminate the family branching at this point. It is like the FamilyBranch,
  * but without the posibility to define children. */
trait FamilyLeaf[Parent <: Actor[?] with FamilyChild[?,?]] extends FamilyMain[Nothing,Parent], FamilyParent[Nothing,Parent] :
  this: Actor.Family[Nothing,MyLetter,Parent] =>



