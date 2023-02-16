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


/* Methods stub for when there is no family mixin used. */
private[actors] trait FamilyDefs :
  private[actors] def familyStop() = ()
  private[actors] def familyFinish(): Unit = ()
  private[actors] def familyAbandon(name: String) = ()


/**
 * Holds all the methods needed for managing the children of the family actor member.
 * For internal use. Not all families have children, so this is only mixed in
 * when children are expected. */
private trait FamilyChild[CL <: Actor.Letter, PA <: Actor[?]] extends ActorDefs :
  this: Actor.Family[CL,MyLetter,PA] =>

  /** The super type for the letters the childeren may receive. */
  type ChildLetter = CL

  /** Variable that holds all the children of this actor. */
  private var _children: Map[String,BareActor[ChildLetter,?]] = Map.empty

  /** Save access to the children of this actor. */
  protected def children: Map[String,BareActor[ChildLetter,?]] = _children

  /** Get the current number of children. For internal use. */
  private[actors] override def childrenSize: Int = children.size

  /**
   * Recursively stop the whole family tree upwards. The children are stopped before
   * the parent, but it is not guaranteed that they will also finish before the parent. The
   * actual stopping takes place after the current letter have finished processing, and
   * the children may finish after the the parents. This truth also holds recursively.
   * Directly after this call the childeren are removed from the map, so this is
   * the last action for them from this actor. */
  private[actors] override def familyStop(): Unit = synchronized {
    children.values.foreach(_.stopDirect())
    _children = Map.empty }

  /**
   * Recursively stop the whole family tree, by sending the finish letter to all
   * children. This is done internally, after the parents mailbox reaches the
   * finish letter. All mailboxes will be handled with the letters present. The
   * sequence in which they terminate cannot be predicted, as it is unclear what
   * actions are caused by the letters that are waiting to be processed. But, if the
   * parent is the only actor that sends letters to the children, all mailboxes are
   * cleaned from the bottom up. Afterwards, the actors are removed from the childrens
   * map, by there own doing. So it may take a little while before there references are
   * removed. */
  private[actors] override def familyFinish(): Unit = synchronized {
    children.values.foreach(_.stopFinish())
    _children = Map.empty }

  /**
   * Adopt child actors with the given name. You are responsible for giving unique names and
   * prohibiting multiple adoptions per actor yourself. If an actor under this name already
   * exists, it is overwritten. Once adopted, the actor is only removed after it stopped
   * working. This is automatic. Returns if the actor was succesfully adopted. */
  protected def adopt(children: Actor.Family[?,ChildLetter,? <: Actor[MyLetter]] *): Unit =
    synchronized { if isActive then children.collect{ case child: BareActor[ChildLetter,?] =>_children += child.name -> child } }

  /**
   * Reject a child with a given name. Normally, there should not be a reason to do so, but when
   * you want to prohibit the termination of an actor when the parent stops, this could be one.
   * The parent cannot be removed. */
  protected[actors] def reject(name: String): Unit =
    synchronized { _children -= name }

  /**
   * Forward a message to all children, or children of which the name pass the test 'include'.
   * Returns the number of children that accepted the letter. */
  protected def relay(letter: ChildLetter, sender: Sender, include: String => Boolean): Int =
    val selected = children.filter((key,_) => include(key))
    def send(name: String, child: BareActor[ChildLetter,?]) = child.sendEnvelope(child.pack(letter,sender))
    children.map(send).count(identity)

  /**
   * Forward a message to one specific child on the basis of its name. Returns true if successful and
   * false if that child is not present or does not accept the letter. */
  protected def pass(letter: ChildLetter, sender: Sender, name: String): Boolean =
    children.get(name).map(child => child.sendEnvelope(child.pack(letter,sender))).getOrElse(false)


/**
 * Holds all the general methods needed for managing the family actor.
 * For internal use. This is always mixed in. */
private trait FamilyMain[CL <: Actor.Letter, PA <: Actor[?]] extends ActorDefs :
  this: Actor.Family[CL,MyLetter,PA] =>

  /** Counter to generate a unique name for the childeren/workers of this actor. */
  private var _workersCounter: Long = 0L

  /** Get the number of worker names generated. */
  protected[actors] def workersCounter: Long = _workersCounter

  /** Get the current number of children. For internal use. In not overriden, this value is zero. */
  private[actors] def childrenSize: Int = 0

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeFamily(): Option[MonitorActor.Family] = Some(MonitorActor.Family(childrenSize,workersCounter))

  /**
   * Generates a unique name for a new child actor within its siblings of the structure #<nr>.
   * Every name is quaranteed to be unique for this actor during its entire lifetime. Names that
   * start with a # are expected never to be reused, and thus the number of incarnations of this
   * named actor is not stored. This is ideal for worker actors. However, worker actors can have
   * stable names as well, as long as you know they are created/destroyed sequentially. If you just
   * need a bunch of actors on the fly to solve some tasks and then they are gone, autoname is great. */
  protected def autoname: String =
    _workersCounter = _workersCounter + 1
    s"$workerPrefix${_workersCounter}"


/**
 * Holds all the methods needed for accessing the parent of this family. For internal use.
 * Not all actors have a parent, so this is not mixed in for the root of the family. */
private trait FamilyParent[CL <: Actor.Letter, PA <: Actor[?] with FamilyChild[?,?]] extends ActorDefs :
  this: Actor.Family[CL,MyLetter,PA] =>

  /** The type of the parent for this actor. */
  type Parent = PA

  /**
   * Access to the parent of this actor. It should be implemented as value parameter in the
   * class definition of this actor. That way the parent is an stable reference. */
  protected def parent: Parent

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(name: String): Unit = parent.reject(name)

  /**
   * The path returns the full lineage of this actor: dot separated names of all parents.
   * The dot can be replaced by your own char by overriding the familySepChar. */
  override def path: String = s"${parent.path}$familyPathSeparator$name"


/**
 * Mixin you need to create the root actor and setup a family tree. You need to specify the base
 * type of all child letters the children of this actor may receive. You may have multiple family
 * trees in your system, each with its own root. */
trait FamilyRoot[ChildLetter <: Actor.Letter] extends FamilyMain[ChildLetter,Nothing], FamilyChild[ChildLetter,Nothing] :
  this: Actor.Family[ChildLetter,MyLetter,Nothing] =>
  override def path: String = name


/**
 * Mixin you need to create child actors. Actual creation should be done within the parent
 * without enclosing any of its variable state. This is your responsibilty. You need to specify the base
 * type of all child letters the children of this actor may receive, as well as the parent actor type.
 * Also, your actor class needs to implement the parent. The best way to do this is to make it a class
 * parameter. That way you are obliged to define it at creation. New children must be adopted by the parent
 * after creation manually. */
trait FamilyBranch[ChildLetter <: Actor.Letter, Parent <: Actor[?] with FamilyChild[?,?]] extends FamilyMain[ChildLetter,Parent], FamilyChild[ChildLetter,Parent], FamilyParent[ChildLetter,Parent] :
  this: Actor.Family[ChildLetter,MyLetter,Parent] =>


/**
 * Mixin that you can use to terminate the family branching at this point. It is like the FamilyBranch,
 * but without the posibility to define children. */
trait FamilyLeaf[Parent <: Actor[?] with FamilyChild[?,?]] extends FamilyMain[Nothing,Parent], FamilyParent[Nothing,Parent] :
  this: Actor.Family[Nothing,MyLetter,Parent] =>



