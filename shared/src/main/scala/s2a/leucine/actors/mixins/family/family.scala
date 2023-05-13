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
private trait FamilyDefs :
  private[actors] def familySize: Int = 0
  private[actors] def familyDropNeedle(): Unit = ()
  private[actors] def familyRemoved: Boolean = false
  private[actors] def familyStop(finish: Boolean): Unit = ()
  private[actors] def familyTerminate(complete: Boolean): Unit = ()
  private[actors] def familyAbandon(): Boolean = false
  private[actors] def familyReport(): Unit = ()


/**
 * Mixin you need to create the root actor and setup a family tree. You need to specify the base
 * type of all child letters the children of this actor may receive. You may have multiple family
 * trees in your system, each with its own root. */
trait FamilyRoot[Define <: FamilyDefine](private[actors] val familyDefine: Define) extends FamilyChild, FamilyMain, ActorInit :
  self: BareActor =>
  type FamilyCommon = familyDefine.FamilyCommon
  type FamilyAccept = familyDefine.FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = familyDefine.MyFamilyLetter[Sender]

  final override def path: String = name

  /* Called to count this trait */
  override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin you need to create child actors. Actual creation should be done within the parent
 * without enclosing any of its variable state. This is your responsibility. You need to specify the base
 * type of all child letters the children of this actor may receive, as well as the parent actor type.
 * Also, your actor class needs to implement the parent. The best way to do this is to make it a class
 * parameter. That way you are obliged to define it at creation. New children must be adopted by the parent
 * after creation manually. */
trait FamilyBranch[Parent <: Actor.Parent, Define <: FamilyDefine](private[actors] val familyDefine: Define) extends FamilyChild, FamilyMain, FamilyParent, FamilySelect[Parent], ActorInit :
  self: BareActor =>
  type FamilyCommon = familyDefine.FamilyCommon
  type FamilyAccept = familyDefine.FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = familyDefine.MyFamilyLetter[Sender]

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)

  /* Called to count this trait */
  override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin that you can use to terminate the family branching at this point. It is like the FamilyBranch,
 * but without the possibility to define children. */
trait FamilyLeaf[Parent <: Actor.Parent] extends FamilyMain, FamilyParent, FamilySelect[Parent], ActorInit :

  // Dit klopt ook niet. De verwijzing moet zijn naar de childactor van de parent, of in elk geval eenzelfde type
  // Daarom gaat het steeds fout als we self in de parent calls gebruiken. De vraag is, hoe komen we erbij.
  // nB: Het werkt dus wel als dit type overeenkomt met de parent, maar dat is hier eigenlijk niet te bepalen.
  // We krijgen dus zoiets als self: ChildActor[Parent#RelaySelector]
  self: BareActor =>

  /* These type relations ensure that the ChildActor accepts at least the letters from at least
   * the senders the whole family does. It may accept more. Regarding the common actors, all
   * the senders that the letters hold in common, must also be hold in common by the family. */

  // Hier zit de clou. Het feit dat we 'm hier niet weten laat zien dat er iets wringt in het ontwerp.
  // Deze RelaySelector slaat op de instantie van de parent en NIET op deze instantie.
  // Daar moeten we 'm dus op de een of andere manier vandaan halen.
  //type RelaySelector = true //familyDefine.RelaySelector

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /**
   * Abandon the parent. Normally, there should not be a reason for the user to do so, but when
   * you want to prohibit the termination of this actor when the parent stops, this could be one.
   * The parent itself cannot be removed from the child that is rejected. When the actor is still
   * active it will be put under guard. There is no way to reunite the child and parent later on. */
  protected def abandon(): Boolean = parent.reject(self,activity.active)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)

  /* Called to count this trait */
  override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin to construct a family tree where all levels accept the same letters from the same set of actors,
 * and which may be build dynamically/recursively. The field 'parent' is an option in this case and the
 * root of the tree should not have a parent. The type of the parent equals the type of the FamilyTree
 * and all letters are derived from one common ancestor. */
trait FamilyTree[Tree <: Actor.Parent] extends FamilyChild, FamilyMain, FamilySelect[Tree], NameActor, ActorInit :
  self: BareActor =>
  type FamilyAccept = Accept
  type FamilyCommon = Common
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = MyLetter[Sender]

  type Parent = PA

  /**
   * Access to the parent of this actor. It should be implemented as value parameter in the
   * class definition of this actor. That way the parent is an stable reference. The root
   * of the family tree should not have a parent (supply none)*/
  protected def parent: Option[Parent]

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.map(_.reject(self,false)).getOrElse(false)

  /**
   * Abandon the parent when present. Normally, there should not be a reason for the user to do so, but when
   * you want to prohibit the termination of this actor when the parent stops, this could be one. Note that
   * you should not call this when the actor itself has children.
   * The parent itself cannot be removed from the child that is rejected. When the actor is still
   * active it will be put under guard. There is no way to reunite the child and parent later on. */
  protected def abandon(): Boolean = parent.map(_.reject(self,activity.active)).getOrElse(false)

  /** Register this actor. */
  private[actors] override def register(prename: String): String =
    // This works:
    parent.map(_.adopt(prename,self)).getOrElse(super.register(prename))
    // Code below gives: Recursion limit exceeded. Maybe there is an illegal cyclic reference?
    // parent match
    // /* Children register at the parent. */
    //   case Some(p) => p.adopt(prename,self)
    //   /* If this is the root of the family then we register at the guard. */
    //   case None    => super.register(prename)

  /**
   * The path returns the full lineage of this actor: dot separated names of all parents.
   * The dot can be replaced by your own char by overriding the familySepChar. */
  final override val path: String = parent.map(_.path) match
    case Some(path) => s"$path${context.familyPathSeparator}$name"
    case None       => name

  /* Called to count this trait */
  override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


trait RelayDefine :
  type RelaySelector <: Boolean

object RelayDefineTrue extends RelayDefine :
  type RelaySelector = true

object RelayDefineFalse extends RelayDefine :
  type RelaySelector = false

trait FamilyDefine :
  type RelaySelector <: Boolean
  type FamilyAccept <: Actor
  type FamilyCommon <: FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] <: Actor.Letter[Sender]
