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


/**
 * Mixin to construct a family tree where all levels accept the same letters from the same set of actors,
 * and which may be build dynamically/recursively. The field 'parent' is an option in this case and the
 * root of the tree should not have a parent. The type of the parent equals the type of the FamilyTree
 * and all letters are derived from one common ancestor. Since all letters are accepted by all family
 * members relaying is implied (no need to mixin FamilyRelay) */
trait FamilyTree[Tree <: Actor.Parent & FamilyRelay & BareActor] extends FamilyParent, FamilyRelay, FamilyMain, FamilyDoSelect[Tree], NameActor, ActorInit :
  self: Tree  =>

  /* In a Tree every element is a Tree itself. */
  type FamilyShared = Tree
  type FamilyAccept = Accept
  type FamilyCommon = Common
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = MyLetter[Sender]
  type Parent = FamilyParent

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
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()

