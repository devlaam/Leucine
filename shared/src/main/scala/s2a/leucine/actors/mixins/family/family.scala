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
  private[actors] def familySize: Int = 0
  private[actors] def familyRemoved: Boolean = false
  private[actors] def familyStop(finish: Boolean): Unit = ()
  private[actors] def familyTerminate(complete: Boolean): Unit = ()
  private[actors] def familyAbandon(): Boolean = false
  private[actors] def familyReport(): Unit = ()


/**
 * Mixin you need to create the root actor and setup a family tree. You need to specify the base
 * type of all child letters the children of this actor may receive. You may have multiple family
 * trees in your system, each with its own root. */
trait FamilyRoot[ChildLetter <: Actor.Letter, ChildSender <: Actor] extends FamilyChild, FamilyMain :
  self: BareActor =>
  private[actors] type CL = ChildLetter
  private[actors] type RS = ChildSender

  final override def path: String = name


/**
 * Mixin you need to create child actors. Actual creation should be done within the parent
 * without enclosing any of its variable state. This is your responsibilty. You need to specify the base
 * type of all child letters the children of this actor may receive, as well as the parent actor type.
 * Also, your actor class needs to implement the parent. The best way to do this is to make it a class
 * parameter. That way you are obliged to define it at creation. New children must be adopted by the parent
 * after creation manually. */
trait FamilyBranch[ChildLetter <: Actor.Letter, ChildSender <: Actor, Parent <: Actor.Parent] extends FamilyChild, FamilyMain, FamilyParent :
  self: BareActor =>
  private[actors] type CL = ChildLetter
  private[actors] type RS = ChildSender
  private[actors] type PA = Parent { type CL <: self.MyLetter; type RS <: self.Sender }

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)


/**
 * Mixin that you can use to terminate the family branching at this point. It is like the FamilyBranch,
 * but without the posibility to define children. */
trait FamilyLeaf[Parent <: Actor.Parent] extends FamilyMain, FamilyParent:
  self: BareActor =>
  private[actors] type PA = Parent { type CL <: self.MyLetter; type RS <: self.Sender }

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /**
   * Abandon the parent. Normally, there should not be a reason for the user to do so, but when
   * you want to prohibit the termination of this actor when the parent stops, this could be one.
   * The parent itself cannot be removed from the child that is rejected. When the actor is still
   * active it will be put under guard. There is no way to reunite the child and parent later on. */
  protected def abandon(): Boolean = parent.reject(self,isActive)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)




/**
 * Mixin to construct a family tree where all levels accept the same letters, and which may be build dynamically/recursively.
 * The field 'parent' is an option in this case and the root of the tree should not have a parent. The type of the parent equals
 * the type of the FamilyTree and all letters are derived from one common ancestor. */
trait FamilyTree[Tree <: Actor.Parent] extends FamilyChild, FamilyMain, Actor :
  self: BareActor =>
  private[actors] type CL = MyLetter
  private[actors] type RS = Sender
  private[actors] type Parent = Tree { type CL <: self.MyLetter; type RS <: self.Sender }

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
  protected def abandon(): Boolean = parent.map(_.reject(self,isActive)).getOrElse(false)

  /** Register this actor. */
  private[actors] override def register(prename: String): String =
    parent match
    /* Childeren register at the parent. */
      case Some(p) => p.adopt(prename,self)
      /* If this is the root of the family then we register at the guard. */
      case None    => super.register(prename)

  /**
   * The path returns the full lineage of this actor: dot separated names of all parents.
   * The dot can be replaced by your own char by overriding the familySepChar. */
  final override val path: String = parent match
    case Some(p) => s"${p.path}${context.familyPathSeparator}$name"
    case None    => name

