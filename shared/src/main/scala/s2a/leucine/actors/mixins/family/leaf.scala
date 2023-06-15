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
 * Mixin that you can use to terminate the family branching at this point. It is like the FamilyBranch,
 * but without the possibility to define children. */
trait FamilyLeaf[Parent <: Actor.Parent[false]] extends FamilyMain, FamilyParent[false], FamilySelect[false,Parent], ActorInit :
  self: BareActor  =>

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
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()



trait FamilyLeafRelayed[Parent <: Actor.Parent[true]] extends FamilyMain, FamilyParent[true], FamilySelect[true,Parent], ActorInit :
  self: BareActor  =>

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
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()
