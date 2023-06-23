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
 * Mixin you need to create child actors, that allow for have children of their own. Actual creation could
 * be done within or outside the parent but without enclosing any of its variable state. You need to specify the
 * type of the parent here. Also, your actor class needs to implement the parent itself. The best way to do
 * this is to make it a class parameter. That way you are obliged to define it at creation. This mixin does
 * not allow to relay messages to its own children, not is it able to receive relayed messages from the parent. */
trait FamilyBranch[Parent <: Actor.Parent & FamilyNoRelay] extends FamilyChild, FamilyNoRelay, FamilyMain, FamilyParent, FamilyNoSelect[Parent], ActorInit :
  self: BareActor  =>

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin you need to create child actors, that allow for have children of their own. Actual creation could
 * be done within or outside the parent but without enclosing any of its variable state. You need to specify the
 * type of the parent here. Also, your actor class needs to implement the parent itself. The best way to do
 * this is to make it a class parameter. That way you are obliged to define it at creation. This mixin is
 * able to relay messages to its own children, but not to receive relayed messages from the parent. */
trait FamilyBranchRelay[Parent <: Actor.Parent & FamilyNoRelay, Define <: FamilyDefine](private[actors] val familyDefine: Define) extends FamilyChild, FamilyRelay, FamilyMain, FamilyParent, FamilyNoSelect[Parent], ActorInit :
  self: BareActor  =>

  /* Explicit Family type definitions */
  type FamilyCommon = familyDefine.FamilyCommon
  type FamilyAccept = familyDefine.FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = familyDefine.FamilyLetter[Sender]

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin you need to create child actors, that allow for have children of their own. Actual creation could
 * be done within or outside the parent but without enclosing any of its variable state. You need to specify the
 * type of the parent here. Also, your actor class needs to implement the parent itself. The best way to do
 * this is to make it a class parameter. That way you are obliged to define it at creation. This mixin does
 * not allow to relay messages to its own children, but is able to receive relayed messages from the parent. */
trait FamilyBranchRelayed[Parent <: Actor.Parent & FamilyRelay] extends FamilyChild, FamilyNoRelay, FamilyMain, FamilyParent, FamilyDoSelect[Parent], ActorInit :
  self: BareActor  =>

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin you need to create child actors, that allow for have children of their own. Actual creation could
 * be done within or outside the parent but without enclosing any of its variable state. You need to specify the
 * type of the parent here. Also, your actor class needs to implement the parent itself. The best way to do
 * this is to make it a class parameter. That way you are obliged to define it at creation. This mixin is
 * able to relay messages to its own children, and able to receive relayed messages from the parent. */
trait FamilyBranchRelayRelayed[Parent <: Actor.Parent & FamilyRelay, Define <: FamilyDefine](private[actors] val familyDefine: Define) extends FamilyChild, FamilyRelay, FamilyMain, FamilyParent, FamilyDoSelect[Parent], ActorInit :
  self: BareActor  =>

  /* Explicit Family type definitions */
  type FamilyCommon = familyDefine.FamilyCommon
  type FamilyAccept = familyDefine.FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = familyDefine.FamilyLetter[Sender]

  /** Internally called to remove an actor from its parents list, just before termination. */
  private[actors] override def familyAbandon(): Boolean = parent.reject(self,false)

  /** Register this actor, we are a child, so we do this at the parent. */
  private[actors] override def register(prename: String): String = parent.adopt(prename,self)

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()
