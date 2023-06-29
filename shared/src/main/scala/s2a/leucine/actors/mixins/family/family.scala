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
 * Base type for you family definitions. These are the types you use to define
 * the messages that need to be relayed from parent to child. Without relaying,
 * there is no need to make use of these. Note that, if you want to relay messages
 * from the outside, that is, that are received by the parent first, this parent
 * must of course also be able to receive them, which must be incorporated in the
 * types of the parent. It is good practice that, if you use relaying all children
 * are derived from the same actor type. */
trait FamilyDefine :

  /**
   * Here you define the type that all children hold in common. Often, there is only
   * one actor type for all children, in which case you may use that one. When there
   * are more, you may define the union type to be the ChildActor type. Otherwise,
   * just define this to be the BareActor, you cannot skip its definition. */
  type ChildActor <: BareActor

  /**
   * The type for all Senders for messages that can be relayed between parent and child.
   * This is analogous to the Accept type you define for each actor. The type FamilyAccept
   * should be a subtype of each child in the family otherwise the sender cannot be relayed.
   * Without relaying, there is no need to define this type. */
  type FamilyAccept <: Actor

  /**
   * The bottom type for all common letters. This type only has a hidden analogous in the
   * actors. In other words, all actor types have a Common, but you never set this yourself.
   * So what should this type be? From the requirement you learn that it should be a subtype
   * of FamilyAccept. But, it should also be a supertype of all Common's of the actors.
   * So, depending on the actor types of the children, you can use this list to choose from:
   *   AcceptActor   :   FamilyCommon = Actor
   *   RestrictActor :   FamilyCommon = Nothing
   *   SelectActor   :   FamilyCommon = union of all Accept of all children
   *   WideActor     :   FamilyCommon = Actor
   * Without relaying, there is no need to define this type. */
  type FamilyCommon <: FamilyAccept

  /**
   * The super type for the letters the children may receive. This type usually is the intersection
   * of all Letter types of all children. Derived letters may than be relay to all children as well.
   * Without relaying, there is no need to define this type. */
  type FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] <: Actor.Letter[Sender]

/** This object is used as default FamilyDefine in case there is no special type to define manually. */
object BareFamily extends FamilyDefine :
   type ChildActor = BareActor

