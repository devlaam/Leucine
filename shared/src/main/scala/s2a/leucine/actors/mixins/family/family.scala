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
 * types of the parent. */
trait FamilyDefine :
  /** The type for all Senders for messages that can be relayed between parent and child. */
  type FamilyAccept <: Actor
  /** The bottom type for all common letters. */
  type FamilyCommon <: FamilyAccept
  /** The super type for the letters the children may receive. */
  type FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] <: Actor.Letter[Sender]
