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



trait FamilyDefine :
  type FamilyAccept <: Actor
  type FamilyCommon <: FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] <: Actor.Letter[Sender]

//Example that can be used for families that accept all letters ???
object FamilyDefineAccept extends FamilyDefine :
  type FamilyAccept = Actor
  type FamilyCommon = Actor
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = Actor.Letter[Sender]

object FamilyDefineRefuse extends FamilyDefine :
  type FamilyAccept = Nothing
  type FamilyCommon = Nothing
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = Nothing

