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


/** Trait to choose between a family with or without relaying. For internal use only. */
transparent private trait FamilySelect[RSP <: Boolean, Parent <: Actor.Parent[RSP]] :
  self: BareActor =>

  /* These type relations ensure that the ChildActor accepts at least the letters from at least
   * the senders the whole family does. It may accept more. Regarding the common actors, all
   * the senders that the letters hold in common, must also be hold in common by the family. */
  private[actors] type PAEXT = Parent {
    type FamilyAccept <: self.Accept
    type FamilyCommon >: self.Common
    type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] <: self.MyLetter[Sender] }

  /** Which Parent type is used depends on the RelaySelector in the Parent. */
  private[actors] type PA <: Actor.Parent[RSP] = RSP match
     case true   => PAEXT
     case false  => Parent

