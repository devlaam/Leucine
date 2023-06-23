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


/** Mixin for the situation the child does not accept relayed messages. */
transparent private trait FamilyNoSelect[Parent <: Actor.Parent] :
  self: BareActor =>

  /* In case the actor is not able to accept relayed messages there are no
   * special restrictions to the Parent type. */
  private[actors] type PA = Parent


/** Mixin for the situation the child does accept relayed messages. */
transparent private trait FamilyDoSelect[Parent <: Actor.Parent] :
  self: BareActor =>

  /* In case the actor is able to accept relayed messages there are
   * special restrictions to the Parent Family types in relation to this
   * child. These are enforced here. */
  private[actors] type PA = Parent {
    type FamilyAccept <: self.Accept
    type FamilyCommon >: self.Common
    type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] <: self.MyLetter[Sender] }
