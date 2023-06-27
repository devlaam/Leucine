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
 * Mixin you need to create the root actor and setup a family tree. * There may be multiple family
 * trees in the system, each with its own root. Use this mixin if the children are independent,
 * in the sense that messages are not relayed unmatched from parent to child (using a relay
 * method. Messages can always be resend to the child after the type has become known by
 * type matching of course. */
trait FamilyRoot extends FamilyParent, FamilyNoRelay, FamilyMain, ActorInit :
  self: BareActor =>

  final override def path: String = name

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()


/**
 * Mixin you need to create the root actor and setup a family tree. * There may be multiple family
 * trees in the system, each with its own root. Use this mixin if messages need to be relayed to the
 * children (without individual type matching, using a relay method). */
trait FamilyRootRelay[Define <: FamilyDefine](private[actors] val familyDefine: Define) extends FamilyParent, FamilyRelay, FamilyMain, ActorInit :
  self: BareActor =>

  type FamilyCommon = familyDefine.FamilyCommon
  type FamilyAccept = familyDefine.FamilyAccept
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = familyDefine.FamilyLetter[Sender]

  final override def path: String = name

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()
