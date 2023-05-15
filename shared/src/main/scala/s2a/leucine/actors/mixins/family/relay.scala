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
 * Mixin you can use to relay messages within the family without first testing
 * the sender type. This requires that (some) letters can be received by the
 * parent as well as by all children. */
transparent private trait FamilyRelay extends FamilyTypes, ActorDefs :
  this: ControlActor & FamilyChild[true] =>

  /** The actor type of the combined children. */
  /* These type relations ensure that the ChildRelayActor accepts at least the letters from at least
   * the senders the whole family does. It may accept more. Regarding the common actors, all
   * the senders that the letters hold in common, must also be hold in common by the family. */
  type ChildActor = BareActor {
    type Accept >: FamilyAccept
    type Common <: FamilyCommon
    type MyLetter[Sender >: FamilyCommon <: FamilyAccept] >: MyFamilyLetter[Sender] }

  /**
   * Sends a letter from sender on the a specific child. Results true if the letter
   * was accepted by the child. */
  private[actors] def passOn[Sender >: FamilyCommon <: FamilyAccept](letter: MyFamilyLetter[Sender], sender: Sender)(child: ChildActor): Boolean = child.sendEnvelope(child.pack(letter,sender))

  private[actors] def relayEnvGrouped[Sender >: FamilyCommon <: FamilyAccept](letter: MyFamilyLetter[Sender], sender: Sender, toIndexed: Boolean, toWorkers: Boolean, toAutoNamed: Boolean): Int =
    def include(child: ChildActor): Boolean =
      if      child.isWorker                                            then toWorkers
      else if (toIndexed == toAutoNamed) || _index.contains(child.name) then toIndexed
      else                                                                   toAutoNamed
    val selected = _children.filter(include)
    if context.actorTracing then println(s"In actor=$path: relayAll: children.size=${_children.size}, selected.size=${selected.size}")
    selected.map(passOn(letter,sender)).count(identity)

  /**
   * Forward a message to all children, or children of which the name pass the test 'include'.
   * Returns the number of children that accepted the letter. */
  private[actors] def relayEnvFilter[Sender >: FamilyCommon <: FamilyAccept](letter: MyFamilyLetter[Sender], sender: Sender, include: String => Boolean): Int =
    val selected = _index.filter((key,_) => include(key)).values
    if context.actorTracing then println(s"In actor=$path: relay: children.size=${_children.size}, selected.size=${selected.size}")
    selected.map(passOn(letter,sender)).count(identity)

  /**
   * Forward a message to one specific child on the basis of its name. Returns true if successful and
   * false if that child is not present or does not accept the letter. */
  private[actors] def passEnv[Sender >: FamilyCommon <: FamilyAccept](letter: MyFamilyLetter[Sender], sender: Sender, name: String): Boolean =
    _index.get(name).map(passOn(letter,sender)).getOrElse(false)


object NoRelay extends FamilyDefine :
  type WithRelay = false
  type FamilyAccept = Nothing
  type FamilyCommon = Nothing
  type MyFamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = Nothing

