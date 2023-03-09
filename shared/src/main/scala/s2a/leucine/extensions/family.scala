package s2a.leucine.extensions

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


import s2a.leucine.actors.*

/** Experiment to see how easy the user can extend the possibilities of the actors. */
trait FamilyChildExtra :
  /* The type of the sender. */
  type Sender

  /* The type for all Senders for messages that can be relayed between parent and child. */
  type RelaySender <: Actor[?]

  /* The super type for the letters the childeren may receive. */
  type ChildLetter <: Actor.Letter

  /**The actor type of the children. */
  type ChildActor = BareActor[ChildLetter, RelaySender,?]


  /* Methods to extend. */
  protected def children: Map[String,ChildActor]
  protected def relay(letter: ChildLetter, sender: RelaySender, include: String => Boolean): Int
  protected def pass(letter: ChildLetter, sender: RelaySender, name: String): Boolean

  /** Forward a message to all children */
  protected def relay(letter: ChildLetter, sender: RelaySender): Int = relay(letter,sender,_ => true)

  /** Test if the actor has a child with this name. */
  protected def has(name: String): Boolean = children.contains(name)

  /** Get the child actor with this name. if it exists. */
  protected def get(name: String): Option[ChildActor] = children.get(name)
