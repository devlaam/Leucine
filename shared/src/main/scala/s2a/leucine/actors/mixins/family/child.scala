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
 * Holds all the methods needed for accessing the parent of this family. For internal use.
 * Not all actors have a parent, so this is not mixed in for the root of the family. */
transparent private trait FamilyChild extends ActorDefs :

  /* Local type */
  private[actors] type FamilyParent <: Actor.Parent

  /** The type of the parent for this actor. */
  type Parent = FamilyParent

  /** Reference to the actor context. */
  private[actors] def context: ActorContext

  /**
   * Access to the parent of this actor. It should be implemented as value parameter in the
   * class definition of this actor. That way the parent is an stable reference. */
  protected def parent: Parent

  /**
   * The path returns the full lineage of this actor: dot separated names of all parents.
   * The dot can be replaced by your own char by overriding the familySepChar. */
  final override val path: String = s"${parent.path}${context.familyPathSeparator}$name"

