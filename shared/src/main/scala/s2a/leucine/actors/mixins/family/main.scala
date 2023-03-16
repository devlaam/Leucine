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
 * Holds all the general methods needed for managing the family actor.
 * For internal use. This is always mixed in. */
transparent private trait FamilyMain extends ActorDefs :

  /** Reference to the actor context. */
  private[actors] def context: ActorContext

  /** Counter to generate a unique name for the childeren/workers of this actor. */
  private var _workersCounter: Long = 0L

  /** Get the number of worker names generated. */
  protected[actors] def workersCounter: Long = _workersCounter

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeFamily(): Option[MonitorActor.Family] = Some(MonitorActor.Family(familySize,workersCounter))

  /**
   * Generates a unique name for a new child actor within its siblings of the structure #<nr>.
   * The name is generated in the parent and given to the child (contrary to uniqueName),
   * Every name is quaranteed to be unique for this actor during its entire lifetime, where the
   * nr represents the number of workers created this way. Names that
   * start with a # are expected never to be reused, and thus the number of incarnations of this
   * named actor is not stored. This is ideal for worker actors. However, worker actors can have
   * stable names as well, as long as you know they are created/destroyed sequentially. If you just
   * need a bunch of actors on the fly to solve some tasks and then they are gone use workerName. */
  protected def workerName: String =
    _workersCounter = _workersCounter + 1
    s"${context.workerPrefix}${_workersCounter}"

