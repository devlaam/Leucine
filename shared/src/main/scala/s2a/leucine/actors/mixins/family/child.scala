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
 * Holds all the methods needed for managing the children of the family actor member.
 * For internal use. Not all families have children, so this is only mixed in
 * when children are expected. */
transparent private trait FamilyChild extends ActorDefs :
  this: ControlActor =>

  /* Local types */
  private[actors] type RS <: Actor
  private[actors] type CL[T <: RS] <: Actor.Letter

  /** The type for all Senders for messages that can be relayed between parent and child. */
  type ChildSender = RS

  /** The super type for the letters the children may receive. */
  type ChildLetter[T <: ChildSender] = CL[T]

  /** The actor type of the combined children. */
  type ChildActor = BareActor { type Sender >: ChildSender; type MyLetter[T <: ChildSender] >: ChildLetter[T] }

  /** Reference to the actor context. */
  private[actors] def context: ActorContext

  /** Generates a unique name for a new child actor within its siblings of the structure #<nr>. */
  private val worker = new Worker

  /**
   * Variable that holds the termination result from the actor in case
   * we have to wait for the children to finish */
  private var termination: Option[Boolean] = None

  /** Variable that holds all the children of this actor. */
  private var _children: Set[ChildActor] = Set.empty

  /** Variable that holds all indexed children of this actor. */
  private var _index: Map[String,ChildActor] = Map.empty

  /** Variable that holds all names of the children that are being removed */
  private var removed: List[String] = Nil

  /** Take a snapshot of the internals of this actor. */
  private[actors] override def probeFamily(): Option[MonitorAid.Family] = Some(MonitorAid.Family(_index.size,_children.size,worker.size))

  /**
   * Save access to the children of this actor. Note, this is temporary copy,
   * so it may already have changed after being read. */
  protected[actors] def children: Set[ChildActor] = _children

  /**
   * Pass the dropped needles to all children. Needle dropping is to test if the actors has gone silent.  */
  private[actors] override def familyDropNeedle(): Unit =
    /* If needles are dropped from a parent, this is not the root, so we say false here. That is important
     * since needles dropped by the root are treated differently. */
    _children.foreach(_.dropNeedle(false))

  /**
   * Save access to the index of this actor. Note, this is temporary copy,
   * so it may already have changed after being read. */
  protected[actors] def index: Map[String,ChildActor] = _index

  /** Get the current number of children. For internal use, not synchronized. */
  private[actors] override def familySize: Int = _children.size

  /**
   * Recursively stop the whole family tree upwards. The children are stopped before
   * the parent, but it is not guaranteed that they will also finish before the parent. The
   * actual stopping takes place after the current letter/mailbox has finished processing, and
   * the children may finish their letters after the parents. This truth also holds recursively.
   * If the parent is the only actor that sends letters to the children, all mailboxes are
   * cleaned from the bottom up. Afterwards, the actors are removed from the children's
   * map, by there own doing. So it may take a little while before there references are
   * removed. */
  private[actors] override def familyStop(finish: Boolean): Unit = synchronized {
    val stop = if finish then Actor.Stop.Finish else Actor.Stop.Direct
    _children.foreach(_.stop(stop)) }

  /**
   * Called when this actor still has children (_children guaranteed to be non empty). Defers
   * the termination to the moment the last child says goodbye. */
  private[actors] override def familyTerminate(complete: Boolean): Unit = termination = Some(complete)

  /**
   * Adopt child actors with the given name. Once adopted, the actor is removed after it
   * stopped working, which is automatic. Returns the new real name. */
  private[actors] def adopt(prename: String, child: ChildActor): String =
    /* If the name is empty, an unique name is given. If the name starts with a worker prefix a new
     * free worker name is generated. In these situations the actor is not put in the
     * index. But, if an actor under the prename already exists, its index entry overwritten. */
    val rename = Auxiliary.rename(prename,child,worker,context.workerPrefix)
    synchronized {
      if context.actorTracing then println(s"In actor=$name:  adopting: $name")
      /* All children are added. */
      _children += child
      /* If required add it to the index. If the name already exists, overwrite the entry.
       * There is nothing we can realistically do to save the  day. */
      if rename.inIndex then _index += rename.name -> child }
    rename.name

  /**
   * Reject a child with a given name. For internal use. Normally, there should not be a reason
   * for the user to do so, but when you want to prohibit the termination of an actor when the
   * parent stops, this could be one. The parent cannot be removed from the child you reject,
   * so the parent may stop, but will be disposed off only after the child has stopped as well.
   * A manual call to reject while active also issues a callback 'abandoned' with the children's
   * name later on.  */
  private[actors] def reject(child: ChildActor, keep: Boolean): Boolean = synchronized {
    /* Test if the child is even present. The user may also call this and misspell the name. */
    if !_children.contains(child) then false else
      /* Remove the child from the list and index */
      _index    -= child.name
      _children -= child
      /* Now see which action has to be taken. */
      termination match
        /* If we are terminating, and this is the last child, call the deferred termination steps. */
        case Some(complete) => if _children.isEmpty then deferred(processTerminate(complete))
        /* If we are in normal operation (most likely the child is terminating) */
        case None =>
          if keep then ActorGuard.add(child)
          /* In case we are still active, put them on the list to be reported as abandoned. A process
           * tickle may be needed in case the mailbox is empty so the callbacks are still handled.
           * This is no core process however. */
          if activity.active then { removed = child.name :: removed ; processTrigger(false) }
      true }

  /** See if there are any children removed that we did not yet report. */
  private[actors] override def familyRemoved: Boolean = synchronized { !removed.isEmpty }

  /** Report all children that rejected themselves (abandoned the parent) as gone. */
  private[actors] override def familyReport(): Unit =
    /* Get the removed names and clear the list. Note that the order of the callbacks is
     * reversed to the order of removal, but since the addition of the names to the removed
     * children list can be any, there is no rationale for reversing this list. */
    val (report,barren) = synchronized {
      val result = removed
      removed = Nil
      (result,_children.isEmpty && (stopper == Actor.Stop.Barren)) }
    report.foreach(abandoned)
    if barren then stopWith(true)

  /**
   * Override this method to see which child has left the parent. This method may arrive some time
   * later as the actual removal. Every child that has been removed when the actor was still active
   * will generate a callback. However, there may be removals between deactivation of the actor and
   * the complete termination, for example after a stopDirect() or stopFinish() call. These will
   * not generate a callback. Also, the number of children may not decrease by one each call.
   * More children could be removed in the mean time. However, as long as no new children are adopted,
   * you may assume the number of children will never increase. Once it is zero, all children are
   * gone, although there still may be callbacks queued up. The order of removal does not correspond
   * to the order of the callbacks. These can be in any order.
   * Furthermore, you cannot assume you can send letters to the child up to the moment this callback
   * arrives. The child most likely is already out of scope for a while. The callback comes 'eventually'. */
  protected def abandoned(name: String): Unit = ()


  /** Get the actor with this path/name if it exists. It will recurse into the family tree if needed. */
  protected[actors] def get(path: String): Option[Actor] = FamilyChild.searchFor(path,context.familyPathSeparator,_index)

  /**
   * Sends a letter from sender on the a specific child. Results true if the letter
   * was accepted by the child. */
  private[actors] def passOn[T <: ChildSender](letter: ChildLetter[T], sender: T)(child: ChildActor): Boolean = child.sendEnvelope(child.pack(letter,sender))

  private[actors] def relayEnvGrouped[T <: ChildSender](letter: ChildLetter[T], sender: T, toIndexed: Boolean, toWorkers: Boolean, toAutoNamed: Boolean): Int =
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
  private[actors] def relayEnvFilter[T <: ChildSender](letter: ChildLetter[T], sender: T, include: String => Boolean): Int =
    val selected = _index.filter((key,_) => include(key)).values
    if context.actorTracing then println(s"In actor=$path: relay: children.size=${_children.size}, selected.size=${selected.size}")
    selected.map(passOn(letter,sender)).count(identity)

  /**
   * Forward a message to one specific child on the basis of its name. Returns true if successful and
   * false if that child is not present or does not accept the letter. */
  private[actors] def passEnv[T <: ChildSender](letter: ChildLetter[T], sender: T, name: String): Boolean =
    _index.get(name).map(passOn(letter,sender)).getOrElse(false)


/* Contains method specific to the FamilyChild. */
private[actors] object FamilyChild :
  /** General method to search a the family tree. */
  def searchFor(path: String, separator: Char, actors: Map[String,Actor]): Option[Actor] =
    Auxiliary.splitAt(path,separator) match
      /* In case there is no rest path available, the actor with this name should now be present at this level. */
      case (name,"")   => Left(actors.get(name))
      /* If there is some more to the path, that must be a child of the actor with the prior name. */
      case (name,rest) => actors.get(name) match
        /* So it must be of the type FamilyChild to be able to have children. */
        case Some(fc: FamilyChild) => Right(fc: FamilyChild,rest)
        /* If not, this actor does not exists. */
        case _                     => Left(None)
    /* In order to make sure we get no nested stack frames we must end with the drill down call on the child. */
    match
      case Left(actorOpt) => actorOpt
      case Right(fc,rest) => fc.get(rest)
