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

  /* Local type */
  private[actors] type CL
  private[actors] type RS

  /** The type for all Senders for messages that can be relayed between parent and child. */
  type ChildSender = RS

  /** The super type for the letters the childeren may receive. */
  type ChildLetter = CL

  /** The actor type of the combined children. */
  type ChildActor = BareActor { type MyLetter >: ChildLetter ; type Sender >: ChildSender }

  /** Reference to the actor context. */
  private[actors] def context: ActorContext

  /**
   * Variable that holds the termination result from the actor in case
   * we have to wait for the children to finish */
  private var termination: Option[Boolean] = None

  /** Stop on barren flag. If this is true the actor will stopFinish when all children are gone. */
  private var stopOnBarren: Boolean = false

  /** See if this actor is still active. */
  def isActive: Boolean

  /** The mailbox is processed, but no more letters are accepted. Terminate afterwards. */
  def stopFinish(): Unit

  /** Execute an action later on the context. */
  private[actors] def deferred(action: => Unit): Unit

  /** Triggers the processLoop into execution, depending on the phase. */
  private[actors] def processTrigger(): Unit

  /** Last goodbyes of this actor. */
  private[actors] def processTerminate(complete: Boolean): Unit

  /** Variable that holds all the children of this actor. */
  private var _children: Set[ChildActor] = Set.empty

  /** Variable that holds all indexed children of this actor. */
  private var _index: Map[String,ChildActor] = Map.empty

  /** Variable that holds all names of the children that are being removed */
  private var removed: List[String] = Nil

  /** Generates a unique name for a new child actor within its siblings of the structure #<nr>. */
  protected def workerName: String

  /**
   * Save access to the children of this actor. Note, this is temporary copy,
   * so it may already have changed after being read. */
  protected[actors] def children: Set[ChildActor] = _children

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
   * cleaned from the bottom up. Afterwards, the actors are removed from the childrens
   * map, by there own doing. So it may take a little while before there references are
   * removed. */
  private[actors] override def familyStop(finish: Boolean): Unit = synchronized { _children.foreach(_.stopWith(finish)) }

  /**
   * Called when this actor still has childeren (_children guaranteed to be non empty). Defers
   * the termination to the moment the last child says goodbye. */
  private[actors] override def familyTerminate(complete: Boolean): Unit = termination = Some(complete)

  /**
   * Adopt child actors with the given name. If the name is empty, a unique name is given. If the name starts
   * with a worker prefix a new free worker name is generated. In these situations the actor is not put in the
   * index. But, if an actor under the prename already exists, its index entry overwritten. Once adopted, the actor is only removed after it
   * stopped working. This is automatic. Returns the new real name if needed. */
  private[actors] def adopt(prename: String, child: ChildActor): String =
    val (name,index) =
      /* If there is no prename, generate a unique name, based on the childs class name, but prohibit indexing. */
      if      prename.isEmpty                          then (child.uniqueName,false)
      /* If we want a worker, generate a new free worker name and  prohibit indexing. */
      else if prename.startsWith(context.workerPrefix) then (workerName,false)
      /* If this actor was give a name by hand use that, and try to index it. */
      else                                                  (prename,true)
    synchronized {
      if context.actorTracing then println(s"In actor=$name:  adopting: $name")
      /* All childeren are added. */
      _children += child
      /* If required add it to the index. If the name already exists, overwrite the entry.
       * There is nothing we can realisticly do to save the  day. */
      if index then _index += name -> child }
    name

  /**
   * Reject a child with a given name. For internal use. Normally, there should not be a reason
   * for the user to do so, but when you want to prohibit the termination of an actor when the
   * parent stops, this could be one. The parent cannot be removed from the child you reject,
   * so the parent may stop, but will be disposed off only after the child has stopped as well.
   * A manual call to reject while active also issues a callback 'abandoned' with the childs
   * name lateron.  */
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
          if keep then ActorGuard.add("",child) // dit kan gevaarlijk zijn ivm deadlock!
          /* In case we are still active, put them on the list to be reported as abandoned. A processTrigger()
           * may be needed in case the mailbox is empty so the callbacks are still handled. */
          if isActive then { removed = child.name :: removed ; processTrigger() }
      true }

  private[actors] override def familyRemoved: Boolean = synchronized { !removed.isEmpty }

  /** Report all childeren that rejected themselves (abandoned the parent) as gone. */
  private[actors] override def familyReport(): Unit =
    /* Get the removed names and clear the list. Note that the order of the callbacks is
     * reversed to the order of removal, but since the addition of the names to the removed
     * childeren list can be any, there is no rationale for reversing this list. */
    val (report,stop) = synchronized {
      val result = removed
      removed = Nil
      (result,_children.isEmpty && stopOnBarren) }
    report.foreach(abandoned)
    if stop then stopFinish()

  /**
   * Stop this actor with a stopFinish() after all its children have stopped (active=true). If the actor
   * has no childeren at the moment of this call, it will stopFinish() directly. As long as the stop did not
   * kick in, it may be cancelled with an other call with active = false. The stopFinish is called
   * internally after all abandoned callbacks had a chance to run. If the remaining letters in the mailbox
   * create new childeren, the teardown procedure is not cancelled. The new children are stopped as well. */
  final protected def stopBarren(active: Boolean): Unit = synchronized {
    stopOnBarren = active
    if active && _children.isEmpty then stopFinish() }

  /**
   * Override this method to see which child has left the parent. This method may arrive some time
   * later as the actual removal. Every child that has been removed when the actor was still active
   * will generate a callback. However, there may be removals between deactivation of the actor and
   * the complete termination, for example after a stopDirect() or stopFinish() call. These will
   * not generate a callback. Also, the number of childeren may not decrease by one each call.
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
  private[actors] def passOn(letter: ChildLetter, sender: ChildSender)(child: ChildActor): Boolean = child.sendEnvelope(child.pack(letter,sender))

  private[actors] def relayEnvGrouped(letter: ChildLetter, sender: ChildSender, toIndexed: Boolean, toWorkers: Boolean, toAutoNamed: Boolean): Int =
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
  private[actors] def relayEnvFilter(letter: ChildLetter, sender: ChildSender, include: String => Boolean): Int =
    val selected = _index.filter((key,_) => include(key)).values
    if context.actorTracing then println(s"In actor=$path: relay: children.size=${_children.size}, selected.size=${selected.size}")
    selected.map(passOn(letter,sender)).count(identity)

  /**
   * Forward a message to one specific child on the basis of its name. Returns true if successful and
   * false if that child is not present or does not accept the letter. */
  private[actors] def passEnv(letter: ChildLetter, sender: ChildSender, name: String): Boolean =
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
    /* In order to make sure we get no nested stackframes we must end with the drill down call on the child. */
    match
      case Left(actorOpt) => actorOpt
      case Right(fc,rest) => fc.get(rest)
