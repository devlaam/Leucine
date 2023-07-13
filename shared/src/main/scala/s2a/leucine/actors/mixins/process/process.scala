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


/* Methods stub for when there is no stash mixin used. */
private trait ProcessDefs extends BareDefs

/** Mixin if you need to make use of partial functions for the message processing. */
private transparent trait ProcessAid extends ActorInit, ActorDefs :
  this: BareActor =>

  /* The stack holds all processes that were added by the user. Init is first (and default) process
   * that is always available. It may be the only one. So calling `head` on the stack will never give
   * rise to an exception.  */
  private val stack: StackQueue[RedirectBase] =
    val init = new RedirectBase { def apply[Sender >: Common <: Accept] = processShared }
    StackQueue(init)

  /** Captures the viable unmatched handler and uses it in case a letter is not read. */
  private[actors] protected def unmatchedShared[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Receive

  /**  Shared receive method, called by the receive method from the actor. */
  private[actors] protected def receiveShared[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Receive =
    /* Handles the situation where the message is not processed by the user.*/
    def failing[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive] =
      case (letter,sender) => unmatchedShared(letter,sender)
    /* First try to process the message by the topmost process handler, if it fails, fallback on unmatched. */
    (stack.head.apply orElse failing)(letter,sender)

  /** Shared process method, to be indirectly called by the user. */
  private[actors] protected def processShared[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive]

  /**
   * Process class to be instantiated by the user to define a new process. See the Process object for
   * examples how this can be done. */
  private[actors] transparent protected trait RedirectBase :
    /** Other processes can be implemented by defining apply in the instance of Process. */
    def apply[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive]

  /** All user defined process actions are derived from this trait. */
  protected sealed trait ProcessAction

  /** Switch the process from one to an other, see the Process object for the correct use. */
  protected def switch(action: ProcessAction): Unit = action match
    case p: ProcessBase#ReplaceBase => stack.patch(p)
    case p: ProcessBase#PushBase    => stack.push(p)
    case p: ProcessBase#Pop         => stack.pop(p.n)
    case p: ProcessBase#Keep        => stack.preserve(p.n)
    case _: ProcessBase#Clear       => stack.clear()

  /** Base class for the access object to define the stack manipulation actions. */
  private[actors] protected class ProcessBase :
    /**
     * Action to replace the current process by a new one. If this is the first 'new' process
     * calling this is equivalent to a Process.Push, if not, the last process is removed.
     * Define the new process as an apply method (precise signature depends on actor type):
     *   switch(new Process.Replace { def process[Sender <: Accept] = myProcess1 } )
     * where myProcess1 is the partial function containing an alternative treatment of all
     * letters. */
    private[actors] abstract class ReplaceBase extends RedirectBase, ProcessAction
    /**
     * Action to add a new process on top of the current process. Define the new process
     * as an apply method (precise signature depends on actor type):
     *   switch(new Process.Push { def process[Sender <: Accept] = myProcess2 } )
     * where myProcess2 is the partial function containing an alternative treatment of all
     * letters. */
    private[actors] abstract class PushBase extends RedirectBase, ProcessAction
    /**
     * Removes the current process from the process stack, so the one before becomes the current
     * one. If this action removes the last process, the initial process will be restored. You
     * may remove more processes at once. If you remove more processes than are available on the
     * stack, this is equivalent to a Process.Clear. For n <= 0  no action is taken. Without
     * parameter, n is assumed to be 1.  Use it like:
     *   switch(Process.Pop(2))
     */
    class Pop(val n: Int = 1) extends ProcessAction
    /**
     * Remove just enough processes from the stack to keep n processes left. If there are already
     * n or less processes present, no action is taken. if n <= 0, this is equivalent to a
     * Process.Clear. Example of use:
     *  switch(Process.Keep(2)) */
    class Keep(val n: Int) extends ProcessAction
    /**
     * Removes all processes from the stack and restores the initial process as the current one.
     * Example of use:
     *  switch(Process.Clear()) */
    class Clear extends ProcessAction

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()
