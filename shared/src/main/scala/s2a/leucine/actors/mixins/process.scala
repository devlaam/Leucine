package s2a.leucine.actors

import scala.quoted.ToExpr.NilToExpr

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

/** Mixin if you need to store letters away.  */
trait ProcessAid extends ActorInit, ActorDefs :
  this: BareActor =>

  /* The stack holds all processes that were added by the user. Init is first (and default) process
   * that is always available. It may be the only one. So calling `head` on the stack will never give
   * rise to an exception.  */
  private val stack: StackQueue[Process] =
    val init = new Process { def apply[Sender >: Common <: Accept] = process }
    StackQueue(init)

  /** Captures the viable unmatched handler and uses it in case a letter is not read. */
  protected def unmatched[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Receive

  /* The receive method is overridden here and may not be implemented by the user any more. */
  final protected def receive[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Receive =
    def failing[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive] =
      case (letter,sender) => unmatched(letter,sender)
    (stack.head.apply orElse failing)(letter,sender)

  /**
   * This is the first process to be called and must be implemented by the user. The idea is that the user
   * may implement more methods with comparable signature to be able to switch between them. */
  protected def process[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive]

  /**
   * Process class to be instantiated by the user to define a new process. See the Process object for
   * examples how this can be done. */
  protected trait Process :
    /** Other processes can be implemented by defining apply in the instance of Process. */
    def apply[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive]

  /** Switch the process from one to an other, see the Process object for the correct use. */
  def switch(action: Process.Action): Unit = action match
    case proc: Process.Replace => stack.patch(proc)
    case proc: Process.Push    => stack.push(proc)
    case Process.Pop(n)        => stack.pop(n)
    case Process.Keep(n)       => stack.preserve(n)
    case Process.Clear         => stack.clear()

  /** Object Process for user to manipulate the Process */
  protected object Process :
    /** All user defined process actions are derived from this trait. */
    sealed trait Action
    /**
     * Action to replace the current process by a new one. If this is the first 'new' process
     * calling this is equivalent to a Process.Push, if not, the last process is remove.
     * Define the new process as an apply method (precise signature depends on actor type):
     *   switch(new Process.Replace { def apply[Sender <: Accept] = myProcess1 } )
     * where myProcess1 is the partial function containing an alternative treatment of all
     * letters. */
    abstract class Replace extends Process, Action
    /**
     * Action to add a new process on top of the current process. Define the new process
     * as an apply method (precise signature depends on actor type):
     *   switch(new Process.Push { def apply[Sender <: Accept] = myProcess2 } )
     * where myProcess2 is the partial function containing an alternative treatment of all
     * letters. */
    abstract class Push extends Process, Action
    /**
     * Removes the current process from the process stack, so the one before becomes the current
     * one. If this action removes the last process, the initial process will be restored. You
     * may remove more processes at once. If you remove more processes than are available on the
     * stack, this is equivalent to a Process.Clear. For n <= 0  no action is taken. Without
     * parameter, n is assumed to be 1.  Use it like:
     *   switch(Process.Pop(2))
     */
    case class Pop(n: Int = 1) extends Action
    /**
     * Remove just enough process from the stack to keep n processes left. If there are already
     * n or less processes present, no action is taken. if n <= 0, this is equivalent to a
     * Process.Clear. Example of use:
     *  switch(Process.Keep(2)) */
    case class Keep(n: Int) extends Action
    /**
     * Removes all processes from the stack and restores the initial process as the current one.
     * Example of use:
     *  switch(Process.Clear) */
    object Clear extends Action

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()

