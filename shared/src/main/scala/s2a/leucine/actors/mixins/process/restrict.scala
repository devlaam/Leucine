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


/** Use this mixin with the RestrictActor to make use of partial functions in the message handling. */
trait RestrictProcess extends ProcessAid :
  this: RestrictActor[?] =>

  /** Alias for the partial function type expected for the restrict actor. */
  protected type Process[Sender <: Accept] = PartialFunction[(Letter[Sender],Sender),Receive]

  /* Redirected call to match types */
  private[actors] protected def unmatchedShared[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Receive = unmatched(letter,sender)

  /* Redirected call to match types */
  private[actors] protected def processShared[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive] = process

  /** The receive method is overridden here and may not be implemented by the user any more. */
  final protected def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Receive = receiveShared(letter,sender)

  /**
   * This is the first process to be called and must be implemented by the user. The idea is that the user
   * may implement more methods with comparable signature to be able to switch between them. */
  protected def process[Sender <: Accept]: Process[Sender]

  /** Redirection trait that is instantiated to store the new process call on the stack. */
  transparent protected trait Redirect extends RedirectBase:
    final def apply[Sender >: Common <: Accept] = process
    /** Process method that should be defined by the user to handle the messages in an alternative manner. */
    def process[Sender <: Accept]: Process[Sender]

  /** Access object for the user to define the required actions. */
  protected object Process extends ProcessBase:
    /**
     * Action to replace the current process by a new one. If this is the first 'new' process
     * calling this is equivalent to a Process.Push, if not, the last process is removed.
     * Define the new process as an apply method:
     *   switch(new Process.Replace { def process[Sender <: Accept] = myProcess1 } )
     * where myProcess1 is the partial function containing an alternative treatment of all
     * letters. */
    abstract class Replace extends ReplaceBase, Redirect
    /**
     * Action to add a new process on top of the current process. Define the new process
     * as an apply method:
     *   switch(new Process.Push { def process[Sender <: Accept] = myProcess2 } )
     * where myProcess2 is the partial function containing an alternative treatment of all
     * letters. */
    abstract class Push extends PushBase, Redirect
