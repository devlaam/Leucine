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
private trait ProcessDefs extends BareDefs:
  trait ProcessOps
    //private[actors] def storeEnv[Sender >: Common <: Accept](envelope: Env[Sender]): Unit

/** Mixin if you need to store letters away.  */
trait ProcessAid extends ActorInit, ActorDefs :
  this: BareActor =>


  private val stack: StackQueue[Process] =
    val init = new Process { def apply[Sender >: Common <: Accept] = process }
    StackQueue(init)

  protected def process[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive]

  // Even proberen of dit kan.
  final protected def receive[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Receive = stack.head.apply(letter,sender)


  protected trait Process :
    def apply[Sender >: Common <: Accept]: PartialFunction[(MyLetter[Sender],Sender),Receive]


  def switch(action: Process.Action): Unit = action match
    case proc: Process.Replace => stack.patch(proc)
    case proc: Process.Push    => stack.push(proc)
    case Process.Pop(n)        => stack.pop(n)
    case Process.Keep(n)       => stack.preserve(n)
    case Process.Clear         => stack.clear() // purge?

  /** Object Process for user to manipulate the Process */
  protected object Process extends ProcessOps :
    sealed trait Action
    abstract class Replace extends Process, Action
    abstract class Push extends Process, Action
    case class Pop(n: Int = 1) extends Action
    case class Keep(n: Int) extends Action
    object Clear extends Action

  /* Tests, this is the way the Process can be used. REMOVE! */
  switch(new Process.Replace { def apply[Sender >: Common <: Accept] = process } )
  switch(new Process.Push { def apply[Sender >: Common <: Accept] = process } )
  switch(Process.Pop(2))
  switch(Process.Keep(2))
  switch(Process.Clear)

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()

