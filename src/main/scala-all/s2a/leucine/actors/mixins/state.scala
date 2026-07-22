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
private trait StateDefs extends BareDefs :
  private[actors] def receivedTransform(receive: Receive): Unit = ()
  private[actors] def sendedTransform(): Receive = ()

trait StateAid[MyState <: Actor.State](initial: MyState) extends ActorInit, ActorDefs :
  type State = MyState

  /**
   * Variable that keeps track of the transient state between processes.
   * Note that although access on state may be from different threads, it is strictly sequential, so
   * there is no need to protect manipulations. */
  /**
   * This defines the initial state that is used before the first letter is processed if needed.
   * The related definition must be in the actor constructor of the user code. */
  private var _state: State = initial

  protected def state: State = _state

  private[actors] final override def receivedTransform(receive: Receive): Unit =
    if receive != () then _state = receive.asInstanceOf[State]

  private[actors] final override def sendedTransform(): State = _state

  // Starks doen het misschien zo.
  // protected def start(initial: State): Unit =
  //   state = initial
  //   start()

  //protected def receive(letter: Letter, sender: Sender): State
  //protected def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): State

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()
