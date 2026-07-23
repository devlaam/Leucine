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

/* Methods stub for when there is no state mixin used. */
private trait StateDefs extends BareDefs :
  private[actors] def storeState(receive: Receive): Unit = ()

/** Mixin to make your actor stateful. Supply the Type and initial value of your State. */
trait StateAid[AState <: Actor.State](initial: AState) extends ActorInit, ActorDefs :
  type State = AState

  /* Variable that keeps track of the transient state between processes. Note that although access on
   * state may be from different threads, it is strictly sequential, so there is no need to protect
   * manipulations. It is initialized with the initial state before the first letter is processed. */
  private var _state: State = initial

  /** Method to set the new value of the state after the letter has been processed. */
  private[actors] final override def storeState(receive: Receive): Unit =
    /* Only when we have an actual new state we update the value. */
    if receive != () then _state = receive.asInstanceOf[State]

  /**
   * Current state available in the actor for generating a new one. Make sure to only call upon its
   * value when inside the receive method. */
  protected def state: State = _state

  /* Called to count this trait */
  private[actors] override def initCount: Int = super.initCount + 1

  /* Signal that this trait is instantiated */
  initReady()
