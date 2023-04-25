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

transparent private trait ActorShare(prename: String) extends BareActor :

  /* Delivers the started() callback to the user. */
  private[actors] final def deliverStarted(): Unit = started()

  /* Delivers the stopped() callback to the user. */
  private[actors] def deliverStopped(cause: Actor.Stop, complete: Boolean): Unit = stopped(cause,complete)

  /**
   * Called after actor construction and guaranteed before the first message is processed. Use this to perform
   * work to initialize the actor. Apart from a few instructions, work should not be done in the constructor itself
   * since this effectively runs in the thread of an other actor. The method started() runs in its own thread.
   * Do not forget to call start() upon the actor after creation if you want quick handling of the started()
   * or if the actor may never receive any letters. Override this with your own implementation. */
  protected def started(): Unit = ()

  /**
   * Called before actor deactivation and guaranteed after the last message is processed. If there were any
   * unprocessed letters in this actor at tear down, complete is false. These could be in the normal mailbox
   * or on the stash, if present. Cause returns the last stop mode, so the cause of stopping this actor is
   * known.
   * In case of a actorContext shutdown this is NOT called, for this disruptively terminates the processing loops.
   * It is however called when stop(...) is used, or when the actor is shutdown by a parent. The actor may still
   * be around after this method is called, but will never accept new messages. The parent is still defined,
   * when stopped() is executed (but may already stopped processing messages) but all the children
   * will already be removed from the list, and their stopped() methods have already been called.
   * If you rely on started() and stopped() to always come in pairs, do call start() on the actor after creation.
   * Otherwise stopped() may be called without started() when the actor processed no letters. */
  protected def stopped(cause: Actor.Stop, complete: Boolean): Unit = ()

  /** The final name of this actor. It will be the name given, or a generated name for unnamed actors and workers */
  final val name = register(prename)
