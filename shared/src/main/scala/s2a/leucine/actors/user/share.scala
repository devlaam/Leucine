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


  /* Provides the default implementation for handling an exception. */
  private[actors] def defaultExcept[Sender >: Common <: Accept](letter: MyLetter[Sender], sender: Sender): Unit =
    /* This counts as a failed message, if this exception is not handled. */
    synchronized { failed += 1 }
    /* If this exception is not handled, the letter is registered as unreadable. */
    ActorGuard.fail(Actor.Post(Actor.Mail.Unreadable,path,letter,sender))


  /**
   * Called after actor construction and guaranteed before the first message is processed. Use this to
   * perform work to initialize the actor. Apart from a few instructions, work should not be done in
   * the constructor itself since this effectively runs in the thread of the actor that constructed this
   * actor. The method started() runs in its own thread. Override this with your own implementation. */
  protected def started(): Unit = ()

  /**
   * Called before actor deactivation and guaranteed after the last message is processed. If there were
   * any unprocessed messages in this actor at tear down, complete is false. These could be in the normal
   * mailbox or on the stash, if present. Cause returns the last stop mode, so the cause of stopping
   * this actor is known. In case of a actorContext shutdown this is NOT called, for this disruptively
   * terminates all processing loops. It is however called when stop(...) is used, or when the actor
   * is shutdown by a parent. The actor may still be around after this method is called, but will never
   * accept new messages. The parent is still defined, when stopped() is executed (but may already
   * stopped processing messages) but all the children will already be removed from the list, and their
   * stopped() methods have already been called. Apart from the situation described above, you can rely
   * on started() and stopped() to always come in pairs, even when no messages are processed at all. */
  protected def stopped(cause: Actor.Stop, complete: Boolean): Unit = ()

  /* Use to distinguish between basic and other actors. AcceptActors does not have sender as parameter. */
  extension (fc: FamilyRelay)
    /**
     * Forward a message to all children with indexed name, workers and auto named.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (children that were not given an explicit name) or workers. */
    protected def relayAll[Sender >: fc.FamilyCommon <: fc.FamilyAccept](letter: fc.MyFamilyLetter[Sender], sender: Sender): Int =
      fc.relayEnvGrouped(letter,sender,true,true,true)
     /**
     * Forward a message to children of which the name passes the test 'include'.
     * Returns the number of children that accepted the letter. Does not include
     * auto named children (children that were not given an explicit name) or workers. */
    protected def relayFilter[Sender >: fc.FamilyCommon <: fc.FamilyAccept](letter: fc.MyFamilyLetter[Sender], sender: Sender, include: String => Boolean): Int =
      fc.relayEnvFilter(letter,sender,include)
    /**
     * Forward a message to children per group: indexed and/or workers and/or children that were given
     * an automatic name, i.e. children that were not given an explicit name.
     * Returns the number of children that accepted the letter.  */
    protected def relayGrouped[Sender >: fc.FamilyCommon <: fc.FamilyAccept](letter: fc.MyFamilyLetter[Sender], sender: Sender, toIndexed: Boolean, toWorkers: Boolean, toAutoNamed: Boolean): Int =
      fc.relayEnvGrouped(letter,sender,toIndexed,toWorkers,toAutoNamed)
    /**
     * Forward a message to one specific child on the basis of its name. Returns true if successful and
     * false if that child is not present or does not accept the letter. */
    protected def pass[Sender >: fc.FamilyCommon <: fc.FamilyAccept](letter: fc.MyFamilyLetter[Sender], sender: Sender, name: String): Boolean =
      fc.passEnv(letter,sender,name)


  /** The final name of this actor. It will be the name given, or a generated name for unnamed actors and workers */
  final val name = register(prename)


/** Shared definitions for all Defines */
transparent private trait ShareDefine :
  /** Use this inside the actor to allow the anonymous sender in Accept for example. */
  type Anonymous = Actor.Anonymous.type
  /** Define the State you want to modify. Note: if you do not want/have this, mixin Stateless. */
  type State <: Actor.State
  /** Define the initial value of the state. */
  def initial: State
