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

import java.util.concurrent.Callable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt


/**
 * This object is meant to be run in the main thread and enables you to exit the application
 * when all actors have finished. Put all the actors you create under guard by calling add
 * on them directly after creation. For families only the root actor should to be added. Note
 * that if you create actors within other actors which are not a adopted by a family you must
 * add them as well, or call watch(force = true), if you want to properly close the application
 * at termination. */
object ActorGuard :

  /** Collection of all actors that are relevant for keeping the thread pool alive. */
  private var actors: Set[Actor] = Set.empty

  /* Index with with all named actors to find them on their name. Not all actors of
   * stored in the set actors are present here. But all of the indexed are also
   * present in the actors, which is the 'leading collection'. */
  private var index: Map[String,Actor] = Map.empty

  /* Worker instance to give all workers, not part of a family a worker name. */
  private[actors] val worker = new Worker

  /** Contains all actors that require needle drop. */
  private var silent: Set[Actor] = Set.empty

  /** Add or remove an actor to the needle dropping for silence detection. */
  private[actors] def dropNeedles(active: Boolean, actor: Actor): Unit = synchronized {
    if active then silent += actor else silent -= actor }

  /**
   * See if all the actors that are running have completed. We do not put synchronized
   * here and rely in the atomicity of the reference variable actors and _.isActive calls.
   * Synchronizing would slow down the application and there is no problem with memory
   * inconsistency for reading the actors variable. Note that this opens the possibility that
   * the application terminates when an actor is added after the watch is called in an
   * other thread and all other actors have terminated as well. This however is an unlikely
   * scenario from a design perspective. The other threads are populated with actors, so
   * these have not terminated when they themselves created new actors.  */
  private def allTerminated: Boolean =
    import Actor.Activity.*, Actor.Stop.*
    /* Use a var to collect all actors that are running that may be stopped. */
    var haltables: List[Actor] = Nil
    /* Test this actor on its activity, return true when is has already stopped or may be stopped. */
    def act(actor: Actor): Boolean =
    /* Get its activity */
      val activity = actor.activity
      /* If the actor is allowed to be stopped ... */
      if activity == Haltable
      /* ... add it to the collection of actors that will be stopped */
      then { haltables = actor :: haltables; true }
      /* ... otherwise see if the actor has already stopped. */
      else activity == Terminated
    /* Loop all actors to see if they have stopped or may be stopped. Terminate the loop
     * as soon as one actor does not fulfill this test. Note, usually this loop is very
     * short. Terminated actors remove themselves from the list when they are done. */
    val mayTerminate = actors.forall(act)
    /* If we may terminate the actor system ... */
    if mayTerminate
    /* ... we must first terminate the haltable (but still active) actors */
    then haltables.foreach(_.stopWith(true))
    /* if not, we must probe all actors that requested it to see if they are silent (doing nothing). */
    else silent.foreach(_.dropNeedle(true))
    /* Finally we are really terminated if we were allowed to terminate and there were no haltables left. */
    mayTerminate && haltables.isEmpty


  /** Put an actor under guard. If requested add it to the index as well. */
  private[actors] def add(actor: Actor, rename: Auxiliary.Rename = Auxiliary.Rename.empty): Unit =
    /* We had some problems with null names here due to a design flaw regarding the
     * order of object construction. These should be gone now. Lets verify for a while. */
    assert(rename.name != null, "uninitialized name in ActorGuard|add")
    synchronized {
      /* Put it in the index if required. */
      if rename.inIndex then index += rename.name -> actor
      /* Always add it to the base collection of (running) actors. */
      actors += actor }

  /** Removes an actor from the list, and index. Call when the actor is terminated. */
  private[actors] def remove(actor: Actor): Unit = synchronized {
    /* Try to remove the name. In some situations this is called when it cannot be a member. This is okay. */
    index -= actor.name
    /* Remove the actor for needle dropping */
    silent -= actor
    /* Remove the actor from the primary collection. */
    actors -= actor }

  /**
   * Get the actor with this path/name if it exists. It will recurse into the family tree if required
   * All actors you gave a name manually are indexed. Here is the primary search point. If you are
   * already inside a family actor, it is more efficient to search just that tree. */
  def get(path: String)(using context: ActorContext): Option[Actor] = FamilyChild.searchFor(path,context.familyPathSeparator,index)

  /**
   * Start watching for actor system completion. This uses polling to see if all actors are
   * done. Do not set the pollInterval to low, for this calls all actors under guard. The minimum is 1
   * second. In practice this time defines the maximum time to wait for the application to terminate
   * after all the work is done. So for servers running days or more 10 seconds to 1 minute may be a good
   * value, for CLI apps, use 2 seconds. Use force if you want to terminate other processes as well when
   * the actors are all completed to shutdown. Note that if some actors have not stopped by themselves,
   * but are not able to receive any messages any more, the application may run indefinitely, and this
   * is platform dependent. In that case you may need to call context.showdown(true/false) somewhere
   * manually. Calling the watch method may be needed to start the actor system  depending on the
   * platform. */
  def watch(force: Boolean, pollInterval: FiniteDuration, complete: () => Unit = () => ())(using context: ActorContext): Unit =
    /* Make sure we wait at least one second. */
    val pollLimited = pollInterval max 1.second
    /* Now, wait for the system to complete by polling allTerminated. At completion call the complete handler. */
    context.waitForExit(force,pollLimited)(allTerminated,complete)
