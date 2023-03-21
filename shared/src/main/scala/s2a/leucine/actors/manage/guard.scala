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
 * on them direcly after creation. For families only the root actor should to be added. Note
 * that if you create actors within other actors which are not a adopted by a family you must
 * add them as well, or call watch(force = true), if you want to properly close the application
 * at termination. */
object ActorGuard :

  /** Collection of all actors that are relevant for keeping the threadpool alive. */
  private var actors: Set[Actor] = Set.empty

  /* Index with with all named actors to find them on their name. Not all actors of
   * stored in the set actors are present here. But all of the indexed are also
   * present in the actors, which is the 'leading collection'. */
  private var index: Map[String,Actor] = Map.empty

  /**
   * See if all the actors that are running have completed. We do not put synchronized
   * here and rely in the atomicity of the reference variable actors and _.isActive calls.
   * Synchronizing would slow down the application and there is no problem with memory
   * inconsistency for reading the actors variable. Note that this opens the possibility that
   * the application terminates when an actor is added after the watch is called in an
   * other thread and all other actors have terminated as well. This however is an unlikely
   * scenario from a design perspective. The other threads are populated with actors, so
   * these have not terminated when they themselves created new actors.  */
  private def allTerminated = actors.forall(_.isTerminated)

  /** Put an actor under guard. If it has a prename, add it to the index as well. */
  private[actors] def add(prename: String, actor: Actor): Unit =
    /* We had some problems with null names here due to a design flaw regaring the
     * order of object construction. These should be gone now. Lets verify for a while. */
    assert(prename != null, "uninitialised name in ActorGuard|add")
    synchronized {
      /* We use prename here, to avoid confusion with the actors real name (to be determined) */
      if !prename.isEmpty then index += prename -> actor
      actors += actor }

  /** Removes an actor from the list, and index. Call when the actor is terminated. */
  private[actors] def remove(actor: Actor): Unit = synchronized {
    /* Try to remove the name. In some situations this is called when it cannot be a member. This is okay. */
    index -= actor.name
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
   * after all the work is done. Use force if you want to terminate other processes as well when
   * the actors are all completed to shutdown. Note that if some actors have not stopped by themselves,
   * but are not able to receive any messages any more, the application may run indefinitely, and this
   * is platform dependent. In that case you may need to call context.showdown(true/false) somewhere
   * manually. Calling the watch method may be needed to start the actor system  depening on the
   * platform. */
  def watch(force: Boolean, complete: () => Unit = () => (), pollInterval: FiniteDuration = 10.seconds)(using context: ActorContext): Unit =
    /* Make sure we wait at least one second. */
    context.waitForExit(force,pollInterval max 1.second)(allTerminated,complete)
