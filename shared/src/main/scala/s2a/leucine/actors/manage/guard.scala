package s2a.leucine.actors

import java.util.concurrent.Callable
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt


/** This object is meant to be run in the main thread and enables you to exit the application
  * when all actors have finished. Put all the actors you create under guard by calling add
  * on them direcly after creation. For families only the root actor needs to be added. Note
  * that if you create actors within other actors which are not a adopted by a family you must
  * add them as well, or call watch(force = true), if you want to properly close the application
  * at termination. */
object ActorGuard :

  /* Collection of all actors that are relevant for keeping the threadpool alive. */
  private var actors: Set[Actor[?,?]] = Set.empty

  /* See if all the actors that are running have completed. We do not put synchronized
   * here and rely in the atomicity of the reference variable actors and _.isActive calls.
   * Synchronizing would slow down the application and there is no problem with  memory in
   * consistency for reading the actors variable. Note that this opens the possibility that
   * the application terminates when an actor is added after the watch is called in an
   * other thread and all other actors have terminated as well. This however is an unlikely
   * scenario from a design perspective. The other threads are populated with actors, so
   * these have not terminated when they themselves created new actors.  */
  private def allDone = actors.forall(!_.isActive)

  /* This method is synchronized for it modifies the actors set. This is not a big problem
   * normally since it is mostly done at the start of the application. Synchronization is
   * needed if called concurrently from within an other actor threads.  */
  /** Put an actor under guard. */
  def add(actor: Actor[?,?]): Unit = synchronized { actors = actors + actor }

  /** Start watching for actor system completion. This uses polling to see if all actors are
   * done. Do not set the pollInterval to low, for calls all existing actors. The minimum is 1
   * second. In pratice this time defines the maximum time to wait for the application to terminate
   * after all the work is done. Use force if you want to terminate other processes as well when
   * the actors are all completed to shutdown. Note, that if some actors have not stopped by themselves,
   * but are not able to receive any messages any more, the application may run indefinitely, and this
   * is platform dependent. In that case you may need to call context.showdown(true/false) somewhere
   * manually. Calling the watch method may be needed to start the actor system  depening on the
   * platform. */
  def watch(force: Boolean, pollInterval: FiniteDuration = 10.seconds)(using context: ActorContext): Unit =
    /* Make sure we wait at least one second. */
    context.waitForExit(force,pollInterval min 1.second)(allDone)
