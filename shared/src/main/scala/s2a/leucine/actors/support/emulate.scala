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
import scala.collection.immutable.{Queue,SortedMap}


/* For applications where there is no access to the threading libraries, for
 * example virtual executors, you can use this Context. All tasks are kept in
 * collections, and thread libraries are not called. Runs on every platform.
 * The emulator is also protected against use in a multithreaded environment
 * where tasks are scheduled from different threads. */

/** Context implementation with manual single thread scheduling. */
abstract class ContextEmulation extends PlatformContext :

  /** Variable that determines the continuation of the main loop. */
  private var continue: Boolean = false

  /** Queue for all runnables that need to be executed directly */
  private var tasks: Queue[Runnable] = Queue.empty

  /** Map for all callables that need to be executed on a specific moment. */
  private var timers: SortedMap[Long,Callable[Unit]] = SortedMap.empty

  /** Set for all attempts that need to be tried every cycle. */
  private var attempts: Set[() => Option[Unit]] = Set.empty

  /** Variable that keeps track of the time this application has run since the start. In nano seconds. */
  private var passedTime: Long = 0

  /**
   * Test if there is an active timer that need to be handled. We only have timers available
   * when there are any and the first timer has expired. */
  private def hasFirstTimer: Boolean = synchronized { !timers.isEmpty && (timers.head._1 < passedTime) }

  /** Execute the first timer task that is due. */
  private def execFirstTimer(): Unit = synchronized {
    val (time,task) = timers.head
    timers = timers - time
    task }.call()

  /** See if there is a task to be executed (FIFO order). */
  private def hasFirstTask: Boolean = synchronized(!tasks.isEmpty)

  /** Execute the first timer task that is due. */
  private def execFirstTask(): Unit = synchronized {
    val (task,rest) = tasks.dequeue
    tasks = rest
    task }.run()

  /* See if there is any attempt that must be tested. */
  private def hasAttempts: Boolean = synchronized(!attempts.isEmpty)

  /** Try all attempts. All succeeded attempts are directly handled and removed. */
  private def tryAttempts(): Unit = synchronized { attempts = attempts.filter(_().isEmpty) }

  /** Main loop with a periodic hook */
  private def mainLoop(hook: () => Unit, complete: () => Unit, interval: Long): Unit =
    /* Stores the time the system first started (not necessary the real time) */
    val baseTime: Long = System.nanoTime()
    /* Stores the last time we called the hook */
    var lastHook: Long = 0L
    /* Test if enough time has past to make a new call to then hook */
    def mustCallHook = passedTime - lastHook > interval
    /* See if there is anything left to do */
    def hasWork: Boolean = !(tasks.isEmpty && timers.isEmpty && attempts.isEmpty)

    /* One pass of each activity inside the inner execution loop. Return true
     * if we have done any work. If not, we may take some sleep for example. */
    def pass(): Boolean =
      /* Every turn we measure how much time has passed. */
      passedTime = System.nanoTime() - baseTime
      /* The most important are the timers, so if there is any ..  */
      if hasFirstTimer then
        /* ... this will be handled first. */
        execFirstTimer()
        /* Work done, no need to sleep */
        true
      /* Subsequently we see if there is a task present ... */
      else if hasFirstTask  then
        /* ... if so we handle only one.  */
        execFirstTask()
        /* Work done, no need to sleep */
        true
      /* See if we must call the hook, which should be a rare event ... */
      else if mustCallHook then
        /* ... if so, register that we made the call ... */
        lastHook = passedTime
        /* ... and perform the action. */
        hook()
        /* Work done, no need to sleep */
        true
      else
        /* Iff there are no timers or tasks it makes sense to probe the asynchronous events.
         * This is because they usually result in more work (i/o). If there are attempts, we
         * try them all. Attempts are supposed to be handled quickly, and they do not have a
         *  natural ordering like timers of tasks. */
        if hasAttempts then tryAttempts()
        /* In this situation we must also pause after each round of attempts so that we do
         * not hammer the attempt methods, or consume too much time on this loop when there
         * are none. We have done no real work */
        false

    /* Inner execution loop */
    def loop(): Unit =
      while
        /* First we test if we must continue the loop. This is the case as long as
         * continue stays true (can be set to false from the outside) and we still have
         * work or _active remains true. If continue is set to false, we immediately stop,
         * (forced stop) and if _active is set to false, we complete the remaining work
         * first. */
        if synchronized { continue = continue && (_active || hasWork); continue }
        /* pass() executes some work and returns true, in that case there is no need to
         * sleep. Otherwise we take a nap. If sleep returns false (on JS) we exit the loop. */
        then (pass() || ContextImplementation.sleep(loop(),idleThreadPause))
        /* If we exit the loop due to discontinuation, say the last goodbyes. */
        else { complete(); false }
      do ()

    /* Now start the execution */
    loop()

  /** Wait loop which waits until the main loop is finished. */
  private def waitLoop(interval: FiniteDuration): Unit =
    /* We should often probe if the main loop has finished but not to often. */
    val pause = (interval / 10) max idleThreadPause
    /* Inner wait loop */
    def loop(): Unit =
      while synchronized(continue) do ContextImplementation.sleep(loop(),pause)
    /* Now start the wait */
    loop()


  /**
   * Test if this is the first time if we enter the loop. Simultaneously clears the first use flag.
   * Returns true if the mainLoop is already initiated (running), and false otherwise. */
  private def initiated: Boolean = synchronized {
    /* If continue is true, the mainLoop is running, return true ... */
    if continue then true else
      /* ... otherwise make continue true and ... */
      continue = true
      /* ... return false in order to start the mainLoop. */
      false }

  /**
   * Application starts active. Once this is set to false, the planned tasks and timers
   * are completed, no new ones will be accepted. */
  private var _active: Boolean = true

  /** True as long as there has been no Shutdown request. */
  def active: Boolean = _active

  /** Indicates if the context runs on system threads or in an emulated environment. */
  def emulated: Boolean = true

  /**
   * True if all treads have completed. In a single threaded system this is never the
   * case since the main has stopped if you could get true, but you can only read this
   * from main tread. */
  def terminated: Boolean = !continue

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if _active then synchronized { tasks = tasks.enqueue(runnable) }

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable = synchronized {
    if _active then
      var time = passedTime + delay.toNanos
      /* There may already be a time with this exact delay, so we plan it a few nano seconds later. */
      while timers.contains(time) do time = time + 1
      /* Add the new timer and action to the existing timers. */
      timers = timers + (time -> callable)
      /* Construct an object that enables the user to retract the action. */
      new Cancellable { def cancel() = timers = timers - time  }
    else Cancellable.empty }

  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
   * digestible process. As longs as there is no result yet, the attempt should produce None */
  def await[M](digestible: Digestible[M], attempt: => Option[M]): Cancellable =
    if _active then
      /* Create the actual attempt as a delayed digest. */
      val doAttempt = () => attempt.map(digestible.digest)
      /* Add the attempt to the set of attempts. */
      attempts = attempts + doAttempt
      /* Construct an object that enables the user to retract the attempt. */
      new Cancellable { def cancel() = attempts = attempts - doAttempt }
    else Cancellable.empty

  /**
   * Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
   * there current tasks. With force=true the current execution is interrupted. In any case, no new tasks
   * will be accepted. Once you have called shutdown, it is no longer possible to restart the main loop. */
  def shutdown(force: Boolean): Unit = synchronized {
    _active = false
    if force then continue = false }

  /** This method makes the thread loop ready for reuse after termination. Only for internal use when testing. */
  private[s2a] def revive(): Unit =  _active = true

  /**
   * This method enters an (endless) loop until the application finishes. Every timeout, it will probe a shutdown request.
   * There may be other reasons for shutdown as well. After all threads have completed (by force or not) the method
   * calls complete() and returns. Call in the main thread as last action there. Normally you should only call this once,
   * but in certain situations (tests) it may be needed to call it multiple times. Once a shutdown request is made, reuse
   * has become impossible. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean, complete: () => Unit): Unit =
    /* Method to periodically call to see if we may continue, and how. */
    def hook(): Unit = { if shutdownRequest then shutdown(force) }
    /* Start the main loop for execution. If we come here first, we may enter the mainLoop, if the main loop was
     * already initiated we must wait, and periodically test if the mainLoop is complete */
    if initiated then waitLoop(time) else mainLoop(hook,complete,time.toNanos)
