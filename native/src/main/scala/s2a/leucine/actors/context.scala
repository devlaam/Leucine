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

/* Unfortunately the current native implementation does not have threadpools or timers. We only have a
 * global execution context and a Thread.Sleep. So we have to implement a loop ourselves for the moment.
 * One advantage: no need for synchronization! */

/** Context implementation for the Native Platform */
abstract class ContextImplementation  extends PlatformContext :

  /** Variable that determins the continuation of the main loop. Set to false to halt execution. */
  private var continue = true

  /** Queue for all runnables that need to be executed directly */
  private var tasks: Queue[Runnable] = Queue.empty

  /** Map for all callables that need to be executed on a specific moment. */
  private var timers: SortedMap[Long,Callable[Unit]] = SortedMap.empty

  /** Set for all attempts that need to be tried every cycle. */
  private var attempts: Set[() => Option[Unit]] = Set.empty

  /** Variable that keeps track of the time this application has run since the start. In nanosec */
  private var passedTime: Long = 0

  /**
   * Test if there is an active timer that need to be handled. We only have timers available
   * when there are any and the first timer has expired. */
  private def hasFirstTimer: Boolean = !timers.isEmpty && (timers.head._1 < passedTime)

  /** Execute the first timer task that is due. */
  private def execFirstTimer(): Unit =
    val (time,task) = timers.head
    timers = timers - time
    task.call()

  /** See if there is a task to be executed (FIFO order). */
  private def hasFirstTask: Boolean = !tasks.isEmpty

  /** Execute the first timer task that is due. */
  private def execFirstTask(): Unit =
    val (task,rest) = tasks.dequeue
    tasks = rest
    task.run()

  /* See if there is any attempt that must be tested. */
  private def hasAttempts: Boolean = !attempts.isEmpty

  /** Try all attempts. All succeeded attempts are direcly handled and removed. */
  private def tryAttempts(): Unit = attempts = attempts.filter(_().isEmpty)

  /** Main loop with a periodic hook */
  private def mainLoop(hook: () => Unit, interval: Long): Unit =
    /* Stores the time the system first started (not necessary the real time) */
    val baseTime: Long = System.nanoTime()
    /* Stores the last time we called the hook */
    var lastHook: Long = 0L
    /* Test if enough time has past to make a new call to then hook */
    def mustCallHook = passedTime - lastHook > interval

    /* While we are allowed to run the full program in this loop */
    while continue do
      /* Every turn we measure how much time has passed. */
      passedTime = System.nanoTime() - baseTime
      /* The most important are the timers, so id there is any, this will be handled first. */
      if      hasFirstTimer then execFirstTimer()
      /* Subsequently we see if there is a task present. If so we handle only one. */
      else if hasFirstTask  then execFirstTask()
      else
        /* Iff there are no timers or tasks it makes sense to probe the asynchrone events.
         * This is because they usually result in more work (i/o). If there are attempts, we
         * try them all. Attemps are supposed to be handled quicky, and they do not have a
         *  natural ordening like timers of tasks. */
        if hasAttempts then tryAttempts()
        /* In this situation we must also pause after each round of attempts so that we do
         * not hammer the attempt methods, or consume too much time on this loop when there
         * are none. */
        Thread.sleep(pause.toMillis)
      /* See if we must call the hook. */
      /* If there are no tasks, timers or attempts left, where is nothing to continue. */
      continue = continue && !(tasks.isEmpty && timers.isEmpty && attempts.isEmpty)



  /**
   * Application starts active. Once this is set to false, the planned tasks and timers
   * are completed, no new ones will be accepted. */
  private var _active = true

  /** Returns the platform that is currentluy running, here Native. */
  def platform = PlatformContext.Platform.Native

  /** True as long as there has been no Shutdown request. */
  def active =  _active

  /**
   * True if all treads have completed, for the current Native this is never the case
   * since the main has stopped if you could get true, but you can only read this from
   * main tread. */
  def terminated = !continue

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then tasks = tasks.enqueue(runnable)

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    var time = passedTime + delay.toNanos
    /* There may already be a time with this exact delay, so we plan it a few nanosecs later. */
    while timers.contains(time) do time = time + 1
    /* Add the new timer and action to the exisiting timers. */
    timers = timers + (time -> callable)
    /* Construct an object that enables the user to retract the actoin. */
    new Cancellable { def cancel() = timers = timers - time  }

  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
   * digestable process. As longs as there is no result yet, the attempt should produce None */
  def await[M](digestable: Digestable[M], attempt: => Option[M]): Cancellable =
    /* Create the actual attempt as a delayed digest. */
    val doAttempt = () => attempt.map(digestable.digest)
    /* Add the attempt to the set of attempts. */
    attempts = attempts + doAttempt
    /* Construct an object that enables the user to retract the attempt. */
    new Cancellable { def cancel() = attempts = attempts - doAttempt }

  /**
   * Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
   * there current thasks. With force=true the current execution is interrupted. In any case, no new tasks
   * will be accepted. */
  def shutdown(force: Boolean): Unit =
    _active = false
    if force then continue = false

  /**
   * This method enters an endless loop until the application finishes. Every timeout, it will probe a shutdownrequest.
   * There may be other reasons for shutdown as well. After all threads have completed (by force or not) the method
   * returns. Call in the main thread as last action there. In Native it this also starts the mainloop. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean): Unit =
    def hook(): Unit = { if shutdownRequest then shutdown(force) }
    mainLoop(hook,time.toNanos)

