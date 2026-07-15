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

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit, Callable, ScheduledFuture, RejectedExecutionException}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, FiniteDuration}


/** Context implementation for the JVM */
abstract class ContextImplementation extends PlatformContext :
  import Auxiliary.toUnit

  /** Contains the threadPool we will be utilizing */
  private lazy val threadPool = ContextImplementation.threadPool(threadsPerCore)

  /** Contains the execution context on which all actor tasks will be executed. */
  private lazy val executionContext = ExecutionContext.fromExecutorService(threadPool)

  /**
   * Contains the scheduler that we use for delayed tasks. They should do no or little work.
   * If so, they all fit on one thread, since the there are no promises on accurate timing.  */
  private lazy val scheduledExecutor = Executors.newSingleThreadScheduledExecutor(ContextImplementation.threadFactoryDefault)

  /**
   * Contains ExecuterService on which tasks can be run that require strict sequential handling to be done
   * eventually. For example, the logger transport. Not suited for actors or actor workers. */
  private lazy val queuedExecutor = Executors.newSingleThreadExecutor(ContextImplementation.threadFactoryDefault)

  /** True as long as there has been no Shutdown request and all thread are running. */
  def active: Boolean = !executionContext.isShutdown() && !scheduledExecutor.isShutdown() && !queuedExecutor.isShutdown()

  /** Indicates if the context runs on system threads or in an emulated environment. */
  def emulated: Boolean = false

  /** True if all treads have completed */
  def terminated: Boolean = executionContext.isTerminated() && scheduledExecutor.isTerminated() && queuedExecutor.isTerminated()

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then
    /* Try to execute the task. Note that even though we test for active,
     * this still may race against a shutdown request in between. */
    try executionContext.execute(runnable)
    /* This the task maybe rejected anyway. In that case, there is no need for action. */
    catch case _: RejectedExecutionException => ()

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    /* We only accept new schedules tasks when the system is still running all executors. */
    if active then
      /* Try to schedule the task. Note that even though we test for active,
       * this still may race against a shutdown request in between. */
      try
        val scheduledFuture: ScheduledFuture[Unit] = scheduledExecutor.schedule[Unit](callable,delay.toMillis,TimeUnit.MILLISECONDS)
        /* Construct the Cancellable object with cancel method. */
        new Cancellable { def cancel() = scheduledFuture.cancel(false).toUnit  }
      /* This the task maybe rejected anyway. In that case, there is no task started, so nothing to cancel. */
      catch case _: RejectedExecutionException => Cancellable.empty
    /* If not active, there is no task started, so nothing to cancel. */
    else Cancellable.empty

  /** Enqueue a new task on the special single threaded executor. */
  def enqueue(runnable: Runnable): Unit =
    /* We still accept tasks here even if we are not active any more. The last completing tasks may
     * have some final work to do (logging!). Of course, the thread itself must still be active. */
    if !queuedExecutor.isShutdown() then
    /* Try to execute the task. Note that even though we test for isShutdown,
     * this still may race against a shutdown request in between. */
    try queuedExecutor.execute(runnable)
    /* This the task maybe rejected anyway. In that case, there is no need for action. */
    catch case _: RejectedExecutionException => ()


  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
   * digestible process. As longs as there is no result yet, the attempt should produce None.
   * The attempt as well as the digest from digestible are both executed on the scheduledExecutor
   * and should finish asap. Should not to any work. Typically use this to send a letter.  */
  def await[M](digestible: Digestible[M], attempt: => Option[M]): Cancellable =
    /* Proactively test if we should start the construction of the Awaitable at all. */
    if active then
      /* If so, try to make one and return it as Cancellable. We give active as mayAttempt parameter
       * here so no new attempts are scheduled in case of a shutdown. */
      try ContextImplementation.Awaitable(attempt.map(digestible.digest).isDefined,active,idleThreadPause,scheduledExecutor)
      /* This the task maybe rejected anyway. In that case, there is no task started, so nothing to cancel. */
      catch case _: RejectedExecutionException => Cancellable.empty
    /* Otherwise there is nothing to cancel. */
    else Cancellable.empty

  /**
   * Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
   * there current tasks. With force=true the current execution is interrupted. In any case, no new tasks
   * will be accepted. Note that, when using force, the tear down of the system will be unordered and some tasks
   * will not be completed. Use with care. */
  def shutdown(force: Boolean): Unit =
    if force
    then
      /* With force, the shutdown is aggressive. Just stop the all executors asap. */
      executionContext.shutdownNow().toUnit
      scheduledExecutor.shutdownNow().toUnit
      queuedExecutor.shutdownNow().toUnit
    else
      /* Without force, we first shutdown the executionContext, where the work is done and tasks
       * for the other threads are originated. The other tasks will be shutdown when the work
       * in the executionContext is completed. */
      executionContext.shutdown()

  /** This method makes the thread loop ready for reuse after termination. Not required for this platform. */
  private[s2a] def revive(): Unit =  ()

  /**
   * This method enters an endless loop until the application finishes. Every timeout, it will probe a shutdown request.
   * There may be other reasons for shutdown as well. After all thread have completed (by force or not) the method
   * returns. Call in the main thread as last action there. This method blocks until finished, after which complete()
   * is called. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean, complete: () => Unit): Unit =
    /* As long as no all all tasks are completely terminated we must wait for termination. */
    while !terminated do
      /* We first wait for the threadPool to finish. There the main work is done and every task on the other
       * executors are generated there. Normally, if the treadPool has terminated, the other threads should
       * empty itself. So it does not make sense to wait for their termination if the threadPool is still
       * busy, times out and thus its awaitTermination returns false. */
      if threadPool.awaitTermination(time.toSeconds, TimeUnit.SECONDS)
      then
        /* As soon as the threadPool has terminated, we want that the other threads stop as well, since
         * we do not expect that new work is generated any more. All tasks from the thread pool should be
         * on these threads now, if any. Thus we can shutdown if needed. */
        if !force then scheduledExecutor.shutdown()
        if !force then queuedExecutor.shutdown()
        /* Any scheduled tasks (and thus awaitables) are not rescheduled when they fire since active is
         * already false. Now we wait for those tasks to complete. The question is how long should we wait?
         * In principle these threads should finish quickly. And what if they don't finish. Since they are
         * auxiliary threads, we should not hang on them. There we give them maximally the timeout each,
         * and then we forcefully terminate. */
        if !scheduledExecutor.awaitTermination(time.toSeconds, TimeUnit.SECONDS) then scheduledExecutor.shutdownNow().toUnit
        if !queuedExecutor.awaitTermination(time.toSeconds, TimeUnit.SECONDS)    then queuedExecutor.shutdownNow().toUnit
      else
        /* We arrive here if threadPool is still running. If there is a shutdownRequest we must call shutdown. */
        if shutdownRequest then shutdown(force)
    /* Once we arrive here we are complete. */
    complete()


object ContextImplementation :
  import Auxiliary.toUnit

  /** Returns the platform that is currently running, here the JVM. */
  def platform = PlatformContext.Platform.JVM


  /**
   * Class which continuously retries an attempt until it succeeds or is cancelled. The doAttempt
   * by name reference should return true if it succeeded and false otherwise. The delay between
   * each attempt should be in ms. The parameter mayAttempt should return true as long as we
   * are allowed to perform an attempt. The attempt itself should not block. */
  private class Awaitable(doAttempt: => Boolean, mayAttempt: => Boolean, delay: FiniteDuration, scheduler: ScheduledExecutorService) extends Cancellable :

    /* Allows prohibiting this awaitable to reschedule itself. Needs to
     * be volatile since the cancel request can come from anywhere. */
    @volatile private var continue = true

    /* Call to schedule a new task in the future. */
    private def schedule(c: Callable[Unit]): ScheduledFuture[Unit] = scheduler.schedule[Unit](c,delay.toMillis,TimeUnit.MILLISECONDS)

    /* Holding variable for the current active scheduled task. The RejectedExecutionException
     * that may occur here is catched in its factory method. This is also volatile to be sure
     * a call to cancel(), which may originate from an other thread, always see the latest
     * future present. */
    @volatile private var sf: ScheduledFuture[Unit] = schedule(callable)

    /* Construct a new callable with an embedded call. */
    private def callable: Callable[Unit] = new Callable[Unit] :
      /* This is called as soon as the scheduled timer has ended. It issues a new doAttempt
       * if this allowed by mayAttempt, which should be false if no work is allowed in the
       * main executor anymore. */
      def call() =
        /* See if we may continue. The order here is important due to short circuiting. If
         * continue was already false there is nothing to do. Otherwise see if we still are
         * allowed to do an attempt. If so do the actual attempt and see if we are done. */
        continue = continue && mayAttempt && !doAttempt
        /* If so (yes, there can be a race since the permission may be revoked in the
         * meantime, but that will at be cost one timeout at maximum or the task may
         * be directly rejected) then ... */
        if continue then
          /* try to schedule a new awaitable. */
          try sf = schedule(this)
          /* Which still might fail. If so, stop trying. */
          catch case _: RejectedExecutionException => continue = false

    /** Prohibit this awaitable to reschedule and try to cancel any already scheduled awaitable. */
    def cancel() = if continue then
      /* The method does nothing if continue was already false, if not, make it so */
      continue = false
      /* and prohibit scheduling a new task, but once started, do not interrupt. */
      sf.cancel(false).toUnit


  /** Number of processors on this platform . */
  val processorCount = Runtime.getRuntime().availableProcessors()

  /* Non-daemon threads keep the application from exiting. Daemon threads do not, but an application
   * exit does not terminate the threads by itself. (Note the JVM itself must be running, an JVM exit
   * terminates daemon threads nevertheless). Daemon threads run at lower priority.
   * Thus this means they may still run after the exit. This might be the intention, but usually
   * it is not and produce strange effects. In an orderly system you want all threads to have
   * finished before you relinquish control to the user. Or you must be sure to be the sole process
   * in the JVM. */

  /**
   * Returns the platform dependent default thread factory by calling Executors.defaultThreadFactory. Excerpt from that
   * documentation: This factory creates all new threads used by an Executor in the same ThreadGroup. Each new thread is
   * created as a non-daemon thread with priority set to the smaller of Thread.NORM_PRIORITY and the maximum priority
   * permitted in the thread group.  New threads have names accessible via Thread#getName of "pool-N-thread-M", where
   * N is the sequence number of this factory, M is the sequence number of the thread created by this factory. */
  private def threadFactoryDefault = Executors.defaultThreadFactory()

  /**
   * The number of threads you desire in an application should be so that all the cores are
   * kept busy (or if you have many, you can leave a few for other tasks). Blocking threads do
   * not keep the core busy, so add these to the requested number. Then round to the nearest multiple
   * of cores to keep it easy. This multiple can be specified, by the threadsPerCore. */
  private def threadPool(threadsPerCore: Int) =
    /* Calculate the maximum number of threads allocated */
    val threadsSize = threadsPerCore * processorCount
    /* Create the thread pool with max fixed thread size. */
    Executors.newFixedThreadPool(threadsSize,threadFactoryDefault)

  /** Sleep which returns to the caller */
  private[actors] def sleep(loop: => Unit, delay: FiniteDuration): Boolean =
    if delay != Duration.Zero then Thread.sleep(delay.toMillis)
    true
