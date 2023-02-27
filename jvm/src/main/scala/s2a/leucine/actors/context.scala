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


import java.util.concurrent.{Executors, ScheduledExecutorService, ThreadFactory, TimeUnit, Callable, ScheduledFuture}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration


/** Context implementation for the JVM */
abstract class ContextImplementation extends PlatformContext :

  /** Contains the threadPool we will be utilizing */
  private lazy val threadPool = ContextImplementation.threadPool(true,load)

  /** Contains the execution context on which all tasks will be executed. */
  private lazy val executionContext = ExecutionContext.fromExecutorService(threadPool)

  /** Contains the scheduler that we use for delayed tasks. They all fit on one thread. */
  private lazy val scheduler = Executors.newSingleThreadScheduledExecutor(ContextImplementation.threadFactoryDefault)

  /** Returns the platform that is currentluy running, here the JVM. */
  def platform = PlatformContext.Platform.JVM

  /** True as long as there has been no Shutdown request. */
  def active = !executionContext.isShutdown()

  /** True if all treads have completed */
  def terminated = executionContext.isTerminated()

  /** Execute a new task on the current Execution Context directly */
  def execute(runnable: Runnable): Unit = if active then executionContext.execute(runnable)

  /** Plan a new task on the current Execution Context, which is run after some delay. */
  def schedule(callable: Callable[Unit], delay: FiniteDuration): Cancellable =
    val scheduledFuture: ScheduledFuture[Unit] = scheduler.schedule[Unit](callable,delay.toMillis,TimeUnit.MILLISECONDS)
    new Cancellable { def cancel() = scheduledFuture.cancel(false)  }

  /**
   * Place a task on the Execution Context which is executed after some event arrives. When
   * it arrives it may produce an result of some type. This result is subsequently passed to the
   * digestable process. As longs as there is no result yet, the attempt should produce None */
  def await[M](digestable: Digestable[M], attempt: => Option[M]): Cancellable =
    new ContextImplementation.Awaitable(attempt.map(digestable.digest).isDefined,pause,scheduler)

  /**
   * Perform a shutdown request. With force=false, the shutdown will be effective if all threads have completed
   * there current thasks. With force=true the current execution is interrupted. In any case, no new tasks
   * will be accepted. */
  def shutdown(force: Boolean): Unit = if force then executionContext.shutdownNow() else executionContext.shutdown()

  /**
   * This method enters an endless loop until the application finishes. Every timeout, it will probe a shutdownrequest.
   * There may be other reasons for shutdown as well. After all thread have completed (by force or not) the method
   * returns. Call in the main thread as last action there. This method blocks until finished, after which complete()
   * is called. */
  def waitForExit(force: Boolean, time: FiniteDuration)(shutdownRequest: => Boolean, complete: () => Unit): Unit =
    while active || !terminated do
      if active && shutdownRequest then shutdown(force)
      threadPool.awaitTermination(time.toSeconds, TimeUnit.SECONDS)
    complete()


object ContextImplementation :

  /* Class which continously retries an attempt until it succeeds or is cancelled. The doAttempt
   * by name reference should return true if it succeedded and false otherwise. The delay between
   * each attempt should be in ms. The attempt itself should not block. */
  private class Awaitable(doAttempt: => Boolean, delay: FiniteDuration, scheduler: ScheduledExecutorService) extends Cancellable :
    private var continue = true
    private def schedule(c: Callable[Unit]): ScheduledFuture[Unit] = scheduler.schedule[Unit](c,delay.toMillis,TimeUnit.MILLISECONDS)
    private var sf: ScheduledFuture[Unit] = schedule(callable)
    private def callable: Callable[Unit] = new Callable[Unit] :
      def call() = if continue && !doAttempt then sf = schedule(this)
    def cancel() = { continue = false; sf.cancel(false) }

  /** Number of processors on this platform . */
  val processorCount = Runtime.getRuntime().availableProcessors()

  /** Returns the platform dependent default thread factory */
  def threadFactoryDefault = Executors.defaultThreadFactory()

  /* Non-daemon threads keep the application from exiting. Daemon threads do not, but an application
   * exit does not terminate the threads by itself. (Note the JVM itself must be running, an JVM exit
   * terminates daemon threads nevertheless). Daemon threads run at lower priority.
   * Thus this means they may still run after the exit. This might be the intention, but usually
   * it is not and produce strange effects. In an orderly system you want all threads to have
   * finished before you relinguish control to the user. Or you must be sure to be the sole process
   * in the JVM. */

  /** Custum  thread factory with the ability to determine if the thread is run as daemon. */
  def threadFactory(daemon: Boolean) = new ThreadFactory :
    override def newThread(runnable: Runnable) =
      val thread = new Thread(runnable)
      thread.setDaemon(daemon)
      thread

  /**
   * The number of threads you desire in an application should be so that all the cores are
   * kept busy (or if you have many, you can leave a few for other tasks). Blocking threads do
   * not keep the cory busy, so add these to the requested number. The round to the nearest multiple
   * of cores to keep it easy. This multiple can be specified, by the load usage 1 ... */
  def threadPool(daemon: Boolean, load: Int) = Executors.newFixedThreadPool(load*processorCount,threadFactoryDefault)


