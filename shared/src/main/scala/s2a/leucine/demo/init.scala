package s2a.leucine.demo

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

import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*

/* Note: The examples are given to illustrate how the actors could be used, and are
 * not meant complete or even suited any particular situation. Just use these as a
 * starting point for you own application. */

/* The default actor context for these examples */
given actorContext: ActorContext = ActorContext.system

object DefaultActorLogger extends ActorLogger, DefaultLoggerSettings :
  import ActorLogger.{ShowGroups, GroupBase}

  object GroupA extends GroupBase
  object GroupB extends GroupBase
  object GroupC extends GroupBase

  transparent inline def showGroups = ShowGroups((GroupA))



// This version may cause a compiler error (when trace is included)
// class MyTestClass(x: Int) :
//   def this(x: Int, z: String) =
//     this(x)
//     DefaultActorLogger.trace(withParams = true)

// This version compiles:
class MyTestClass(x: Int, z: String = "") :
  DefaultActorLogger.trace(DefaultActorLogger.GroupA)

  val y: String = {  DefaultActorLogger.trace(DefaultActorLogger.GroupC); "TEST"}

object Init extends LogInfo:
  DefaultActorLogger.trace(DefaultActorLogger.GroupB)
  /* When you arrive here, you can be certain all actors are done */
  def complete(): Unit = println("Demo complete")

  val _ = new MyTestClass(1)

  @main
  def main(): Unit =
    DefaultActorLogger.trace(DefaultActorLogger.AllGroups)
    DefaultActorLogger.info("===> Main called")
    /* Welcome message */
    val _ = new MyTestClass(2,"JA")
    println(s"Started Actor examples on the ${actorContext.platform} platform.")
    /* Define a handler for unhandled messages */
    ActorGuard.failed(post => println(s"FAILED MESSAGE: ${post.full}"))
    /* Open the console as an actor to get commands from the user. */
    new Console
    /* Watch the actors for completion. Note that this blocks for the JVM and Native platforms
     * but not on JS. There blocking is not possible and this call returns immediately. Normally
     * you do not call watch on JS, for this does have not much added value. Except if you use
     * stop(Silent) somewhere. */
    ActorGuard.watch(false,3.seconds,complete)
    /* The application should exit when all work is done. */
