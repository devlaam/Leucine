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

import java.io.PrintWriter

import scala.collection.immutable.{Map, SortedSet}

import s2a.leucine.actors.*
import s2a.leucine.extensions.*
import s2a.leucine.actors.Actor.Anonymous

/* If you want to use the monitor, you must implement some callback methods. Here we keep
 * that simple and only report the after the application has completed. In bigger applications
 * you might need to export the situation from time to time and purge the monitor to prevent
 * data structures from getting to large. */
val monitor = new ActorMonitor {
  import MonitorActor.{Trace, Action, Tracing}
  import ActorMonitor.Record
  /* This callback is directly called in case an actor is added. Not used in this example. */
  def added(path: String, actors: Map[String,Record]): Unit = ()
  /* This callback is directly called in case an actor is removed. Not used in this example. */
  def removed(path: String, actors: Map[String,Record]): Unit = ()
  /* This callback is periodically called on the actor to update the actor matrics. Not used in this example. */
  def sampled(path: String, actors: Map[String,Record]): Unit = ()
  /* This callback is periodically called on the actor collect all tracing. Not used in this example. */
  def traced(path: String, minTime: Long, traces: SortedSet[Trace]): Unit = ()
  /* Method you can implement to show the results obtained sofar. Since this example only has one short
   * run the results are show an the end. */
  def show(path: String): Unit =
    val writer: PrintWriter = new PrintWriter(System.out)
    report(writer)
    writer.flush()
  /* Global setting of tracing. Here we enable is for all actors. */
  override def tracing = Tracing.Enabled }


/* Actor that recursively enters some structure to investigate. It is under monitor supervision.
 * The root of the actor structure has no parent, therefore the parent is optional in this case. */
class Tree(val name: String, val parent: Option[Tree]) extends StandardActor[Tree.Letter,Actor], FamilyTree[Tree], MonitorActor(monitor) :

  /* Write the results of this actor to the console. */
  private def write(kind: String) = println(s"$kind $path")

  /* Show when the actor stops. */
  override protected def stopped(complete: Boolean) =
    /* This is written for all actors. */
    write("stop")
    /* This is executed when the root actor stops, which is at the end. */
    if parent.isEmpty then monitor.show(path)

  /* New children must be created and manually adopted by the parent. */
  private def newChild(i: Int) = adopt(Tree(s"F$i",Some(this)))

  /* Variable to see if all child actors have reported back that their
   * job is done. */
  private var returns: Int = 0

  /* Start the work in this crawler, two wide and three deep. */
  if parent.isEmpty then
    this ! Tree.Create(2,3)
    this ! Tree.Forward


  def receive(letter: Tree.Letter, sender: Sender) = letter match
    /**/
    case Tree.Create(width,level) =>
      /* Calculate how many returns we expect, when we close lateron. */
      if parent.isEmpty then returns = (math.pow(width,level)+0.4).toInt
      /* Create 'width' number of new children. */
      (1 to width).foreach(newChild)
      /* In case we are not yet on the last level, relay this creation oder
       * to the next level. */
      if (level > 1) then relay(Tree.Create(width,level - 1),this)
    case Tree.Forward =>
      /* Report that we are in the forward traversal. */
      write("=>>")
      /* Relay the message to all children, and see if we succeeded. */
      val relayed = relay(Tree.Forward,this)
      /* In case there were no children to accept the message, we are at the
       * end of the structure and start the traversal backwards. */
      if relayed == 0 then parent.map(_ ! Tree.Backward)
    case Tree.Backward =>
      /* Report that we are in the backward traversal. */
      write("<<=")
      /* If we still have a parent, continue the backward traversal. If not,
       * we reached the root, we subtract one from the returns. When returns
       * hit zero, the traversal is complete and we may finish.*/
      parent match
        case Some(p) => p ! Tree.Backward
        case None    => returns -= 1; if returns == 0 then stopFinish()


object Tree :
  sealed trait Letter extends Actor.Letter
  /* Message to create the tree structure. The maximal number of levels
   * is given by depth, the number of actors created in each level given
   * by width. */
  case class  Create(width: Int, depth: Int) extends Letter
  /* Message to traverse the tree in forward direction. */
  case object Forward extends Letter
  /* Message to traverse the tree in backwards direction. */
  case object Backward extends Letter

