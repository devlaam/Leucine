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

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*
import s2a.leucine.actors.Actor.Anonymous


/* Actor that recursively enters some structure to investigate. It is under monitor supervision.
 * The root of the actor structure has no parent, therefore the parent is optional in this case. */
class Tree(name: String, val parent: Option[Tree]) extends AcceptActor(Tree,name), FamilyTree[Tree], TimingAid, MonitorAid(monitor) :

  /* Write the results of this actor to the console. */
  private def write(kind: String) = println(s"$kind $path")

  /* Show when the actor stops. */
  final protected override def stopped(cause: Actor.Stop, complete: Boolean) =
    /* This is written for all actors. */
    write(s"stop:$cause")
    /* This is executed when the root actor stops, which is at the end. */
    if parent.isEmpty then monitor.show(Config(posts = true, traces = true))

  /* New children must be created and manually adopted by the parent. */
  private def newChild(i: Int) = Tree(s"F$i",Some(this))

  /* Variable to see if all child actors have reported back that their
   * job is done. */
  private var returns: Int = 0

  /* Start the work in this crawler, two wide and three deep. */
  if parent.isEmpty then
    this ! Tree.Create(2,3)
    this ! Tree.Forward
    /* Show the internals of the actor after some time */
    post(Tree.Report,6.seconds)
    /* Stop this actor tree when nothing is happening any more. */
    stop(Actor.Stop.Silent)


  final protected def receive(letter: Letter): Unit = letter match
    /**/
    case Tree.Create(width,level) =>
      /* Calculate how many returns we expect, when we close later on. */
      if parent.isEmpty then returns = (math.pow(width,level)+0.4).toInt
      /* Create 'width' number of new children. */
      (1 to width).foreach(newChild)
      /* In case we are not yet on the last level, relay this creation order
       * to the next level. */
      if (level > 1) then relayAll(Tree.Create(width,level - 1))
    case Tree.Forward =>
      /* Report that we are in the forward traversal. */
      write("=>>")
      /* Relay the message to all children, and see if we succeeded. */
      val relayed = relayAll(Tree.Forward)
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
        case None    => returns -= 1; if returns == 0 then println("Wait for silent termination (~12s)")
    case Tree.Report => monitor.show(Config(samples = true))


object Tree extends AcceptDefine, Stateless :
  sealed trait Letter extends Actor.Letter[Actor]
  /* Message to create the tree structure. The maximal number of levels
   * is given by depth, the number of actors created in each level given
   * by width. */
  case class  Create(width: Int, depth: Int) extends Letter
  /* Message to traverse the tree in forward direction. */
  case object Forward extends Letter
  /* Message to traverse the tree in backwards direction. */
  case object Backward extends Letter
  /* Message report the samples once. */
  case object Report extends Letter

