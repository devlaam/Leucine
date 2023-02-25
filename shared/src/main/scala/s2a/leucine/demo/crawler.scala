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

//object NoContent

val monitor = new ActorMonitor {
  import MonitorActor.{Trace, Action, Tracing}
  import ActorMonitor.Record
  def added(path: String, actors: Map[String,Record]): Unit = ()
  def removed(path: String, actors: Map[String,Record]): Unit = ()
  def sampled(path: String, actors: Map[String,Record]): Unit = ()
  def traced(path: String, minTime: Long, traces: SortedSet[Trace]): Unit = ()
  tracing = Tracing.Enabled
  def show(path: String): Unit =
    val writer: PrintWriter = new PrintWriter(System.out)
    report(writer)
    writer.flush()

}

class Tree(val name: String, val parent: Option[Tree]) extends StandardActor[Tree.Letter], FamilyTree[Tree], MonitorActor(monitor), FamilyChildExtra :

  private def write(kind: String) = println(s"$kind$path")

  override protected def stopped(complete: Boolean) =
    write("stop")
    if parent.isEmpty then monitor.show(path)

  private def newChild(i: Int) = adopt(Tree(s"F$i",Some(this)))

  private var returns: Int = 0

  def receive(letter: Tree.Letter, sender: Sender) = letter match
    case Tree.Create(width,level) =>
      if parent.isEmpty then returns = -((math.pow(width,level)+0.4).toInt)
      (1 to width).foreach(newChild)
      if (level > 1) then relay(Tree.Create(width,level - 1),this)
    case Tree.Forward =>
      write("=>>")
      val relayed = relay(Tree.Forward,this)
      if relayed == 0 then parent.map(_ ! Tree.Backward)
    case Tree.Backward =>
      write("<<=")
      parent match
        case Some(p) => p ! Tree.Backward
        case None    => returns += 1; if returns == 0 then stopFinish()


object Tree :
  sealed trait Letter extends Actor.Letter
  case class  Create(width: Int, level: Int) extends Letter
  case object Forward extends Letter
  case object Backward extends Letter

