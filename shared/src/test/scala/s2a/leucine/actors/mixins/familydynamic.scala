package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.annotation.nowarn
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.actors.Actor.Anonymous
import s2a.control.Helpers.*


/* Homogeneous hierarchy */
trait ActorTreeSupply :
  implicit val ac: ActorContext = ActorContext.system

  class Tree(name: String, val parent: Option[Tree], val writeln: String => Unit, val done: Option[() => Unit]) extends StandardActor(Tree,name), FamilyTree[Tree] :

    private def write(kind: String) = writeln(s"$kind$path")

    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      write(s"stop:$cause")
      done.foreach(_())

    override protected def abandoned(child: String) =
      write(s"<<=$child#")

    def newChild(i: Int) = Tree(s"F$i",Some(this),writeln,None)

    private var returns: Int = 0
    def receive[T <: Sender](letter: Tree.Letter[T], sender: T) = letter match
      case Tree.Create(width,level) =>
        if parent.isEmpty then returns = -(width**level)
        (1 to width).foreach(newChild)
        if (level > 1) then relay(Tree.Create(width,level - 1),this)
      case Tree.Forward(bounce) =>
        write("=>>")
        val relayed = relay(Tree.Forward(bounce),this)
        if bounce && (relayed == 0) then parent.map(_ ! Tree.Backward)
      case Tree.Backward =>
        write("<<=")
        parent match
          case Some(p) => p ! Tree.Backward
          case None    => returns += 1; if returns == 0 then stop(Actor.Stop.Finish)
      case Tree.Stop =>
        write("=>>")
        if parent.isEmpty then stop(Actor.Stop.Barren)
        val relayed = relay(Tree.Stop,this)
        if relayed == 0 then stop(Actor.Stop.Direct)

  object Tree extends StandardDefine :
    type Accept = Actor
    sealed trait Letter[T <: Accept] extends Actor.Letter[T]
    case class  Create(width: Int, level: Int) extends Letter[Actor]
    case class  Forward(bounce: Boolean) extends Letter[Actor]
    case object Backward extends Letter[Actor]
    case object Stop extends Letter[Actor]


object TestMethods :
  def containsNoBackwardReferences(seq: List[String]): Boolean =  seq.zipWithIndex.forall( (s,i) => !seq.take(i).exists(_.startsWith(s)) )
  def containsOnlyUniqueElements(seq: Seq[?]): Boolean = seq.size == seq.toSet.size
  def hasThePyramidLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**(level+1)-1)/(width-1)
  def hasTheBlockLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**level) * level

  def forward(list: List[String])  = list.filter(_.contains("=>>"))
  def backward(list: List[String]) = list.filter(_.contains("<<="))
  def stop(list: List[String])     = list.filter(_.contains("stop")).reverse

  def forwElm(i: Int): String = s"=>>F0.F$i"
  def stopElm(i: Int): String = s"stop:DirectF0.F$i"
  def backElm(i: Int): String = s"<<=F$i#F0"

  def forwBeforeStop(list: List[String])(i: Int) = list.indexOf(forwElm(i)) < list.indexOf(stopElm(i))
  def stopBeforeBack(list: List[String])(i: Int) = list.indexOf(stopElm(i)) < list.indexOf(backElm(i))


object TreeActorTestFinish extends TestSuite, ActorTreeSupply :
  import TestMethods.*
  given Actor.Anonymous = Actor.Anonymous
  val buffer = Buffer[String]
  val width = 3
  val level = 4
  val deferred = Deferred(buffer.readlns)
  val tree: Tree = Tree("F0",None,buffer.writeln,Some(deferred.done))
  tree ! Tree.Create(width,level)
  tree ! Tree.Forward(true)
  tree.stop(Actor.Stop.Finish)
  deferred.await()

  val tests = Tests {

    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and
     * the backward (<<=) does not start after a finish command (strict message sequence). Also tests if the
     * tear down is deterministic (i.e. parents only stop after all there children stopped) */
    test("sending letters, finish directly afterwards"){
      /* Remove comment to see the result during tests */
      //test("Show result")                          - { deferred.compare(list => println(list)) }
      test("Forward ContainsNoBackwardReferences") - { deferred.compare(list => containsNoBackwardReferences(forward(list)) ==> true)  }
      test("Forward ContainsOnlyUniqueElements")   - { deferred.compare(list => containsOnlyUniqueElements(forward(list)) ==> true) }
      test("Forward hasThePyramidLength")          - { deferred.compare(list => hasThePyramidLength(forward(list),width,level) ==> true) }
      test("Stop ContainsNoBackwardReferences")    - { deferred.compare(list => containsNoBackwardReferences(stop(list)) ==> true) }
      test("Stop ContainsOnlyUniqueElements")      - { deferred.compare(list => containsOnlyUniqueElements(stop(list)) ==> true) }
      test("Stop hasThePyramidLength")             - { deferred.compare(list => hasThePyramidLength(stop(list),width,level) ==> true) } } }


object TreeActorTestFree extends TestSuite, ActorTreeSupply :
  import TestMethods.*
  given Actor.Anonymous = Actor.Anonymous
  val buffer = Buffer[String]
  val width = 3
  val level = 4
  val deferred = Deferred(buffer.readlns)
  val tree: Tree = Tree("F0",None,buffer.writeln,Some(deferred.done))
  tree ! Tree.Create(width,level)
  tree ! Tree.Forward(true)
  deferred.await()

  val tests = Tests {
    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and the backward
     * (<<=) messages are correct in number, which also must be complete before stop. Also tests if the tear down is
     * deterministic (i.e. parents only stop after all there children stopped) */
    test("sending letters, finish by counting"){
      /* Remove comment to see the result during tests */
      //test("Show result")                           - { deferred.compare(list => println(list)) }
      test("Forward ContainsNoBackwardReferences")  - { deferred.compare(list => containsNoBackwardReferences(forward(list)) ==> true) }
      test("Forward ContainsOnlyUniqueElements")    - { deferred.compare(list => containsOnlyUniqueElements(forward(list)) ==> true) }
      test("Forward hasThePyramidLength")           - { deferred.compare(list => hasThePyramidLength(forward(list),width,level) ==> true) }
      test("Backward hasTheBlockLength")            - { deferred.compare(list => hasTheBlockLength(backward(list),width,level) ==> true) }
      test("Stop ContainsNoBackwardReferences")     - { deferred.compare(list => containsNoBackwardReferences(stop(list)) ==> true) }
      test("Stop ContainsOnlyUniqueElements")       - { deferred.compare(list => containsOnlyUniqueElements(stop(list)) ==> true) }
      test("Stop hasThePyramidLength")              - { deferred.compare(list => hasThePyramidLength(stop(list),width,level) ==> true) } } }


object TreeActorChildStops extends TestSuite, ActorTreeSupply :

  import TestMethods.*
  given Actor.Anonymous = Actor.Anonymous
  val buffer = Buffer[String]
  val width = 5
  val level = 1 // fixed value
  val deferred = Deferred(buffer.readlns)
  val tree: Tree = Tree("F0",None,buffer.writeln,Some(deferred.done))
  tree ! Tree.Create(width,level)
  tree ! Tree.Stop
  deferred.await()

  val tests = Tests {

    /* This tests if the forward (=>>) contains width+1 elements and stops with the childeren. The parent stop may come
     * only after all abandon calls have arrived. */
    test("sending letters, finish directly afterwards"){
      /* Remove comment to see the result during tests */
      //test("Show result")                          - { deferred.compare(list => println(list)) }
      test("Start with parent")                    - { deferred.compare(list => list.take(1) ==> List("=>>F0")) }
      test("Follows with new children")            - { deferred.compare(list => forward(list).drop(1).distinct.size ==> width)  }
      test("Follows (or mix) with stop children")  - { deferred.compare(list => stop(list).distinct.size ==> width+1)  }
      test("Follows with abandon children")        - { deferred.compare(list => backward(list).distinct.size ==> width)  }
      test("All start before stop children")       - { deferred.compare(list => (1 to width).forall(forwBeforeStop(list)) ==> true)  }
      test("All stop before backward children")    - { deferred.compare(list => (1 to width).forall(stopBeforeBack(list)) ==> true)  }
      test("Follows with parent stop")             - { deferred.compare(list => list.drop(1 + 3*width) ==> List("stop:BarrenF0"))  }
  } }
