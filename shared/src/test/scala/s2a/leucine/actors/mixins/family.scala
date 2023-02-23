package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.extensions.*
import s2a.leucine.actors.Actor.Anonymous

object FamilyActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  /* Recursive definition of integer power for non negative arguments and small exponents. */
  extension (value: Int)
    private def pow(x: Int, n: Int): Int = if n==1 then x else x * pow(x,n-1)
    def ** (exp: Int): Int =
      if       exp < 0      then throw Exception("Integer powers with negative exponent not supported.")
      else if  value < 0    then throw Exception("Integer powers with negative base not supported.")
      else if  value <= 1   then value
      else if  exp >= 32    then throw Exception("Integer number too large")
      else if  exp == 0     then 1
      else                  pow(value,exp)

  /* Fixed hierarchy */

  class Root(val name: String) extends StandardActor[Root.Letter], FamilyRoot[Branch.Letter] :

    def receive(letter: Root.Letter, sender: Sender) = ???



  object Root :
    sealed trait Letter extends Actor.Letter



  class Branch(val name: String, protected val parent: Root) extends StandardActor[Branch.Letter], FamilyBranch[Leaf.Letter,Root] :

    def receive(letter: Branch.Letter, sender: Sender) = ???


  object Branch :
    sealed trait Letter extends Actor.Letter



  class Leaf(val name: String, protected val parent: Branch)  extends StandardActor[Leaf.Letter], FamilyLeaf[Branch] :

    def receive(letter: Leaf.Letter, sender: Sender) = ???


  object Leaf :
    sealed trait Letter extends Actor.Letter



  /* Homogeneous hierarchy */

  class Tree(val name: String, val parent: Option[Tree], val writeln: String => Unit, val done: Option[() => Unit]) extends StandardActor[Tree.Letter], FamilyTree[Tree], FamilyChildExtra :

    private def write(kind: String) = writeln(s"$kind$path")

    override protected def stopped(complete: Boolean) =
      write("stop")
      done.foreach(_())

    def newChild(i: Int) = adopt(Tree(s"F$i",Some(this),writeln,None))

    private var returns: Int = 0
    def receive(letter: Tree.Letter, sender: Sender) = letter match
      case Tree.Create(width,level) =>
        if parent.isEmpty then returns = -(width**level)
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


  def containsNoBackwardReferences(seq: List[String]): Boolean =  seq.zipWithIndex.forall( (s,i) => !seq.take(i).exists(_.startsWith(s)) )
  def containsOnlyUniqueElements(seq: Seq[?]): Boolean = seq.size == seq.toSet.size
  def hasThePyramidLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**(level+1)-1)/(width-1)
  def hasTheBlockLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**level) * level

  def forward(list: List[String])  = list.filter(_.contains("=>>"))
  def backward(list: List[String]) = list.filter(_.contains("<<="))
  def stop(list: List[String])     = list.filter(_.contains("stop")).reverse

  val tests = Tests {
    val buffer = Buffer[String]

    val width = 3
    val level = 4

    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and
     * the backward (<==) does not start after a finish command (strict message sequence). Also tests if the
     * teardown is deterministic (ie. parents only stop after all there children stopped) */
    test("sending letters, finish directly afterwards"){
      val deferred = Deferred(buffer.readlns)
      val tree: Tree = Tree("F0",None,buffer.writeln,Some(deferred.done))
      tree ! Tree.Create(width,level)
      tree ! Tree.Forward
      tree.stopFinish()
      /* Uncomment to see the result during tests */
      //test("Show result")                          - { deferred.result.map(list => list) }
      test("Forward ContainsNoBackwardReferences") - { deferred.result.map(list => containsNoBackwardReferences(forward(list)) ==> true)  }
      test("Forward ContainsOnlyUniqueElements")   - { deferred.result.map(list => containsOnlyUniqueElements(forward(list)) ==> true) }
      test("Forward hasThePyramidLength")          - { deferred.result.map(list => hasThePyramidLength(forward(list),width,level) ==> true) }
      test("Backward NoElements")                  - { deferred.result.map(list => backward(list).isEmpty ==> true) }
      test("Stop ContainsNoBackwardReferences")    - { deferred.result.map(list => containsNoBackwardReferences(stop(list)) ==> true) }
      test("Stop ContainsOnlyUniqueElements")      - { deferred.result.map(list => containsOnlyUniqueElements(stop(list)) ==> true) }
      test("Stop hasThePyramidLength")             - { deferred.result.map(list => hasThePyramidLength(stop(list),width,level) ==> true) } }

    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and the backward
     * (<<=) messages are correct in number, which also must be complete before stop. Also tests if the teardown is
     * deterministic (ie. parents only stop after all there children stopped) */
    test("sending letters, finish by counting"){
      val deferred = Deferred(buffer.readlns)
      val tree: Tree = Tree("F0",None,buffer.writeln,Some(deferred.done))
      tree ! Tree.Create(width,level)
      tree ! Tree.Forward
      /* Uncomment to see the result during tests */
      //test("Show result")                          - { deferred.result.map(list => list) }
      test("Forward ContainsNoBackwardReferences")  - { deferred.result.map(list => containsNoBackwardReferences(forward(list)) ==> true) }
      test("Forward ContainsOnlyUniqueElements")    - { deferred.result.map(list => containsOnlyUniqueElements(forward(list)) ==> true) }
      test("Forward hasThePyramidLength")           - { deferred.result.map(list => hasThePyramidLength(forward(list),width,level) ==> true) }
      test("Backward hasTheBlockLength")            - { deferred.result.map(list => hasTheBlockLength(backward(list),width,level) ==> true) }
      test("Stop ContainsNoBackwardReferences")     - { deferred.result.map(list => containsNoBackwardReferences(stop(list)) ==> true) }
      test("Stop ContainsOnlyUniqueElements")       - { deferred.result.map(list => containsOnlyUniqueElements(stop(list)) ==> true) }
      test("Stop hasThePyramidLength")              - { deferred.result.map(list => hasThePyramidLength(stop(list),width,level) ==> true) } }

  }

