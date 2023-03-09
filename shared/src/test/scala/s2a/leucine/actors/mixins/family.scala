package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.extensions.*
import s2a.leucine.actors.Actor.Anonymous

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


/* Homogeneous hierarchy */
trait ActorTreeSupply :
  implicit val ac: ActorContext = ActorContext.system

  class Tree(val name: String, val parent: Option[Tree], val writeln: String => Unit, val done: Option[() => Unit]) extends StandardActor[Tree.Letter,Actor.Any], FamilyTree[Tree], FamilyChildExtra :

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


/* Hetrogeneous hierarchy */
trait ActorFamilySupply :
  implicit val ac: ActorContext = ActorContext.system


  class Outside(val name: String) extends StandardActor[Outside.Letter,Outside.Accept]:

    def receive(letter: Outside.Letter, sender: Sender) = ???

  object Outside :
    sealed trait Letter extends Actor.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous


  object Level :
    sealed trait Letter extends Actor.Letter
    case class Common(msg: String) extends Letter

  class Level0(val name: String) extends StandardActor[Level0.Letter,Level0.Accept], FamilyRoot[Level.Letter,Outside] :
    adopt(Level1A("1a",this))
    //adopt(Level1B("1b",this))
    //adopt(Level1C("1c",this))

    def receive(letter: Level0.Letter, sender: Sender) = ???


  object Level0 :
    sealed trait Letter extends Level.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous | Outside


  class Level1A(val name: String, protected val parent: Level0) extends StandardActor[Level1A.Letter,Level1A.Accept], FamilyBranch[Level2.Letter,Level0,Null] :
    def receive(letter: Level1A.Letter, sender: Sender) = ???

  object Level1A :
    sealed trait Letter extends Level.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous | Outside


  class Level1B(val name: String, protected val parent: Level0) extends StandardActor[Level1B.Letter,Level1B.Accept], FamilyBranch[Level2.Letter,Level0,Null] :
    def receive(letter: Level1B.Letter, sender: Sender) = ???

  object Level1B :
    sealed trait Letter extends Level.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous | Outside


  class Level1C(val name: String, protected val parent: Level0) extends StandardActor[Level1C.Letter,Level1C.Accept], FamilyBranch[Level2.Letter,Level0,Null] :
    def receive(letter: Level1C.Letter, sender: Sender) = ???

  object Level1C :
    sealed trait Letter extends Level.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous | Outside



  class Level2(val name: String, protected val parent: Level1A) extends StandardActor[Level2.Letter,Level2.Accept], FamilyBranch[Level3.Letter,Level1A,Null] :
    def receive(letter: Level2.Letter, sender: Sender) = ???

  object Level2 :
    sealed trait Letter extends Actor.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous


  class Level3(val name: String, protected val parent: Level2) extends StandardActor[Level3.Letter,Level3.Accept], FamilyLeaf[Level2] :
    def receive(letter: Level3.Letter, sender: Sender) = ???

  object Level3 :
    sealed trait Letter extends Actor.Letter
    case class Text(msg: String) extends Letter
    type Accept = Actor.Anonymous



object TestMethods :
  def containsNoBackwardReferences(seq: List[String]): Boolean =  seq.zipWithIndex.forall( (s,i) => !seq.take(i).exists(_.startsWith(s)) )
  def containsOnlyUniqueElements(seq: Seq[?]): Boolean = seq.size == seq.toSet.size
  def hasThePyramidLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**(level+1)-1)/(width-1)
  def hasTheBlockLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**level) * level

  def forward(list: List[String])  = list.filter(_.contains("=>>"))
  def backward(list: List[String]) = list.filter(_.contains("<<="))
  def stop(list: List[String])     = list.filter(_.contains("stop")).reverse


object TreeActorTestFinish extends TestSuite, ActorTreeSupply :
  import TestMethods.*
  given Actor.Anonymous = Actor.Anonymous
  val buffer = Buffer[String]
  val width = 3
  val level = 4
  val deferred = Deferred(buffer.readlns)
  val tree: Tree = Tree("F0",None,buffer.writeln,Some(deferred.done))
  tree ! Tree.Create(width,level)
  tree ! Tree.Forward
  tree.stopFinish()
  deferred.await()

  val tests = Tests {
    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and
     * the backward (<==) does not start after a finish command (strict message sequence). Also tests if the
     * teardown is deterministic (ie. parents only stop after all there children stopped) */
    test("sending letters, finish directly afterwards"){
      /* Uncomment to see the result during tests */
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
  tree ! Tree.Forward
  deferred.await()

  val tests = Tests {
    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and the backward
     * (<<=) messages are correct in number, which also must be complete before stop. Also tests if the teardown is
     * deterministic (ie. parents only stop after all there children stopped) */
    test("sending letters, finish by counting"){
      /* Uncomment to see the result during tests */
      //test("Show result")                           - { deferred.compare(list => println(list)) }
      test("Forward ContainsNoBackwardReferences")  - { deferred.compare(list => containsNoBackwardReferences(forward(list)) ==> true) }
      test("Forward ContainsOnlyUniqueElements")    - { deferred.compare(list => containsOnlyUniqueElements(forward(list)) ==> true) }
      test("Forward hasThePyramidLength")           - { deferred.compare(list => hasThePyramidLength(forward(list),width,level) ==> true) }
      test("Backward hasTheBlockLength")            - { deferred.compare(list => hasTheBlockLength(backward(list),width,level) ==> true) }
      test("Stop ContainsNoBackwardReferences")     - { deferred.compare(list => containsNoBackwardReferences(stop(list)) ==> true) }
      test("Stop ContainsOnlyUniqueElements")       - { deferred.compare(list => containsOnlyUniqueElements(stop(list)) ==> true) }
      test("Stop hasThePyramidLength")              - { deferred.compare(list => hasThePyramidLength(stop(list),width,level) ==> true) } } }

