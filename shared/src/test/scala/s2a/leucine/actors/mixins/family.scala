package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.annotation.nowarn
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

extension (value: String)
  def clean(n: Int = 200) = value.replaceAll("\\s","").replaceAll("s2a.leucine.actors.","").take(n)


/* Homogeneous hierarchy */
trait ActorTreeSupply :
  implicit val ac: ActorContext = ActorContext.system

  class Tree(val name: String, val parent: Option[Tree], val writeln: String => Unit, val done: Option[() => Unit]) extends StandardActor[Tree.Letter,Actor], FamilyTree[Tree] :

    private def write(kind: String) = writeln(s"$kind$path")

    override protected def stopped(complete: Boolean) =
      write("stop")
      done.foreach(_())

    override protected def abandoned(child: String) =
      write(s"<<=$child#")

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
      case Tree.Stop =>
        write("=>>")
        if parent.isEmpty then stopBarren(true)
        val relayed = relay(Tree.Stop,this)
        if relayed == 0 then stopDirect()

  object Tree :
    sealed trait Letter extends Actor.Letter
    case class  Create(width: Int, level: Int) extends Letter
    case object Forward extends Letter
    case object Backward extends Letter
    case object Stop extends Letter


/* Hetrogeneous hierarchy */
object ActorFamilySupply extends TestSuite :
  import TestMethods.*
  implicit val ac: ActorContext = ActorContext.system

  /* Due to a bug in uTest we must define all nested objects out of the Tests macro.
   * Not the most beautiful design.
   * This generates an macro expansion error??
   *   val tests = Tests {
   *     object X :
   *       object Y }
   */

  object Outside_ :
    sealed trait Letter extends Actor.Letter
    case class Text(msg: String) extends Letter
    case object Bell extends Letter
    //type Accept = Actor.Anonymous | Outside

  object Level_ :
    sealed trait Letter extends Actor.Letter
    case object Common extends Level0_.Letter, Level1A_.Letter, Level1B_.Letter, Level1C_.Letter

  object Level0_ :
    sealed trait Letter extends Actor.Letter
    case object Test0 extends Letter
    //type Accept = Actor.Anonymous | Outside

  object Level1A_ :
    sealed trait Letter extends Actor.Letter
    case object Test1A extends Letter
    //type Accept = Actor.Anonymous | Outside | Level0

  object Level1B_ :
    sealed trait Letter extends Actor.Letter
    case object Test1B extends Letter
    //type Accept = Actor.Anonymous | Outside

  object Level1C_ :
    sealed trait Letter extends Actor.Letter
    case class Text(msg: String) extends Letter
    //type Accept = Actor.Anonymous | Outside | Level1A

  object Level2A_ :
    sealed trait Letter extends Actor.Letter
    case class Text(msg: String) extends Letter

  val tests = Tests {

    type Outside_Accept = Actor.Anonymous | Outside
    type Level0_Accept = Actor.Anonymous | Outside
    type Level0_ChildrenLetters = Level1A_.Letter & Level1B_.Letter & Level1C_.Letter
    type Level0_ChildrenAccept  = Level1A_Accept & Level1B_Accept & Level1C_Accept
    type Level1A_Accept = Actor.Anonymous | Outside | Level0
    type Level1B_Accept = Actor.Anonymous | Outside
    type Level1C_Accept = Actor.Anonymous | Outside | Level1A

    class Outside(val name: String) extends StandardActor[Outside_.Letter,Outside_Accept], TimingActor :
      post(Outside_.Bell,1.seconds)

      @nowarn /* This method does not generate a warning outside of the tests macro expansion */
      def receive(letter: Outside_.Letter, sender: Sender) = (letter,sender) match
        case(Outside_.Text(msg), s: Anonymous) => ()
        case(Outside_.Text(msg), s: Outside) => ()
        case(Outside_.Bell,_) => ()

    val outside = new Outside("boo")


    class Level0(val name: String) extends StandardActor[Level0_.Letter,Level0_Accept], FamilyRoot[Level0_ChildrenLetters, Level0_ChildrenAccept] :
      val level1A = Level1A("1a",this)
      val level1B = Level1B("1b",this)
      val level1C = Level1C("1c",this)
      adopt(level1A,level1B,level1C)
      level1A.send(Level1A_.Test1A,Actor.Anonymous)
      level1A.send(Level1A_.Test1A,outside)
      level1A.send(Level1A_.Test1A,this)
      level1B.send(Level1B_.Test1B,Actor.Anonymous)
      level1B.send(Level1B_.Test1B,outside)
      level1C.send(Level1C_.Text("ba"),level1A)
      compileError("level1C.send(Level1C_.Text(\"ba\"),level1B)").msg.clean() ==> "Found:(Level0.this.level1B:Level1B)Required:Level0.this.level1C.Sender"
      compileError("level1B.send(Level1B_.Test1B,this)").msg.clean() ==> "Found:(Level0.this:Level0)Required:Level0.this.level1B.Sender"

      relay(Level_.Common,outside,_ => true)
      compileError("relay(Level_.Common,this,_ => true)").msg.clean(75) ==> "Found:(ActorFamilySupply.Level_.Common.type,Level0,Any)Required:FamilyChild"
      compileError("relay(Level1A_.Test1A,outside,_ => true)").msg.clean(78) ==> "Found:(ActorFamilySupply.Level1A_.Test1A.type,Outside,Any)Required:FamilyChild"
      compileError("relay(Level1B_.Test1B,outside,_ => true)").msg.clean(78) ==> "Found:(ActorFamilySupply.Level1B_.Test1B.type,Outside,Any)Required:FamilyChild"

      get("1a").isDefined    ==> true
      get("1a.2a").isDefined ==> true
      get("1b.2a").isDefined ==> false

      get("1a")    ==> Some(level1A)
      get("1b.2a") ==> None

      @nowarn /* This method does not generate match warnings outside of the tests macro expansion */
      def receive(letter: MyLetter, sender: Sender) = (letter,sender) match
        case (Level0_.Test0, s: Actor.Anonymous) => ()
        case (Level0_.Test0, s: Outside) => ()
        case (Level_.Common, rs: ChildSender) => relay(Level_.Common,rs,_ => true)
        case (Level0_.Test0, s: Level0) => ()
        case (Level_.Common, s: Level0) => ()
        /* Testing these hit a compiler bug. */
        //compileError("case (Level0_.Test0, s: Level0) => ()").msg.clean() ==> "Dit kan niet en dat klopt."
        //compileError("case (Level_.Common, s: Level0) => ()").msg.clean() ==> "Dit kan niet en dat klopt."
        //compileError("case (Level1A_.Test1A, rs: ChildSender) => relay(cl,rs,_ => true)").msg.clean() ==> "Dit kan niet en dat klopt."
        //compileError("case (Level1B_.Test1B, rs: ChildSender) => relay(cl,rs,_ => true) ").msg.clean() ==> "Dit kan niet en dat klopt."


    class Level1A(val name: String, protected val parent: Level0) extends StandardActor[Level1A_.Letter,Level1A_Accept], FamilyBranch[Level2A_.Letter,Actor,Level0] :
      val level2A = Level2A("2a",this)
      adopt(level2A)
      level2A.send(Level2A_.Text("hi"),this)
      level2A.send(Level2A_.Text("hi"),Actor.Anonymous)
      compileError("level2A.send(Level1A_.Test1A,Actor.Anonymous)").msg.clean() ==> "Found:ActorFamilySupply.Level1A_.Test1A.typeRequired:Level1A.this.level2A.MyLetter"
      compileError("level2A.send(Level_.Common,Actor.Anonymous)").msg.clean() ==> "Found:ActorFamilySupply.Level_.Common.typeRequired:Level1A.this.level2A.MyLetter"

      def receive(letter: Level1A_.Letter, sender: Sender) = ()

    class Level1B(val name: String, protected val parent: Level0) extends StandardActor[Level1B_.Letter,Level1B_Accept], FamilyLeaf[Level0] :
      def receive(letter: Level1B_.Letter, sender: Sender) = ()

    class Level1C(val name: String, protected val parent: Level0) extends StandardActor[Level1C_.Letter,Level1C_Accept], FamilyLeaf[Level0] :
      def receive(letter: Level1C_.Letter, sender: Sender) = ()

    class Level2A(val name: String, protected val parent: Level1A) extends StandardActor[Level2A_.Letter,Actor], FamilyLeaf[Level1A] :
      def receive(letter: Level2A_.Letter, sender: Sender) = ()

    val level0 = Level0("l0")
    level0.send(Level0_.Test0,Actor.Anonymous)
    level0.send(Level0_.Test0,outside)
    level0.send(Level_.Common,outside)

}


object TestMethods :
  def containsNoBackwardReferences(seq: List[String]): Boolean =  seq.zipWithIndex.forall( (s,i) => !seq.take(i).exists(_.startsWith(s)) )
  def containsOnlyUniqueElements(seq: Seq[?]): Boolean = seq.size == seq.toSet.size
  def hasThePyramidLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**(level+1)-1)/(width-1)
  def hasTheBlockLength(seq: Seq[?], width: Int, level: Int): Boolean = seq.size == (width**level) * level

  def forward(list: List[String])  = list.filter(_.contains("=>>"))
  def backward(list: List[String]) = list.filter(_.contains("<<="))
  def stop(list: List[String])     = list.filter(_.contains("stop")).reverse

  def forwElm(i: Int): String = s"=>>F0.F$i"
  def stopElm(i: Int): String = s"stopF0.F$i"
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
  tree ! Tree.Forward
  tree.stopFinish()
  deferred.await()

  val tests = Tests {

    /* This tests if the forward (=>>) recursive buildup of children completes, has the correct buildup and
     * the backward (<<=) does not start after a finish command (strict message sequence). Also tests if the
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
      /* Uncomment to see the result during tests */
      //test("Show result")                          - { deferred.compare(list => println(list)) }
      test("Start with parent")                    - { deferred.compare(list => list.take(1) ==> List("=>>F0")) }
      test("Follows with new children")            - { deferred.compare(list => forward(list).drop(1).distinct.size ==> width)  }
      test("Follows (or mix) with stop children")  - { deferred.compare(list => stop(list).distinct.size ==> width+1)  }
      test("Follows with abandon children")        - { deferred.compare(list => backward(list.drop(11)).distinct.size ==> width)  }
      test("All start before  stop children")      - { deferred.compare(list => (1 to width).forall(forwBeforeStop(list)) ==> true)  }
      test("All stop before backward children")    - { deferred.compare(list => (1 to width).forall(stopBeforeBack(list)) ==> true)  }
      test("Follows with parent stop")             - { deferred.compare(list => list.drop(1 + 3*width) ==> List("stopF0"))  }
  } }


