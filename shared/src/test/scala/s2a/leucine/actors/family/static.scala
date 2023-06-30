package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.annotation.nowarn
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.actors.Actor.Anonymous
import s2a.control.Helpers.*


/* Heterogeneous hierarchy */
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

  object Outside_ extends RestrictDefine, Stateless  :
    type Accept = Anonymous | Outside
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case class Text(msg: String) extends Letter[Accept]
    case object Bell extends Letter[Accept]

  object Level0_ extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Outside
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Test0 extends Letter[Accept]
    type ChildActor = BareActor
    type FamilyAccept = Accept & Level1A_.Accept & Level1B_.Accept & Level1C_.Accept
    type FamilyCommon = Nothing
    type FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] = Letter[Sender] & Level1A_.Letter[Sender] & Level1B_.Letter[Sender] & Level1C_.Letter[Sender]
    /* TODO: This is still problematic, but we must be able to define common letters. */
    case object Common extends Letter[Anonymous], Level1A_.Letter[Anonymous], Level1B_.Letter[Anonymous], Level1C_.Letter[Anonymous]

  object Level1A_ extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Outside | Level0
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Test1A extends Letter[Accept]
    type ChildActor = BareActor
    type FamilyAccept = Actor
    type FamilyLetter[Sender <: FamilyAccept] =  Level2A_.Letter[Sender]

  object Level1B_ extends RestrictDefine, Stateless  :
    type Accept = Anonymous | Outside
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Test1B extends Letter[Accept]

  object Level1C_ extends RestrictDefine, Stateless  :
    type Accept = Anonymous | Outside | Level1A
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case class Text(msg: String) extends Letter[Accept]

  object Level2A_ extends RestrictDefine, Stateless  :
    type Accept = Actor
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case class Text(msg: String) extends Letter[Accept]

  class Outside extends RestrictActor(Outside_,"boo"), TimingAid, Stateless  :
    post(Outside_.Bell,1.seconds)
    def receive[Sender <: Accept](letter: Outside_.Letter[Sender], sender: Sender) = (letter,sender) match
      case(Outside_.Text(msg), s: Anonymous) => ()
      case(Outside_.Text(msg), s: Outside) => ()
      case(Outside_.Bell,_) => ()
      /* Unfortunately the compiler does not understand the case below cannot occur */
      case(Outside_.Text(msg),_) => ()

  abstract class Level0 extends RestrictActor(Level0_,"l0"), FamilyRootRelay(Level0_) :
    def outside: Outside
    val level1A = Level1A(this)
    val level1B = Level1B(this)
    val level1C = Level1C(this)
    level1A.send(Level1A_.Test1A,Actor.Anonymous)
    level1A.send(Level1A_.Test1A,outside)
    level1A.send(Level1A_.Test1A,this)
    level1B.send(Level1B_.Test1B,Actor.Anonymous)
    level1B.send(Level1B_.Test1B,outside)
    level1C.send(Level1C_.Text("ba"),level1A)


    def receive[Sender <: Accept](letter: MyLetter[Sender], sender: Sender): Unit = (letter,sender) match
      case (Level0_.Test0, s: Anonymous) => ()
      case (Level0_.Test0, s: Outside)   => ()
      case (Level0_.Test0, s: Level0)    => ()
      case (Level0_.Common, _)           => ()
      /* Unfortunately the compiler does not understand the case below cannot occur */
      case(Level0_.Test0,_) => ()


  class Level1A(protected val parent: Level0) extends RestrictActor(Level1A_,"1a"), FamilyBranchRelayRelayed[Level0,Level1A_.type](Level1A_) :
    val level2A = Level2A(this)
    level2A.send(Level2A_.Text("hi"),this)
    level2A.send(Level2A_.Text("hi"),Actor.Anonymous)
    /* For the moment we just test if an error occurs, but we do not check the content for the error message is not stable. */
    compileError("level2A.send(Level1A_.Test1A,Actor.Anonymous)").msg.clean(5) ==> "Found"

    def receive[Sender <: Accept](letter: Level1A_.Letter[Sender], sender: Sender): Unit = ()

  class Level1B(protected val parent: Level0) extends RestrictActor(Level1B_,"1b"), FamilyLeafRelayed[Level0] :
    def receive[Sender <: Accept](letter: Level1B_.Letter[Sender], sender: Sender): Unit = ()

  class Level1C(protected val parent: Level0) extends RestrictActor(Level1C_,"1c"), FamilyLeafRelayed[Level0] :
    def receive[Sender <: Accept](letter: Level1C_.Letter[Sender], sender: Sender): Unit = ()

  class Level2A(protected val parent: Level1A) extends RestrictActor(Level2A_,"2a"), FamilyLeafRelayed[Level1A] :
    def receive[Sender <: Accept](letter: Level2A_.Letter[Sender], sender: Sender): Unit = ()


  val tests = Tests {

    class Level0Ext(val outside: Outside) extends Level0 :
      get("1a").isDefined    ==> true
      get("1a.2a").isDefined ==> true
      get("1b.2a").isDefined ==> false
      get("1a")    ==> Some(level1A)
      get("1b.2a") ==> None
      /* For the moment we just test if an error occurs, but we do not check the content for the error message is not stable. */
      compileError("level1C.send(Level1C_.Text(\"ba\"),level1B)").msg.clean(5) ==> "Found"
      compileError("level1B.send(Level1B_.Test1B,this)").msg.clean(5) ==> "Found"
      compileError("relayAll(Level1A_.Test1A,outside)").msg.clean(5) ==> "Found"
      compileError("relayAll(Level1B_.Test1B,outside)").msg.clean(5) ==> "Found"
      override def receive[Sender <: Accept](letter: MyLetter[Sender], sender: Sender): Unit = super.receive(letter,sender)
        /* Testing these hit a compiler bug. */
        //(letter,sender) match
        //  compileError("case (Level0_.Test0, s: Level0) => ()").msg.clean() ==> "Dit kan niet en dat klopt."
        //  compileError("case (Level1A_.Test1A, rs: FamilyAccept) => relay(cl,rs,_ => true)").msg.clean() ==> "Dit kan niet en dat klopt."
        //  compileError("case (Level1B_.Test1B, rs: FamilyAccept) => relay(cl,rs,_ => true) ").msg.clean() ==> "Dit kan niet en dat klopt."

    val outside = new Outside

    val level0 = Level0Ext(outside)

    level0.send(Level0_.Test0,Actor.Anonymous)
    level0.send(Level0_.Test0,outside)
 }
