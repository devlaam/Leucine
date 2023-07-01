package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.annotation.nowarn
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.actors.Actor.Anonymous
import s2a.control.Helpers.*


/* Actor Structure:
 *  Grandma (one instance)
 *  - Mama (3 instances)
 *    - Child (optional)
 *      Son <: Child  (3 instances)
 *      Daughter <: Child (3 instances)
 */

trait SiblingCommon :
  given ac: ActorContext = ActorContext.system

  def start(): Unit
  var writeln: String => Unit  = (s: String) => ()
  var done: Option[() => Unit] = None

  val result = Set("s3-SH-m1","s2-SH-m1","s1-SH-m2","s1-SH-m1","s2-SH-m3","s2-SH-m2","s3-SH-m2","d1-DH-m1","d2-DH-m1","d3-DH-m1","d3-DH-m2","d1-DH-m2",
                   "s3-SH-m3","s1-SH-m3","d2-DH-m2","s2-MF-m1","d2-MF-m1","d1-DH-m3","d1-MF-m1","s3-MF-m1","d3-DH-m3","s1-MF-m1","d2-DH-m3","d3-MF-m1",
                   "s1-MF-m2","d3-MF-m2","d1-MF-m2","d2-MF-m2","d1-MF-m3","d3-MF-m3","s3-MF-m2","s2-MF-m2","d2-MF-m3","s2-MF-m3","s3-MF-m3","s1-MF-m3")

  val tests = Tests {
    val buffer = Buffer[String]
    val deferred = Deferred(buffer.readlns)
    writeln = buffer.writeln
    done = Some(deferred.done)
    start()
    deferred.await()
    test("ContainsAllElements")  - { deferred.compare(list => list.toSet ==> result) } }


object SiblingRestrictSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends RestrictActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = Mama
    type FamilyAccept = Anonymous
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends FamilyLetter[Accept]

  class Mama(name: String, val parent: Grandma) extends RestrictActor(Mama,name), FamilyBranch[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = Son | Daughter
    type FamilyAccept = Mama
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends FamilyLetter[FamilyAccept]

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Sender], sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends RestrictActor(Son,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends RestrictDefine, Stateless  :
    type Accept = Son | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  class Daughter(name: String, val parent: Mama) extends RestrictActor(Daughter,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends RestrictDefine, Stateless  :
    type Accept = Daughter | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingRestrictRelaySupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends RestrictActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = Mama
    type FamilyAccept = Anonymous
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends FamilyLetter[Accept]

  class Mama(name: String, val parent: Grandma) extends RestrictActor(Mama,name), FamilyBranchRelay[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        relayAll(Mama.FamilyHello,this)

  object Mama extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = BareActor
    type FamilyAccept = Mama
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends FamilyLetter[FamilyAccept]

  class Son(name: String, val parent: Mama) extends RestrictActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends RestrictDefine, Stateless  :
    type Accept = Son | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  class Daughter(name: String, val parent: Mama) extends RestrictActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends RestrictDefine, Stateless  :
    type Accept = Daughter | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingRestrictRelayedSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends RestrictActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender)


  object Grandma extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = Mama
    type FamilyAccept = Anonymous
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends FamilyLetter[Accept]

  class Mama(name: String, val parent: Grandma) extends RestrictActor(Mama,name), FamilyBranchRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = Son | Daughter
    type FamilyAccept = Mama
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends FamilyLetter[FamilyAccept]

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Sender], sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends RestrictActor(Son,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends RestrictDefine, Stateless  :
    type Accept = Son | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  class Daughter(name: String, val parent: Mama) extends RestrictActor(Daughter,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends RestrictDefine, Stateless  :
    type Accept = Daughter | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello



object SiblingRestrictRelayRelayedSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends RestrictActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender)

  object Grandma extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = Mama
    type FamilyAccept = Anonymous
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends FamilyLetter[Accept]

  class Mama(name: String, val parent: Grandma) extends RestrictActor(Mama,name), FamilyBranchRelayRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        relayAll(Mama.FamilyHello,this)

  object Mama extends RestrictDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]
    type ChildActor = BareActor
    type FamilyAccept = Mama
    sealed trait FamilyLetter[Sender >: FamilyCommon <: FamilyAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends FamilyLetter[FamilyAccept]

  class Son(name: String, val parent: Mama) extends RestrictActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends RestrictDefine, Stateless :
    type Accept = Son | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  class Daughter(name: String, val parent: Mama) extends RestrictActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends RestrictDefine, Stateless  :
    type Accept = Daughter | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello
