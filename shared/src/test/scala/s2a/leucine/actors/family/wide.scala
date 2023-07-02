package s2a.leucine.actors


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.annotation.nowarn
import utest.*

import s2a.control.{Buffer, Deferred}
import s2a.leucine.actors.Actor.Anonymous
import s2a.control.Helpers.*


object SiblingWideSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends WideActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = Mama
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  class Mama(name: String, val parent: Grandma) extends WideActor(Mama,name), FamilyBranch[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = Son | Daughter
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  trait Child extends BareActor, FamilyLeaf[Mama]

  class Son(name: String, val parent: Mama) extends WideActor(Son,name), Child :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  class Daughter(name: String, val parent: Mama) extends WideActor(Daughter,name), Child :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingWideRelaySupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends WideActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = Mama
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  class Mama(name: String, val parent: Grandma) extends WideActor(Mama,name), FamilyBranchRelay[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        relayAll(Mama.FamilyHello,this)

  object Mama extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = BareActor
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  class Son(name: String, val parent: Mama) extends WideActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  class Daughter(name: String, val parent: Mama) extends WideActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingWideRelayedSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends WideActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender)


  object Grandma extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = Mama
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  class Mama(name: String, val parent: Grandma) extends WideActor(Mama,name), FamilyBranchRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = Son | Daughter
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  trait Child extends BareActor, FamilyLeaf[Mama]

  class Son(name: String, val parent: Mama) extends WideActor(Son,name), Child :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  class Daughter(name: String, val parent: Mama) extends WideActor(Daughter,name), Child :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello



object SiblingWideRelayRelayedSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends WideActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender)

  object Grandma extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = Mama
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  class Mama(name: String, val parent: Grandma) extends WideActor(Mama,name), FamilyBranchRelayRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        relayAll(Mama.FamilyHello,this)

  object Mama extends WideDefine, FamilyDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]
    type ChildActor = BareActor
    type ChildLetter = Actor.Letter[Actor]
    case object FamilyHello extends Actor.Letter[Actor]

  class Son(name: String, val parent: Mama) extends WideActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends WideDefine, Stateless :
    case object Hello extends Actor.Letter[Actor]

  class Daughter(name: String, val parent: Mama) extends WideActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends WideDefine, Stateless  :
    case object Hello extends Actor.Letter[Actor]

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello
