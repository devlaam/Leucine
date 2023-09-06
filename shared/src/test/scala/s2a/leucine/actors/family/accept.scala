package s2a.leucine.actors


import utest.*

import s2a.leucine.actors.Actor.Anonymous


object SiblingAcceptSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends AcceptActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = Mama
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends ChildLetter

  class Mama(name: String, val parent: Grandma) extends AcceptActor(Mama,name), FamilyBranch[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = Son | Daughter
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Actor], sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends AcceptActor(Son,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Actor], sender: Sender): Boolean = super[AcceptActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends AcceptActor(Daughter,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Actor], sender: Sender): Boolean = super[AcceptActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingAcceptRelaySupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends AcceptActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = Mama
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends FamilyLetter[Actor]

  class Mama(name: String, val parent: Grandma) extends AcceptActor(Mama,name), FamilyBranchRelay[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        relayAll(Mama.FamilyHello,this)

  object Mama extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = BareActor
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  class Son(name: String, val parent: Mama) extends AcceptActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends AcceptActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingAcceptRelayedSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends AcceptActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender)


  object Grandma extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = Mama
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends FamilyLetter[Actor]

  class Mama(name: String, val parent: Grandma) extends AcceptActor(Mama,name), FamilyBranchRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = Son | Daughter
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Actor], sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends AcceptActor(Son,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Actor], sender: Sender): Boolean = super[AcceptActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends AcceptActor(Daughter,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.FamilyLetter[Actor], sender: Sender): Boolean = super[AcceptActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello



object SiblingAcceptRelayRelayedSupply extends TestSuite, SiblingCommon :
  import TestMethods.*

  class Grandma(name: String) extends AcceptActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender)

  object Grandma extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = Mama
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends FamilyLetter[Actor]

  class Mama(name: String, val parent: Grandma) extends AcceptActor(Mama,name), FamilyBranchRelayRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        relayAll(Mama.FamilyHello,this)

  object Mama extends AcceptDefine, FamilyDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter
    type ChildActor = BareActor
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  class Son(name: String, val parent: Mama) extends AcceptActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends AcceptDefine, Stateless :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends AcceptActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello
