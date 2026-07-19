package s2a.leucine.actors


import utest.*

object SiblingSelectSupply extends TestSuite, SiblingCommon :

  class Grandma(name: String) extends SelectActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = Mama
    type ChildAccept = Anonymous | Grandma
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends ChildLetter

  class Mama(name: String, val parent: Grandma) extends SelectActor(Mama,name), FamilyBranch[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        val _ = children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        val _ = children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = Son | Daughter
    type ChildAccept = Mama
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.ChildLetter, sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends SelectActor(Son,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.ChildLetter, sender: Sender): Boolean = super[SelectActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends SelectActor(Daughter,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.ChildLetter, sender: Sender): Boolean = super[SelectActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingSelectRelaySupply extends TestSuite, SiblingCommon :

  class Grandma(name: String) extends SelectActor(Grandma,name), FamilyRoot(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => children.foreach(child => child.send(Grandma.FamilyHello,sender))

  object Grandma extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = Mama
    type ChildAccept = Anonymous | Grandma
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends ChildLetter

  class Mama(name: String, val parent: Grandma) extends SelectActor(Mama,name), FamilyBranchRelay[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        val _ = children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        val _ = children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        val _ = relayAll(Mama.FamilyHello,this)

  object Mama extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = BareActor
    type ChildAccept = Mama
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  class Son(name: String, val parent: Mama) extends SelectActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends SelectActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello


object SiblingSelectRelayedSupply extends TestSuite, SiblingCommon :
  import Auxiliary.toUnit

  class Grandma(name: String) extends SelectActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender).toUnit


  object Grandma extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = Mama
    type ChildAccept = Anonymous | Grandma
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends ChildLetter

  class Mama(name: String, val parent: Grandma) extends SelectActor(Mama,name), FamilyBranchRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        val _ = children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        val _ = children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        children.foreach(child => child.send(Mama.FamilyHello,this))

  object Mama extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = Son | Daughter
    type ChildAccept = Mama
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.ChildLetter, sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends SelectActor(Son,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.ChildLetter, sender: Sender): Boolean = super[SelectActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends SelectActor(Daughter,name), Child :
    def send[Sender <: Mama.FamilyAccept](letter: Mama.ChildLetter, sender: Sender): Boolean = super[SelectActor].send(letter,sender)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello



object SiblingSelectRelayRelayedSupply extends TestSuite, SiblingCommon :
  import Auxiliary.toUnit

  class Grandma(name: String) extends SelectActor(Grandma,name), FamilyRootRelay(Grandma) :
    new Mama("m1",this)
    new Mama("m2",this)
    new Mama("m3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Grandma.Hello       => children.foreach(child => child.send(Mama.Hello,this))
      case Grandma.FamilyHello => relayAll(Grandma.FamilyHello,sender).toUnit

  object Grandma extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = Mama
    type ChildAccept = Anonymous | Grandma
    sealed trait ChildLetter extends Letter, Mama.Letter
    case object FamilyHello extends ChildLetter

  class Mama(name: String, val parent: Grandma) extends SelectActor(Mama,name), FamilyBranchRelayRelayed[Grandma,Mama.type](Mama) :
    new Son("s1",this); new Daughter("d1",this)
    new Son("s2",this); new Daughter("d2",this)
    new Son("s3",this); new Daughter("d3",this)
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Mama.Hello =>
        val _ = children.collect{ case (son : Son)           => son.send(Son.Hello,this) }
        val _ = children.collect{ case (daughter : Daughter) => daughter.send(Daughter.Hello,this) }
      case Grandma.FamilyHello =>
        val _ = relayAll(Mama.FamilyHello,this)

  object Mama extends SelectDefine, FamilyDefine, Stateless  :
    type Accept = Anonymous | Grandma
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter
    type ChildActor = BareActor
    type ChildAccept = Mama
    sealed trait ChildLetter extends Son.Letter, Daughter.Letter
    case object FamilyHello extends ChildLetter

  class Son(name: String, val parent: Mama) extends SelectActor(Son,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends SelectDefine, Stateless :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  class Daughter(name: String, val parent: Mama) extends SelectActor(Daughter,name), FamilyLeafRelayed[Mama] :
    def receive(letter: Letter, sender: Sender): Unit = letter match
      case Daughter.Hello   => writeln(s"$name-DH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Daughter extends SelectDefine, Stateless  :
    type Accept = Mama
    sealed trait Letter extends Actor.Letter[Accept]
    case object Hello extends Letter

  def start(): Unit =
    given Actor.Anonymous = Actor.Anonymous
    val grandma: Grandma = Grandma("Oma")
    grandma ! Grandma.Hello
    grandma ! Grandma.FamilyHello
