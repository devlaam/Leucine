package s2a.leucine.actors


import utest.*

import s2a.leucine.actors.Actor.Anonymous


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
    type ChildAccept = Anonymous
    sealed trait ChildLetter[Sender <: ChildAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends ChildLetter[Accept]

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
    type ChildAccept = Mama
    sealed trait ChildLetter[Sender <: ChildAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends ChildLetter[ChildAccept]

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.ChildAccept](letter: Mama.ChildLetter[Sender], sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends RestrictActor(Son,name), Child :
    def send[Sender <: Mama.ChildAccept](letter: Mama.ChildLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends RestrictDefine, Stateless  :
    type Accept = Son | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  class Daughter(name: String, val parent: Mama) extends RestrictActor(Daughter,name), Child :
    def send[Sender <: Mama.ChildAccept](letter: Mama.ChildLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
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
    type ChildAccept = Anonymous
    sealed trait ChildLetter[Sender <: ChildAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends ChildLetter[Accept]

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
    type ChildAccept = Mama
    sealed trait ChildLetter[Sender <: ChildAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends ChildLetter[ChildAccept]

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
    type ChildAccept = Anonymous
    sealed trait ChildLetter[Sender <: ChildAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends ChildLetter[Accept]

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
    type ChildAccept = Mama
    sealed trait ChildLetter[Sender <: ChildAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends ChildLetter[ChildAccept]

  trait Child extends BareActor, FamilyLeaf[Mama] :
    def send[Sender <: Mama.ChildAccept](letter: Mama.ChildLetter[Sender], sender: Sender): Boolean

  class Son(name: String, val parent: Mama) extends RestrictActor(Son,name), Child :
    def send[Sender <: Mama.ChildAccept](letter: Mama.ChildLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
    def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
      case Son.Hello        => writeln(s"$name-SH-${sender.name}")
      case Mama.FamilyHello => writeln(s"$name-MF-${sender.name}")

  object Son extends RestrictDefine, Stateless  :
    type Accept = Son | Mama
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case object Hello extends Letter[Accept]

  class Daughter(name: String, val parent: Mama) extends RestrictActor(Daughter,name), Child :
    def send[Sender <: Mama.ChildAccept](letter: Mama.ChildLetter[Sender], sender: Sender): Boolean = super[RestrictActor].send(letter,sender)
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
    type ChildAccept = Anonymous
    sealed trait ChildLetter[Sender <: ChildAccept] extends Letter[Sender], Mama.Letter[Sender]
    case object FamilyHello extends ChildLetter[Accept]

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
    type ChildAccept = Mama
    sealed trait ChildLetter[Sender <: ChildAccept] extends Son.Letter[Sender], Daughter.Letter[Sender]
    case object FamilyHello extends ChildLetter[ChildAccept]

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
