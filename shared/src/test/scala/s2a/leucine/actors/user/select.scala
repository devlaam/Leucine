package s2a.leucine.actors

import utest.*

import s2a.control.{Buffer, Deferred}

object SelectActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system


  class Joni(val writeln: String => Unit, val done: () => Unit) extends SelectActor(Joni,"Joni") :
    var mary: Option[Mary] = None
    var sara: Option[Sara] = None
    def sendJoni(text: String, depth: Int) = this ! Joni.Text(s"${text}J",depth+1)
    def sendMary(text: String, depth: Int) = mary.foreach(_ ! Mary.Text(s"${text}J",depth+1))
    def sendSara(text: String, depth: Int) = sara.foreach(_ ! Sara.Text(s"${text}J",depth+1))
    def receive(letter: Letter, sender: Sender): Unit = (letter,sender) match
      case (Joni.Config(mary,sara),_)        => this.mary=Some(mary); this.sara=Some(sara)
      case (Joni.Text(text,depth), _: Joni)  => writeln(s"$text$depth"); done()
      case (Joni.Text(text,depth), _: Mary)  => if depth< 5 then sendSara(text,depth) else sendJoni(text,depth)
      case (Joni.Text(text,depth), _: Sara)  => if depth< 5 then sendMary(text,depth) else sendJoni(text,depth)
      case (Joni.Text(_,0), _ : Anonymous)   => sendMary("*",0); sendSara("*",0)
      case (Joni.Text(_,_), _ : Anonymous)   => unmatched(letter,sender)

  object Joni extends SelectDefine, Stateless  :
    type Accept = Joni | Mary | Sara | Anonymous
    sealed trait Letter extends Actor.Letter[Accept]
    case class Text(text: String, depth: Int) extends Letter
    case class Config(mary: Mary, sara: Sara) extends Letter



  class Mary(val writeln: String => Unit, val done: () => Unit) extends SelectActor(Mary,"Mary") :
    var joni: Option[Joni] = None
    var sara: Option[Sara] = None
    def sendJoni(text: String, depth: Int) = joni.foreach(_ ! Joni.Text(s"${text}M",depth+1))
    def sendMary(text: String, depth: Int) = this ! Mary.Text(s"${text}M",depth+1)
    def sendSara(text: String, depth: Int) = sara.foreach(_ ! Sara.Text(s"${text}M",depth+1))

    def receive(letter: Letter, sender: Sender): Unit = (letter,sender) match
      case (Mary.Config(joni,sara),_)        => this.joni=Some(joni); this.sara=Some(sara)
      case (Mary.Text(text,depth), _: Joni)  => if depth< 5 then sendSara(text,depth) else sendMary(text,depth)
      case (Mary.Text(text,depth), _: Mary)  => writeln(s"$text$depth"); done()
      case (Mary.Text(text,depth), _: Sara)  => if depth< 5 then sendJoni(text,depth) else sendMary(text,depth)
      case (Mary.Text(_,0), _ : Anonymous)   => sendJoni("*",0); sendSara("*",0)
      case (Mary.Text(_,_), _ : Anonymous)   => unmatched(letter,sender)

  object Mary extends SelectDefine, Stateless  :
    type Accept = Joni | Mary | Sara | Anonymous
    sealed trait Letter extends Actor.Letter[Accept]
    case class Text(text: String, depth: Int) extends Letter
    case class Config(joni: Joni, sara: Sara) extends Letter


  class Sara(val writeln: String => Unit, val done: () => Unit) extends SelectActor(Sara,"Sara") :
    var joni: Option[Joni] = None
    var mary: Option[Mary] = None
    def sendJoni(text: String, depth: Int) = joni.foreach(_ ! Joni.Text(s"${text}S",depth+1))
    def sendMary(text: String, depth: Int) = mary.foreach(_ ! Mary.Text(s"${text}S",depth+1))
    def sendSara(text: String, depth: Int) = this ! Sara.Text(s"${text}S",depth+1)

    def receive(letter: Letter, sender: Sender): Unit = (letter,sender) match
      case (Sara.Config(joni,mary),_)        => this.joni=Some(joni); this.mary=Some(mary)
      case (Sara.Text(text,depth), _: Joni)  => if depth< 5 then sendMary(text,depth) else sendSara(text,depth)
      case (Sara.Text(text,depth), _: Mary)  => if depth< 5 then sendJoni(text,depth) else sendSara(text,depth)
      case (Sara.Text(text,depth), _: Sara)  => writeln(s"$text$depth"); done()
      case (Sara.Text(_,0), _ : Anonymous)   => sendJoni("*",0); sendMary("*",0)
      case (Sara.Text(_,_), _ : Anonymous)   => unmatched(letter,sender)

  object Sara extends SelectDefine, Stateless  :
    type Accept = Joni | Mary | Sara | Anonymous
    sealed trait Letter extends Actor.Letter[Accept]
    case class Text(text: String, depth: Int) extends Letter
    case class Config(joni: Joni, mary: Mary) extends Letter


  val tests = Tests {
    val expect = Set("*JMSJMS6", "*MJSMJS6", "*JSMJSM6", "*MSJMSJ6", "*SMJSMJ6", "*SJMSJM6")
    given Actor.Anonymous = Actor.Anonymous
    test("sending looped letters"){
      val buffer = Buffer[String]
      val deferred = Deferred(buffer.readlns,6)
      val joni = Joni(buffer.writeln,deferred.done)
      val mary = Mary(buffer.writeln,deferred.done)
      val sara = Sara(buffer.writeln,deferred.done)
      joni ! Joni.Config(mary,sara)
      mary ! Mary.Config(joni,sara)
      sara ! Sara.Config(joni,mary)
      joni ! Joni.Text("",0)
      mary ! Mary.Text("",0)
      sara ! Sara.Text("",0)
      deferred.await()
      deferred.compare(_.toSet ==> expect) }
   }
