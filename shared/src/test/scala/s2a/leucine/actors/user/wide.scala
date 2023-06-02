package s2a.leucine.actors

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred}

object WideActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system


  class Joni(val writeln: String => Unit, val done: () => Unit) extends WideActor(Joni,"Joni") :
    var mary: Option[Mary] = None
    var sara: Option[Sara] = None
    def sendJoni(text: String, depth: Int) = this ! Common.Text(s"${text}J",depth+1)
    def sendMary(text: String, depth: Int) = mary.foreach(_ ! Common.Text(s"${text}J",depth+1))
    def sendSara(text: String, depth: Int) = sara.foreach(_ ! Common.Text(s"${text}J",depth+1))
    def receive(letter: Letter, sender: Sender): Unit = (letter,sender) match
      case (Common.Config(_,mary,sara),_)      => this.mary=Some(mary); this.sara=Some(sara)
      case (Common.Text(text,depth), _: Joni)  => writeln(s"$text$depth"); done()
      case (Common.Text(text,depth), _: Mary)  => if depth< 5 then sendSara(text,depth) else sendJoni(text,depth)
      case (Common.Text(text,depth), _: Sara)  => if depth< 5 then sendMary(text,depth) else sendJoni(text,depth)
      case (Common.Text(_,0), _ : Anonymous)   => sendMary("*",0); sendSara("*",0)
      case (Common.Text(_,_), _ : Anonymous)   => unmatched(letter,sender)

  object Joni extends WideDefine, Stateless



  class Mary(val writeln: String => Unit, val done: () => Unit) extends WideActor(Mary,"Mary") :
    var joni: Option[Joni] = None
    var sara: Option[Sara] = None
    def sendJoni(text: String, depth: Int) = joni.foreach(_ ! Common.Text(s"${text}M",depth+1))
    def sendMary(text: String, depth: Int) = this ! Common.Text(s"${text}M",depth+1)
    def sendSara(text: String, depth: Int) = sara.foreach(_ ! Common.Text(s"${text}M",depth+1))

    def receive(letter: Letter, sender: Sender): Unit = (letter,sender) match
      case (Common.Config(joni,_,sara),_)      => this.joni=Some(joni); this.sara=Some(sara)
      case (Common.Text(text,depth), _: Joni)  => if depth< 5 then sendSara(text,depth) else sendMary(text,depth)
      case (Common.Text(text,depth), _: Mary)  => writeln(s"$text$depth"); done()
      case (Common.Text(text,depth), _: Sara)  => if depth< 5 then sendJoni(text,depth) else sendMary(text,depth)
      case (Common.Text(_,0), _ : Anonymous)   => sendJoni("*",0); sendSara("*",0)
      case (Common.Text(_,_), _ : Anonymous)   => unmatched(letter,sender)

  object Mary extends WideDefine, Stateless


  class Sara(val writeln: String => Unit, val done: () => Unit) extends WideActor(Sara,"Sara") :
    var joni: Option[Joni] = None
    var mary: Option[Mary] = None
    def sendJoni(text: String, depth: Int) = joni.foreach(_ ! Common.Text(s"${text}S",depth+1))
    def sendMary(text: String, depth: Int) = mary.foreach(_ ! Common.Text(s"${text}S",depth+1))
    def sendSara(text: String, depth: Int) = this ! Common.Text(s"${text}S",depth+1)

    def receive(letter: Letter, sender: Sender): Unit = (letter,sender) match
      case (Common.Config(joni,mary,_),_)      => this.joni=Some(joni); this.mary=Some(mary)
      case (Common.Text(text,depth), _: Joni)  => if depth< 5 then sendMary(text,depth) else sendSara(text,depth)
      case (Common.Text(text,depth), _: Mary)  => if depth< 5 then sendJoni(text,depth) else sendSara(text,depth)
      case (Common.Text(text,depth), _: Sara)  => writeln(s"$text$depth"); done()
      case (Common.Text(_,0), _ : Anonymous)   => sendJoni("*",0); sendMary("*",0)
      case (Common.Text(_,_), _ : Anonymous)   => unmatched(letter,sender)

  object Sara extends WideDefine, Stateless

  object Common :
    sealed trait Letter extends Actor.Letter[Actor]
    case class Config(joni: Joni, mary: Mary, sara: Sara) extends Letter
    case class Text(text: String, depth: Int) extends Letter



  val tests = Tests {
    val expect = Set("*JMSJMS6", "*MJSMJS6", "*JSMJSM6", "*MSJMSJ6", "*SMJSMJ6", "*SJMSJM6")
    given Actor.Anonymous = Actor.Anonymous
    test("sending looped letters"){
      val buffer = Buffer[String]
      val deferred = Deferred(buffer.readlns,6)
      val joni = Joni(buffer.writeln,deferred.done)
      val mary = Mary(buffer.writeln,deferred.done)
      val sara = Sara(buffer.writeln,deferred.done)
      joni ! Common.Config(joni,mary,sara)
      mary ! Common.Config(joni,mary,sara)
      sara ! Common.Config(joni,mary,sara)
      joni ! Common.Text("",0)
      mary ! Common.Text("",0)
      sara ! Common.Text("",0)
      deferred.await()
      deferred.compare(_.toSet ==> expect) }
   }
