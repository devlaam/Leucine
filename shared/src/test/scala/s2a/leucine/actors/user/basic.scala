package s2a.leucine.actors

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.Deferred

object BasicActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Writer(val name: String, val writeln: String => Unit) extends BasicActor[Writer.Letter] :
    override protected def stopped()   = writeln(s"$name:stopped")
    def receive(letter: Writer.Letter) = letter match
      case  Writer.Text(text: String) => writeln(s"$name:$text")
      case  Writer.Number(int: Int)   => writeln(s"$name:$int")

  object Writer :
    sealed trait Letter extends Actor.Letter
    case class Text(text: String) extends Letter
    case class Number(int: Int)   extends Letter


  val tests = Tests {
    val result = new ListBuffer[String]()
    def writeln(s: String)    = synchronized{result.append(s)}
    def readlns: List[String] = synchronized{result.toList}
    test("sending letters, stop at the end"){
      val writer = new Writer("A",writeln)
      writer.send(Writer.Text("text1"))
      writer.send(Writer.Number(1))
      writer.send(Writer.Text("text2"))
      writer.send(Writer.Number(2))
      writer.send(Actor.Letter.Finish)
      Deferred(readlns).result.map(_ ==> List("A:text1","A:1","A:text2","A:2","A:stopped")) }
    test("sending letters, stop in the middle."){
      val writer = new Writer("B",writeln)
      writer.send(Writer.Text("text3"))
      writer.send(Writer.Number(3))
      writer.send(Actor.Letter.Finish)
      writer.send(Writer.Text("text4"))
      writer.send(Writer.Number(4))
      writer.send(Actor.Letter.Finish)
      Deferred(readlns).result.map(_ ==> List("B:text3","B:3","B:stopped")) }
    test("sending letters, stop at the start."){
      val writer = new Writer("C",writeln)
      writer.send(Actor.Letter.Finish)
      writer.send(Writer.Text("text5"))
      writer.send(Writer.Number(5))
      Deferred(readlns).result.map(_ ==> List("C:stopped")) } }







