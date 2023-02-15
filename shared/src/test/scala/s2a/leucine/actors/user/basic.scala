package s2a.leucine.actors

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.Deferred

object BasicActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Writer(val name: String, val writeln: String => Unit) extends BasicActor[Writer.Letter] :
    override protected def stopped() = writeln(s"$name:stopped")
    override protected def except(letter: MyLetter, cause: Exception, size: Int) = writeln(s"except(${cause.getMessage()},$size)")
    def receive(letter: Writer.Letter) = letter match
      case  Writer.Text(text: String) => writeln(s"$name:$text")
      case  Writer.Number(int: Int)   => writeln(s"$name:$int")
      case  Writer.Except             => throw new Exception(name)

  object Writer :
    sealed trait Letter extends Actor.Letter
    case class Text(text: String) extends Letter
    case class Number(int: Int) extends Letter
    case object Except extends Letter


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
      writer.stopFinish()
      Deferred(readlns).result.map(_ ==> List("A:text1","A:1","A:text2","A:2","A:stopped")) }
    test("sending letters, stop in the middle."){
      val writer = new Writer("B",writeln)
      writer.send(Writer.Text("text3"))
      writer.send(Writer.Number(3))
      writer.stopFinish()
      writer.send(Writer.Text("text4"))
      writer.send(Writer.Number(4))
      writer.stopFinish()
      Deferred(readlns).result.map(_ ==> List("B:text3","B:3","B:stopped")) }
    test("sending letters, stop at the start."){
      val writer = new Writer("C",writeln)
      writer.stopFinish()
      writer.send(Writer.Text("text5"))
      writer.send(Writer.Number(5))
      Deferred(readlns).result.map(_ ==> List("C:stopped")) }
    test("sending letters with exceptions"){
      val writer = new Writer("D",writeln)
      writer.send(Writer.Text("text6"))
      writer.send(Writer.Except)
      writer.send(Writer.Number(6))
      writer.send(Writer.Text("text7"))
      writer.send(Writer.Except)
      writer.send(Writer.Number(7))
      writer.send(Writer.Except)
      writer.stopFinish()
      Deferred(readlns).result.map(_ ==> List("D:text6","except(D,1)","D:6","D:text7","except(D,2)","D:7","except(D,3)","D:stopped")) }
    test("sending letters with random stop"){
      val writer = new Writer("E",writeln)
      def result(n: Int) = if n==0 then Nil else (1 until n).map(i => s"E:$i").appended("E:stopped").toList
      ac.delayed(writer.stopDirect(), 1.millis)
      for i <- 1 until 100 do writer.send(Writer.Number(i))
      Deferred(readlns).result.map(l => l ==> result(l.size)) } }
