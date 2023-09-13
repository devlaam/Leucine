package s2a.leucine.actors


import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred}

object AcceptActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Writer(name: String, val writeln: String => Unit, val done: () => Unit) extends AcceptActor(Writer,name) :
    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      writeln(s"$name:stop:$complete")
      done()
    override protected def except(letter: MyLetter[Accept], sender: Sender, cause: Exception, size: Int) = writeln(s"except(${cause.getMessage()},$size)")
    def receive(letter: Writer.Letter, sender: Sender): Unit = letter match
      case  Writer.Text(text: String) => writeln(s"$name:$text")
      case  Writer.Number(int: Int)   => writeln(s"$name:$int")
      case  Writer.Except             => throw new Exception(name)

  object Writer extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case class Text(text: String) extends Letter
    case class Number(int: Int) extends Letter
    case object Except extends Letter

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, stop at the end"){
      val deferred = Deferred(buffer.readlns)
      val writer = new Writer("A",buffer.writeln,deferred.done)
      val _ = writer.send(Writer.Text("text1"))
      val _ = writer.send(Writer.Number(1))
      val _ = writer.send(Writer.Text("text2"))
      val _ = writer.send(Writer.Number(2))
      val _ = writer.stop(Actor.Stop.Finish)
      deferred.await()
      deferred.compare(_ ==> List("A:text1","A:1","A:text2","A:2","A:stop:true")) }
    test("sending letters, stop in the middle."){
      val deferred = Deferred(buffer.readlns)
      val writer = new Writer("B",buffer.writeln,deferred.done)
      val _ = writer.send(Writer.Text("text3"))
      val _ = writer.send(Writer.Number(3))
      val _ = writer.stop(Actor.Stop.Finish)
      val _ = writer.send(Writer.Text("text4"))
      val _ = writer.send(Writer.Number(4))
      val _ = writer.stop(Actor.Stop.Finish)
      deferred.await()
      deferred.compare(_ ==> List("B:text3","B:3","B:stop:true")) }
    test("sending letters, stop at the start."){
      val deferred = Deferred(buffer.readlns)
      val writer = new Writer("C",buffer.writeln,deferred.done)
      val _ = writer.stop(Actor.Stop.Finish)
      val _ = writer.send(Writer.Text("text5"))
      val _ = writer.send(Writer.Number(5))
      val _ = deferred.await()
      deferred.compare(_ ==> List("C:stop:true")) }
    test("sending letters with exceptions"){
      val deferred = Deferred(buffer.readlns)
      val writer = new Writer("D",buffer.writeln,deferred.done)
      val _ = writer.send(Writer.Text("text6"))
      val _ = writer.send(Writer.Except)
      val _ = writer.send(Writer.Number(6))
      val _ = writer.send(Writer.Text("text7"))
      val _ = writer.send(Writer.Except)
      val _ = writer.send(Writer.Number(7))
      val _ = writer.send(Writer.Except)
      val _ = writer.stop(Actor.Stop.Finish)
      deferred.await()
      deferred.compare(_ ==> List("D:text6","except(D,1)","D:6","D:text7","except(D,2)","D:7","except(D,3)","D:stop:true")) }
    test("sending letters with random stop"){
      val deferred = Deferred(buffer.readlns,0,110.millis)
      val writer = new Writer("E",buffer.writeln,() => ())
      def result(processed: Int, accepted: Int) = (1 until processed).map(i => s"E:$i").appended(s"E:stop:${processed-1==accepted}").toList
      val _ = ac.delayed(writer.stop(Actor.Stop.Direct), 1.millis)
      val accepted = (1 until 30).map(i => writer.send(Writer.Number(i))).count(identity)
      deferred.await()
      deferred.compare(l => l ==> result(l.size,accepted) ) } }
