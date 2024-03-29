package s2a.leucine.actors


import utest.*

import s2a.control.{Buffer, Deferred}

object RefuseActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Writer(name: String, val writeln: String => Unit, val done: () => Unit) extends AcceptActor(Writer,name) :
    override protected def stopped(cause: Actor.Stop, complete: Boolean) =
      writeln(s"$name:stop:$complete")
      done()
    def receive(letter: Writer.Letter, sender: Sender): Unit = letter match
      case  Writer.Text(text: String) => writeln(s"$name:$text")
      case  Writer.Number(int: Int)   => writeln(s"$name:$int")
      case  Writer.Except             => throw new Exception(name)

  object Writer extends AcceptDefine, Stateless  :
    sealed trait Letter extends Actor.Letter[Actor]
    case class Text(text: String) extends Letter
    case class Number(int: Int) extends Letter
    case object Except extends Letter


  class Generator(name: String, writer: Writer, val writeln: String => Unit) extends RefuseActor(RefuseStateless,name) :

    def process(loops: Long): Unit =
      writeln(s"$name:enter")
      writer ! Writer.Text("text1")
      writer ! Writer.Number(1)
      writer ! Writer.Text("text2")
      writer ! Writer.Number(2)
      writeln(s"$name:exit")
      if loops == 1 then
        writer.stop(Actor.Stop.Finish)
        stop(Actor.Stop.Finish)

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, stop at the end"){
      val deferred = Deferred(buffer.readlns)
      val writer = Writer("A",buffer.writeln,deferred.done)
      Generator("B",writer,buffer.writeln)
      deferred.await()
      val _ = deferred.compare(_.filter(_.startsWith("A:")) ==> List("A:text1","A:1","A:text2","A:2","A:text1","A:1","A:text2","A:2","A:stop:true"))
      val _ = deferred.compare(_.filter(_.startsWith("B:")) ==> List("B:enter","B:exit","B:enter","B:exit")) } }
