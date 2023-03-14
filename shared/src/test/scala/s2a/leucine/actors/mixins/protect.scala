package s2a.leucine.actors

import utest.*
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import s2a.control.{Buffer, Deferred}

object ProtectActorTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Digest(val writeln: String => Unit, val done: () => Unit) extends BasicActor[Digest.Letter], ProtectActor :
    val name = "Digest"

    override val maxMailboxSize = 4
    val alarmSize = 2

    override protected def stopped(complete: Boolean) =
      writeln(s"stop:$complete")
      done()

    protected def sizeAlarm(full: Boolean): Unit =
      writeln(s"alarm:$full")

    def receive(letter: Digest.Letter) = letter match
      case Digest.Knock(i) =>  writeln(s"Knock($i)")

  object Digest :
     sealed trait Letter extends Actor.Letter
     case class Knock(i: Int) extends Letter

  val tests = Tests {
    val buffer = Buffer[String]
    test("overflow mailbox"){
      def expect(result: List[String]): String =
        result.contains("alarm:true") ==> true
        result.contains("alarm:false") ==> true
        result.indexOf("alarm:true") < result.indexOf("alarm:false") ==> true
        result.indexOf("Knock(1)") < result.indexOf("alarm:true") ==> true
        result.indexOf("Knock(2)") < result.indexOf("alarm:false") ==> true
        result.find(_.contains("accepted")).map(s => Auxiliary.splitAt(s,'=')._2).getOrElse("") ==> "accepted=7"
        result.mkString
      val deferred = Deferred(buffer.readlns,timeout=200.millis)
      val digest = new Digest(buffer.writeln,deferred.done)
      def act(i: Int) = digest.send(Digest.Knock(i))
      val accepted  = (1 to 10).map(act).count(identity)
      buffer.writeln(s"accepted=$accepted")
      deferred.await()
      test("alarm:true")               - { deferred.compare(_.contains("alarm:true") ==> true) }
      test("alarm:false")              - { deferred.compare(_.contains("alarm:false") ==> true) }
      test("alarm:true < alarm:false") - { deferred.compare(r => (r.indexOf("alarm:true") < r.indexOf("alarm:false")) ==> true) }
      test("contains Knock(1)")        - { deferred.compare(_.contains("Knock(1)") ==> true) }
      test("contains Knock(2)")        - { deferred.compare(_.contains("Knock(2)") ==> true) }
      test("contains Knock(3)")        - { deferred.compare(_.contains("Knock(3)") ==> true) }
      test("contains Knock(4)")        - { deferred.compare(_.contains("Knock(4)") ==> true) }
      test("accepted >= 4")            - { deferred.compare(_.find(_.contains("accepted")).map(s => Auxiliary.splitAt(s,'=')._2).getOrElse("").toInt >= 4 ==> true) } } }
