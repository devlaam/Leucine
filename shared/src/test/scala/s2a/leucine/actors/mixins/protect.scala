package s2a.leucine.actors

import utest.*
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import s2a.control.{Buffer, Deferred}

object ProtectAidTest extends TestSuite :

  implicit val ac: ActorContext = ActorContext.system

  class Digest(val writeln: String => Unit, val done: () => Unit) extends BasicActor[Digest.Letter](), ProtectAid :

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
      val deferred = Deferred(buffer.readlns,timeout=200.millis)
      val digest = new Digest(buffer.writeln,deferred.done)
      def act(i: Int) = digest.send(Digest.Knock(i))
      val accepted  = (1 to 10).map(act).count(identity)
      buffer.writeln(s"accepted=$accepted")
      deferred.await()
      /* When the alarm is raised it must also be lowered. If the false comes before true the must be both absent */
      test("alarm:true == alarm:false") - { deferred.compare(r => (r.contains("alarm:true") == r.contains("alarm:false")) ==> true) }
      /* When the alarm is raised it must be before lowering or the must be both absent (equal case: index = -1) */
      test("alarm:true <= alarm:false") - { deferred.compare(r => (r.indexOf("alarm:true") <= r.indexOf("alarm:false")) ==> true) }
      test("contains Knock(1)")         - { deferred.compare(_.contains("Knock(1)") ==> true) }
      test("contains Knock(2)")         - { deferred.compare(_.contains("Knock(2)") ==> true) }
      test("contains Knock(3)")         - { deferred.compare(_.contains("Knock(3)") ==> true) }
      test("contains Knock(4)")         - { deferred.compare(_.contains("Knock(4)") ==> true) }
      test("accepted >= 4")             - { deferred.compare(_.find(_.contains("accepted")).map(s => Auxiliary.splitAt(s,'=')._2).getOrElse("").toInt >= 4 ==> true) } } }
