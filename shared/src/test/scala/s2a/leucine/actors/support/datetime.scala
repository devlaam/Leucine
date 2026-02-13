package s2a.leucine.actors

import scala.util.Random
import utest.*


/* Since DateTime is a completely independent of the platform, there may be no need to
 * test it on all platforms. Would also be hard, since we need a library to test on
 * Native. */
object DateTimeJVMTest extends TestSuite :
  import java.time.{LocalDateTime, ZoneOffset}

  val maxTS = 2000000000000000000L


  class JVMDateTime(timestamp: Long) :
    private val seconds = timestamp / 1000000000
    private val ldt = LocalDateTime.ofEpochSecond(seconds, 0, ZoneOffset.UTC)
    val nano  = timestamp - (seconds * 1000000000)
    val micro = nano  / 1000
    val milli = micro / 1000
    def year  = ldt.getYear
    def month = ldt.getMonthValue
    def day   = ldt.getDayOfMonth
    def hour  = ldt.getHour
    def min   = ldt.getMinute
    def sec   = ldt.getSecond

  def checkDateTime(timestamp: Long) =
    val ref = JVMDateTime(timestamp)
    val tst = DateTime(timestamp)
    ref.year   ==> tst.year
    ref.month  ==> tst.month
    ref.day    ==> tst.day
    ref.hour   ==> tst.hour
    ref.min    ==> tst.minute
    ref.sec    ==> tst.second
    ref.milli  ==> tst.milli
    ref.micro  ==> tst.micro
    ref.nano   ==> tst.nano

  val tests = Tests :
    test("Zero timestamp")   - checkDateTime(0)
    test("Now timestamp")    - checkDateTime(System.currentTimeMillis()*1000000)
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))
    test("Random timestamp") - checkDateTime(Random.nextLong(maxTS))



