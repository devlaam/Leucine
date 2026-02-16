package s2a.leucine.actors

import scala.util.Random
import utest.*


/* Since DateTime is a completely independent of the platform, there may be no need to
 * test it on all platforms. Would also be hard, since we need a library to test on
 * Native. */
object DateTimeTestNN extends TestSuite :
  import s2a.leucine.test.DateTime as RefDateTime

  val maxTS = 2000000000000000000L

  def checkDateTime(timestamp: Long) =
    val ref = RefDateTime(timestamp)
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
