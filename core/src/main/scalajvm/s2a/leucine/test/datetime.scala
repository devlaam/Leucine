package s2a.leucine.test

import java.time.{LocalDateTime, ZoneOffset}

/* JVM implementation to get the separate parts of the given time in nano seconds since epoch 1970. */
class DateTime(timestamp: Long) :
  private val ldt = LocalDateTime.ofEpochSecond(timestamp / 1000000000, 0, ZoneOffset.UTC)
  val nano: Int  = (timestamp % 1000000000).toInt
  val micro: Int = nano  / 1000
  val milli: Int = micro / 1000
  def year: Int  = ldt.getYear
  def month: Int = ldt.getMonthValue
  def day: Int   = ldt.getDayOfMonth
  def hour: Int  = ldt.getHour
  def min : Int  = ldt.getMinute
  def sec: Int   = ldt.getSecond
