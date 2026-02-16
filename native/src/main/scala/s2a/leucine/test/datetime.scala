package s2a.leucine.test

/* Stub for Datetime as Native has no implementation.
 * Should not be used in tests.  */
class DateTime(timestamp: Long) :
  val nano: Int  = (timestamp % 1000000000).toInt
  val micro: Int = nano  / 1000
  val milli: Int = micro / 1000
  def year: Int  = 0
  def month: Int = 0
  def day: Int   = 0
  def hour: Int  = 0
  def min: Int   = 0
  def sec: Int   = 0
