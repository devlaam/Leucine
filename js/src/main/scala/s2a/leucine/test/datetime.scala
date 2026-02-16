package s2a.leucine.test

import scala.scalajs.js.Date

/* JS implementation to get the separate parts of the given time in nano seconds since epoch 1970. */
class DateTime(timestamp: Long) :
  private val date = new Date((timestamp / 1000000D))
  val nano: Int  = (timestamp % 1000000000).toInt
  val micro: Int = nano  / 1000
  val milli: Int = micro / 1000
  def year: Int  = date.getUTCFullYear().toInt
  def month: Int = date.getUTCMonth().toInt + 1
  def day: Int   = date.getUTCDate().toInt
  def hour: Int  = date.getUTCHours().toInt
  def min: Int   = date.getUTCMinutes().toInt
  def sec: Int   = date.getUTCSeconds().toInt
