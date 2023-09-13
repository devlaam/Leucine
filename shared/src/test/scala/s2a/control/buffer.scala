package s2a.control

import scala.collection.mutable.ListBuffer
import s2a.leucine.actors.Auxiliary

/** Use this to catch your results from different threads and read the total afterwards. */
class Buffer[T] :
  import Auxiliary.toUnit
  val result = new ListBuffer[T]()
  def writeln(s: T): Unit = synchronized{result.append(s).toUnit}
  def readlns: List[T]    = synchronized{result.toList}
