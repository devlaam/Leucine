package s2a.control

import scala.collection.mutable.ListBuffer

/** Use this to catch your results from different threads and read the total afterwards. */
class Buffer[T] :
  val result = new ListBuffer[T]()
  def writeln(s: T)    = synchronized{result.append(s)}
  def readlns: List[T] = synchronized{result.toList}
