package s2a.control

import scala.annotation.tailrec

object Helpers :
   @tailrec
   def repeat[T](n: Int)(body: => T): T =
    val result = body
    if n<=0 then result else repeat(n-1)(body)
