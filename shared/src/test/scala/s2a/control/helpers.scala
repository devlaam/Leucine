package s2a.control

import scala.annotation.tailrec
import s2a.leucine.actors.{ActorContext,DefaultSystem}

object Helpers :

  @tailrec
  def repeat[T](n: Int)(body: => T): T =
    val result = body
    if n<=0 then result else repeat(n-1)(body)

  /* Recursive definition of integer power for non negative arguments and small exponents. */
  extension (value: Int)
    private def pow(x: Int, n: Int): Int = if n==1 then x else x * pow(x,n-1)
    def ** (exp: Int): Int =
      if       exp < 0      then throw Exception("Integer powers with negative exponent not supported.")
      else if  value < 0    then throw Exception("Integer powers with negative base not supported.")
      else if  value <= 1   then value
      else if  exp >= 32    then throw Exception("Integer number too large")
      else if  exp == 0     then 1
      else                  pow(value,exp)

  extension (value: String)
    def clean(n: Int = 200) = value.replaceAll("\\s","").replaceAll("s2a.leucine.actors.","").take(n)

  def emulatedContext: ActorContext = new ActorContext.ActorContexEmulatedImplementation(DefaultSystem)

