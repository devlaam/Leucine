package s2a.leucine.demo

import scala.io.StdIn

object CLI :
  /* One time converstion in the console */
  def talk(tell: String, listen: String => Unit) =
    print(tell)
    /* Yes, this blocks, but it is only at the start, so for the demo this is acceptable. */
    listen(StdIn.readLine)
  /* Actually, there is nothing to close here */
  def close(): Unit = ()
