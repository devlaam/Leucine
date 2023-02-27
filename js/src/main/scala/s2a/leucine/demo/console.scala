package s2a.leucine.demo

import scala.concurrent.{Future, Promise}
import scala.util.Try

import scala.scalajs.js

object CLI :
  /* NodeJS supports a simple cli. */
  private val cli = Node.createCLI()
  /* Communication runs over a callback function. */
  def talk(tell: String, listen: String => Unit) = cli.question(tell,listen)
  /* This cli must be closed at the end. */
  def close(): Unit = cli.close()
