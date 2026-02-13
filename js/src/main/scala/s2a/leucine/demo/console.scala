package s2a.leucine.demo

/**
 * MIT License
 *
 * Copyright (c) 2023 Ruud Vlaming
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 **/

import scala.scalajs.js
import js.Dynamic.global


/* Utility methods for the commend line interface under the Node JS */
object CLI :
  /* NodeJS supports a simple cli. */
  private val cli = Node.createCLI()
  /* Communication runs over a callback function. */
  def talk(tell: String, listen: String => Unit) = cli.question(tell,listen)
  /* This cli must be closed at the end. */
  def close(): Unit = cli.close()

  /* Platform independent way of obtaining the arguments passed at startup. */
  def argsOf(passed: Array[String]): Array[String] =
    /* Test if there was nothing passed yet and if this environment has process arguments available (node.js!) ... */
    if passed.isEmpty && !js.isUndefined(global.process) && !js.isUndefined(global.process.argv) then
      /* ... if so, get them. This typically returns something like:
       * Array[/opt/local/bin/node,../leucine/js/target/scala-3.3.7/leucine-fastopt/main.js,<arg1>,<arg2>]*/
      val argv = global.process.argv.asInstanceOf[js.Array[String]]
      /* We only want the arguments, not the start up path and not the application path, thus */
      argv.drop(2).toArray
    else
      /* ... if not, give back what we got. */
      passed
