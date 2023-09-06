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


/* Holder for the Java Script facades for Node JS objects. */
object Node :
  import js.annotation.{JSGlobalScope, JSImport}
  import js.Dynamic.{global, literal}

  @JSGlobalScope
  @js.native
  object Global extends js.Object :
    val require: js.Function1[String, js.Dynamic] = js.native

  @js.native
  trait Readline extends js.Object :
    def createInterface(options: js.Dynamic): Interface = js.native

  @js.native
  trait Interface extends js.Object :
    def question(query: String, callback: js.Function1[String, Unit]): Unit = js.native
    def close(): Unit = js.native

  global.process.stdin.on("close", () => println("stdin closed"))
  private val dynIO    = literal(input = global.process.stdin, output = global.process.stdout, terminal = true, prompt = ">>")
  private val readline = Global.require("readline").asInstanceOf[Readline]
  def createCLI(): Interface = readline.createInterface(dynIO)

  @JSImport("net", JSImport.Default)
  @js.native
  class Socket extends js.Object :
    def write(data: String): Unit = js.native
    def destroy(): Unit = js.native
    def localPort: Int = js.native
    def remotePort: Int = js.native
    def on(event: String, callback: js.Function1[String, Unit]): Unit = js.native

  @JSImport("net", JSImport.Default)
  @js.native
  class Server extends js.Object :
    def listen(port: Int, host: String): Unit = js.native
    def close(): Unit = js.native

  @JSImport("net", JSImport.Default)
  @js.native
  object Net extends js.Object :
    def createServer(cb: js.Function1[Socket, Unit]): Server = js.native
