package s2a.leucine.demo

import scala.concurrent.{Future, Promise}
import scala.util.Try

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
