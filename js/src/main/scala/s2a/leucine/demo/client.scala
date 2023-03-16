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


/** JS platform specific implementation of the ClientSocket */
class ClientSocketImplementation(jsSocket: Node.Socket) extends ClientSocket :

  /** Container to collect the data send over the line. */
  private val data: StringBuilder = StringBuilder()

  /** Method to fill the data container. */
  private val receive: String => Unit = (s) => data.append(s)

  jsSocket.on("data",receive)

  /** Obtain the port number of the connection on this side. */
  def localPort: Int  = jsSocket.localPort

  /** Obtain the port number of the connection on the other side. */
  def remotePort: Int = jsSocket.remotePort

  /** Write (and flush) some text to the socket. */
  def writeln(text: String): Unit = jsSocket.write(s"$text\n")

  /** Read some text from the socket (up to the newline) */
  def readln: String =
    val result = data.toString()
    data.clear
    result

  /** Close this socket */
  def close(): Unit = jsSocket.destroy()

