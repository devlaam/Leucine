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

import java.nio.channels.SocketChannel
import java.nio.ByteBuffer


/** JVM platform specific implementation of the ClientSocket */
class ClientSocketImplementation(socketChannel: SocketChannel) extends ClientSocket :

  /** Obtain the port number of the connection on this side. */
  def localPort: Int  = socketChannel.socket().getLocalPort()

  /** Obtain the port number of the connection on the other side. */
  def remotePort: Int = socketChannel.socket().getPort()

  /** Write (and flush) some text to the socket. */
  def writeln(text: String): Unit = socketChannel.write(ByteBuffer.wrap(s"$text\n".getBytes()))

  /**
   * Read some text from the socket (up to the newline).
   * Not implemented here, since this is socket is write only. */
  def readln: String = ""

  /** Close this socket */
  def close(): Unit =
    socketChannel.socket().close()
    socketChannel.close()
