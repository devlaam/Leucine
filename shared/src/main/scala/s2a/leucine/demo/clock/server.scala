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


/* Sockets are not identical over all platforms. Therefore we make a minimal abstraction for a
 * local server and client socket connection. For illustration purposes only. This is NOT the
 * best way to do it, but it is short and clear. */

/**
 * Trans platform Server Socket that is able to accept a connection on a specified port and create
 * a trans platform Client Socket to handle it. The socket only works on the loop back address. */
trait ServerSocket:

  /** Opens the socket on the local loop back address at the given port. */
  def open(port: Int): Unit

  /**
   * See if there are any requests made on the port. This test does not block. If there
   * was a request, a client socket is created and will be present only directly after
   * this call in the method connection. */
  def request(): Unit

  /**
   * You may register a callback function that is called when a connection arrives.
   * This will only be called when the platform layer supports this, in which case the
   * method returns true. When false is returned, the callback function is not utilized,
   * and you need to use periodic request() + connection to obtain the new connection.*/
  def onConnect(callback: ClientSocket => Unit): Boolean

  /** Get the last available client connection. This is only readable once, after request(). */
  def connection: Option[ClientSocket]

  /** Close the this socket and, if present, the last client connection. */
  def close(): Unit

  /**
   * Contains the last error (usually due to an exception) of the last action. Should
   * also be used to test if the last action was successful. */
  def error: String
