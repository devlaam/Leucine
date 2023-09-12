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

import java.net.{ServerSocket => JavaServerSocket, InetAddress, SocketTimeoutException }


/** Native platform specific implementation of the ServerSocket */
class ServerSocketImplementation extends ServerSocket:

  /* Note: Normally we would not use the blocking ServerSocket but ServerSocketChannel from the java.nio libs. However,
   * this is not supported under Scala Native yet, and besides, this code is just to for illustration purposes. */
  private var javaServerSocket: Option[JavaServerSocket] = None

  /* Here we keep the last error for checking. If empty, no error occurred. */
  private var _error: String = ""

  /* A possible connection is stored here. */
  private var _client: Option[ClientSocket] = None

  /** Opens the socket on the local loop back address at the given port. */
  def open(port: Int): Unit =
    _error = ""
    try
      /* Create a Java Server Socket that listens on the specified with a queue length of 10
       * connections on the localhost. */
      javaServerSocket = Some(new JavaServerSocket(port,10,InetAddress.getLoopbackAddress()))
      /* The Java ServerSocket is blocking, but we do not want that.
       * So we set a timeout to prevent endless waiting. */
      javaServerSocket.foreach(_.setSoTimeout(2))
    catch
      case e: Exception => _error = e.getMessage

  /**
   * See if there are any requests made on the port. This test does not block. If there
   * was a request, a client socket is created and will be present only directly after
   * this call in the method connection. Note: Calling this is required under Native, since
   * the java.net.ServerSocket does not implement its own callback. */
  def request(): Unit =
    _error = ""
    try
      _client = javaServerSocket.map(_.accept()).map(ClientSocketImplementation(_))
    catch
      /* If we reach a timeout exception there was no one knocking on the door, we may retry */
      case e: SocketTimeoutException => _client = None
      /* Any other exception we cannot handle, so this connection is over. */
      case e: Exception => _error = e.getMessage; _client = None

  /**
   * You may register a callback function that is called when a connection arrives.
   * Native does not support this. */
  def onConnect(callback: ClientSocket => Unit): Boolean = false

  /** Get the last available client connection. */
  def connection: Option[ClientSocket] =
    val result = _client
    _client = None
    result


  /** Close the this socket and, if present, the last client connection. */
  def close(): Unit =
    javaServerSocket.foreach(_.close())
    _client.foreach(_.close())

  /**
   * Contains the last error (usually due to an exception) of the last action. Should
   * also be used to test if the last action was successful. */
  def error = _error
