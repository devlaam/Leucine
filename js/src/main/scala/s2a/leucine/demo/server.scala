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


/** JS platform specific implementation of the ServerSocket */
class ServerSocketImplementation extends ServerSocket:

  private var jsServer: Option[Node.Server] = None

  /* Here we keep the last error for checking. If empty, no error occurred. */
  private var _error: String = ""

  /* A possible connection is stored here. */
  private var _client: Option[ClientSocket] = None

  /* Holder for the custom callback function on connect. */
  private var callback: Option[ClientSocket => Unit] = None

  private val connect = (socket: Node.Socket) =>
    /* Store the new connection for later use */
    _client = Some(ClientSocketImplementation(socket))
    /* Call the custom callback function. */
    callback.zip(_client).foreach(_(_))

  /** Opens the socket on the local loop back address at the given port. */
  def open(port: Int): Unit =
    _error = ""
    try
      /* Create a JS Server Socket with a callback function for the connection */
      jsServer = Some(Node.Net.createServer(connect))
      /* Make sure it listens on the specified port on the localhost. */
      jsServer.map(_.listen(port,"127.0.0.1"))
    catch
      case e: Exception => _error = e.getMessage

  /**
   * See if there are any requests made on the port. This test does not block. If there
   * was a request, a client socket is created and will be present only directly after
   * this call in the method connection. In JS this request is not required and does nothing.
   * use the callback approach. */
  def request(): Unit = ()

  /**
   * You may register a callback function that is called when a connection arrives.
   * JS Supports this. */
  def onConnect(callback: ClientSocket => Unit): Boolean =
    this.callback = Some(callback)
    true

  /** Get the last available client connection. */
  def connection: Option[ClientSocket] =
    val result = _client
    _client = None
    result

  /** Close the this socket and, if present, the last client connection. */
  def close(): Unit = jsServer.map(_.close())

  /**
   * Contains the last error (usually due to an exception) of the last action. Should
   * also be used to test if the last action was successful. */
  def error = _error

