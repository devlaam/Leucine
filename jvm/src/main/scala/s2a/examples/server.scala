package s2a.examples

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

import java.net.InetSocketAddress
import java.nio.channels.{ServerSocketChannel, SocketChannel, Selector, SelectionKey}
import java.nio.ByteBuffer

/** JVM platform specific implementation of the ServerSocket */
class ServerSocketImplementation extends ServerSocket:
  import ServerSocketImplementation.ServerChannel

  /* We use the java nio Channels here inside the ServerChannel object */
  private var serverChannel: Option[ServerChannel] = None

  /* Here we keep the last error for checking. If empty, no error occurred. */
  private var _error: String = ""

  /* A possible connection is stored here. */
  private var _client: Option[ClientSocket] = None

  /** Opens the socket on the local loopback address at the given port. */
  def open(port: Int): Unit =
    _error = ""
    try
      serverChannel = Some(ServerChannel(port))
    catch
      case e: Exception => _error = e.getMessage

  /**
   * See if there are any requests made on the port. This test does not block. If there
   * was a request, a client socket is created and will be present only directly after
   * this call in the method connection. */
  def request(): Unit = serverChannel.foreach(sc =>
    try
      _error  = ""
      _client = sc.nextConnection
    catch
      case e: Exception =>
        _error = e.getMessage
        _client = None )

  /**
   * You may register a callback function that is called when a connection arrives.
   * The current implementation on JVM does not support this. */
  def onConnect(callback: ClientSocket => Unit): Boolean = false

  /** Get the last available client connection. */
  def connection: Option[ClientSocket] =
    val result = _client
    _client = None
    result

  /** Close the this socket and, if present, the last client connection. */
  def close(): Unit = serverChannel.foreach(_.close())


  /**
   * Contains the last error (usually due to an exception) of the last action. Should
   * also be used to test if the last action was successful. */
  def error = _error


object ServerSocketImplementation :
  /* The class ServerChannel implements the java nio channel machinery for non blocking connections.
   * This has to be bent a little so we can distribute the channels over the actors. */
  private class ServerChannel(val port: Int) :

    private val selector = Selector.open()
    private val serverSocketChannel: ServerSocketChannel = ServerSocketChannel.open()
    serverSocketChannel.configureBlocking(false)
    serverSocketChannel.bind(new InetSocketAddress("localhost", port))
    serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT)

    private def nextKey: Option[SelectionKey] =
      if selector.selectNow() <= 0 then None else
        val iterator = selector.selectedKeys().iterator()
        val key: SelectionKey = iterator.next()
        iterator.remove()
        Some(key)

    private def tryAccept(key: SelectionKey): Option[SelectionKey] =
      if !key.isAcceptable() then Some(key) else
        val sc: SocketChannel = serverSocketChannel.accept()
        sc.configureBlocking(false);
        sc.register(selector, SelectionKey.OP_WRITE)
        None

    private def tryConnect(key: SelectionKey): Option[ClientSocket] =
      if !key.isWritable() then None else
        val sc: SocketChannel = key.channel().asInstanceOf[SocketChannel]
        sc.register(selector, 0)
        Some(ClientSocketImplementation(sc))

    def nextConnection: Option[ClientSocket] =
      for
      key    <- nextKey
      accept <- tryAccept(key)
      socket <- tryConnect(accept)
      yield socket

    def close(): Unit =
      serverSocketChannel.socket().close()
      serverSocketChannel.close()
      selector.close()



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

