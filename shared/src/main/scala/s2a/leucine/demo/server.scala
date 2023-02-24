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

import java.util.Date

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

import s2a.leucine.actors.*
import s2a.leucine.extensions.*


/* Sockets are not identical over all platforms. Therefore we make a minimal abstraction for a
 * local server and client socket connection. For illustration purposes only. This is NOT the
 * best way to do it, but it is short and clear. */

/**
 * Trans platform Server Socket that is able to accept a connection on a specified port and create
 * a trans platform Client Socket to handle it. The socket only works on the loopback address. */
trait ServerSocket:

  /** Opens the socket on the local loopback address at the given port. */
  def open(port: Int): Unit

  /**
   * See if there are any requests made on the port. This test does not block. If there
   * was a request, a client socket is created and will be present only directly after
   * this call in the method connection. */
  def request(): Unit

  /**
   * You may register a callback function that is called when a connection arrives.
   * This will only be called when the platform layer supports this, in which case the
   * method returns true. When false is returned, the callback function is not untilized,
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


/** Transplatform Client Socket for communication with an other socket. */
trait ClientSocket() :

  /** Obtain the port number of the connection on this side. */
  def localPort: Int

  /** Obtain the port number of the connection on the other side. */
  def remotePort: Int

  /** Write (and flush) some text to the socket. */
  def writeln(text: String): Unit

  /** Read some text from the socket (up to the newline) */
  def readln: String

  /** Close this socket */
  def close(): Unit


/* This class captures the incomming connection an creates a new actor for each. It is derived from
 * BasicActor because we do no receive letters from others than ourselves. As a best practice we define
 * the letters Server is able to process in the compagnion object, and use Server.Letter as the base
 * type. Furthermore we need to mix TimingActor in for we need letters that arrive after a certain
 * period (with 'post') is over as well as the ability to wait for an i/o event (with 'expect').
 * Since this Actor spawns other other we want to automatically terminate when it stops, we make it
 * root of the family. Direct children of this actor may receive letters of the type Provider.Letter. */
class Server extends BasicActor[Server.Letter], TimingActor, FamilyRoot[Provider.Letter], LogInfo :

  /* There is only one 'Server' so we may fix the name here. */
  val name = "server"

  /* Time this demo will last. */
  val runtime = 60.seconds

  println(s"Server Constructed, will run for $runtime.")

  /* We use different timer methods, which need different anchors so they do not overwrite each other. */
  val terminationAnchor = new Object
  val expectationAnchor = new Object

  /* Make sure this server ends after 60 seconds. It is just for testing. */
  post(Server.Terminated,runtime,terminationAnchor)

  /* First create the generiziled serverSocket. This should not fail. */
  private val serverSocket: ServerSocket = new ServerSocketImplementation

  /* Now see if we can use a callback implementation. If so, this is favoured for
   * it uses less resources compated to io polling by expect. Anytime a new connection
   * arrives we send a letter to ourselves with the connection enclosed. */
  private val useCallback = serverSocket.onConnect(socket =>
    Logger.debug("Callback called.")
    send(Server.Connect(socket)))

  /* See if there anyone knocking on the door. We need this if there is no callback
   * function on the platform available. */
  def connect: Option[Server.Letter] =
    /* Test if we have a request for a connection. */
    serverSocket.request()
    /* This may result in an error, if ... */
    if serverSocket.error.isEmpty
    /* ... not map the connection, if any on a letter */
    then serverSocket.connection.map(Server.Connect(_))
    /* ... so then */
    else
      /* ... report the problem */
      Logger.warn(s"Exception on ServerSocket: ${serverSocket.error}")
      /* ... stop the server by sending the termination letter. */
      Some(Server.Terminated)

  /* Try to open the socket on port 8180.  */
  serverSocket.open(8180)

  /* See if the port was successfully opened ... */
  if serverSocket.error.isEmpty
  then
    /* ... if so report this */
    println("ServerSocket Open on port 8180, try to make a connection.")
    /* ... and wait for the first connection, if needed */
    if !useCallback then expect(connect,expectationAnchor)
  else
    /* ... if not report this */
    Logger.warn(s"ServerSocket cannot be opened: ${serverSocket.error}")
    /* ... and stop the server. */
    stopDirect()


  /* Handle all incomming letters. */
  protected def receive(letter: Server.Letter): Unit = letter match
    /* The new connection will come in as a letter. */
    case Server.Connect(socket) =>
      Logger.info("Accepted a connection.")
      /* We see the providers as workers and generate automatic names for them. */
      val provider = new Provider(workerName,socket,this)
      /* Integrate this provider into the family */
      adopt(provider)
      /* Be ready for a new connection. */
      if !useCallback then expect(connect,expectationAnchor)
    /* The request has come to close stop this server. */
    case Server.Terminated =>
      Logger.info("Server Termination Request")
      /* Cancel the expection for a new connection.
       * BTW, this is automatic in stopDirect, for illustration only. */
      dump(expectationAnchor)
      /* Stop the actor. */
      stopDirect()

  protected override def except(letter: Server.Letter, cause: Exception, size: Int): Unit =
    Logger.warn(s"Exception Occurred: ${cause.getMessage()}")

  override def stopped(complete: Boolean) =
    println("Server stopped")
    /* Decently close this socket. */
    serverSocket.close()


/* This is the natural location to define all the letters the actor may receive. */
object Server :
  /* Base type of all Server Letters, sealed because that enables the compiler to see
   * if we handled them all. */
  sealed trait Letter extends Actor.Letter
  /* Letter that transfers a connection */
  case class Connect(socket: ClientSocket) extends Letter
  /* Letter that indicates the connection is over. */
  case object Terminated extends Letter



/* The provider class sends a timestamp to the connection every two seconds. Since it isn part of a family we must define the
 * parent actor in its declaration. Further, it cannot have a constant name, so that is a parameter too. Lastly since the
 * socket is also fixed during its livetime, this is also made a parameter. An other option would have been to send it in
 * a letter. The StandardActor is used as a base actor, but since we do not realy care who send the messages to the provider,
 * we could have chosen the BasicActor as well. This actor is part of a family but does not have childeren of its own. So
 * we mixin the FamilyLeaf, which requires specifying the parent actor type. We could also have chosen for FamilyBranch, and
 * simply ignoring the childeren. But less is more. */
class Provider(val name: String, protected val socket: ClientSocket, protected val parent: Server) extends StandardActor[Provider.Letter], TimingActor, FamilyLeaf[Server], LogInfo :

  Logger.info(s"Provider Constructed, local=${socket.localPort}, remote=${socket.remotePort}")
  /* Send to the client that we are connected. The path is the full name of this actor. */
  socket.writeln(s"Provider $path Connected.")

  /* Send a message after two seconds. */
  post(Provider.Send,2.seconds)

  /* Handle the messages, which is only the posted letter in this case. */
  def receive(letter: Provider.Letter, sender: Sender): Unit = letter match
    case Provider.Send =>
      val datetime = new Date().toString
      val message  = s"Provider $path says: $datetime"
      Logger.info(message)
      socket.writeln(message)
      /* Send a new message after two seconds. */
      post(Provider.Send,2.seconds)

  /* If this actor is stopped, we must close the connection. */
  override def stopped(complete: Boolean) =
    println(s"Provider $path stopped.")
    socket.close()

object Provider :
  sealed trait Letter extends Actor.Letter
  case object Send extends Letter







