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


import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*


/* This class captures the incoming connection an creates a new actor for each. It is derived from
 * AcceptActor because we do no receive letters from others than ourselves. As a best practice we define
 * the letters Server is able to process in the companion object, and use Server.Letter as the base
 * type. Furthermore we need to mix TimingAid in for we need letters that arrive after a certain
 * period (with 'post') is over as well as the ability to wait for an i/o event (with 'expect').
 * Since this Actor spawns other other we want to automatically terminate when it stops, we make it
 * root of the family. Direct children of this actor may receive letters of the type Provider.Letter. */
class Server extends RestrictActor(Server,"server"), TimingAid, FamilyRoot(Server), LogInfo :

  /* Time this demo will last. */
  val runtime = 60.seconds

  println(s"Server Constructed, will run for $runtime.")

  /* We use different timer methods, which need different anchors so they do not overwrite each other. */
  val terminationAnchor = new Object
  val expectationAnchor = new Object

  /* Make sure this server ends after 60 seconds. It is just for testing. */
  post(Server.Terminated,runtime,terminationAnchor)

  /* First create the generalized serverSocket. This should not fail. */
  private val serverSocket: ServerSocket = new ServerSocketImplementation

  /* Now see if we can use a callback implementation. If so, this is favored for
   * it uses less resources compared to i/o polling by 'expect'. Anytime a new connection
   * arrives we send a letter to ourselves with the connection enclosed. */
  private val useCallback = serverSocket.onConnect(socket =>
    Logger.debug("Callback called.")
    send(Server.Connect(socket),Actor.Anonymous))

  /* See if there anyone knocking on the door. We need this if there is no callback
   * function on the platform available. */
  def connect: Option[Server.Letter[Server.Accept]] =
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
    stop(Actor.Stop.Direct)


  /* Handle all incoming letters. */
  protected def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
    /* The new connection will come in as a letter. */
    case Server.Connect(socket) =>
      Logger.info("Accepted a connection.")
      /* We see the providers as workers and generate automatic names for them. */
      val provider = new Provider(socket,this)
      /* Be ready for a new connection. */
      if !useCallback then expect(connect,expectationAnchor)
    /* The request has come to close stop this server. */
    case Server.Terminated =>
      Logger.info("Server Termination Request")
      /* Cancel the expectation for a new connection.
       * BTW, this is automatic in stopDirect, for illustration only. */
      dump(expectationAnchor)
      /* Stop the actor. */
      stop(Actor.Stop.Direct)

  protected override def except[Sender >: Common <: Accept](letter: Server.Letter[Sender], sender: Sender, cause: Exception, size: Int): Unit =
    Logger.warn(s"Exception Occurred: ${cause.getMessage()}")

  override def stopped(cause: Actor.Stop, complete: Boolean) =
    println("Server stopped")
    /* Decently close this socket. */
    serverSocket.close()


/* This is the natural location to define all the letters the actor may receive. */
object Server extends RestrictDefine, FamilyDefine, Stateless :
  // TODO: At the moment there are no common messages, extend!
  type FamilyAccept = Server & Provider
  type FamilyLetter[Sender <: FamilyAccept] = Nothing
  type Accept = Server | Anonymous //| Actor
  /* Base type of all Server Letters, sealed because that enables the compiler to see
   * if we handled them all. */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  /* Letter that transfers a connection */
  case class Connect(socket: ClientSocket) extends Letter[Accept]
  /* Letter that indicates the connection is over. */
  case object Terminated extends Letter[Accept]
