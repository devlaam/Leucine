package s2a.leucine.wiki.SelectActor

// Scastie: https://scastie.scala-lang.org/devlaam/OVp4m0CCR5mRHcSFGynbCw

import java.util.Date
import s2a.leucine.actors.*
import s2a.leucine.wiki.Shared.*

given ActorContext = ActorContext.system


class Server extends SelectActor(Server,"server") :

  /* Keep all providers here */
  var providers : List[Provider] = Nil

  /* Send to the client that we have started. */
  println(s"Server Constructed")

  /* Handle all incoming letters. */
  protected def receive(letter: Letter, sender: Sender): Unit = letter match
    /* The new connection will come in as a letter with a connection name. */
    case Server.Connect(name) =>
      println(s"Accepted a connection with name: $name.")
      /* We see the providers as workers and generate automatic names for them. */
      providers = new Provider(name) :: providers
      /* Trigger each provider to action. */
      providers.foreach(_ ! Provider.Send)
    /* The request has come to close stop this server. */
    case Server.Terminate =>
      println("Server Termination Request")
      /* Stop all providers directly. */
      providers.foreach(_.stop(Actor.Stop.Direct))
      /* Stop the server actor directly. */
      stop(Actor.Stop.Direct)

  override def stopped(cause: Actor.Stop, complete: Boolean) =
    /* Decently say goodbye */
    println("Server stopped.")


object Server extends SelectDefine :
  /* Only accept messages from the outside world */
  type Accept = Anonymous
  /* Base type of all Server Letters, sealed enables the compiler to see if we handled them all. */
  sealed trait Letter extends Actor.Letter[Accept]
  /* Letter that requires a connection */
  case class Connect(name: String) extends Letter
  /* Letter that indicates the connection is over. */
  case object Terminate extends Letter


class Provider(name: String) extends SelectActor(Provider,name) :

  /* Send to the client that we are connected. */
  println(s"Provider constructed for connection $name")

  /* Handle the messages, which is only the posted letter in this case. */
  def receive(letter: Letter, sender: Sender): Unit = letter match
    case Provider.Send =>
      /* Report the new message */
      val datetime = new Date().toString
      println(s"Provider $name says: $datetime")
      /* Invalid message, uncomment to see error */
      //sender.send(Server.Terminate,this)

  /* If this actor is stopped, we must close the connection. */
  override def stopped(cause: Actor.Stop, complete: Boolean) =
    println(s"Provider stopped, goodbye $name.")


object Provider extends SelectDefine :
  /* Only accept messages from the Server */
  type Accept = Server
  /* Base type of all Server Letters, sealed enables the compiler to see if we handled them all. */
  sealed trait Letter extends Actor.Letter[Accept]
  /* Letter that tells we must send a new time message. */
  case object Send extends Letter


object Main :

  /* All messages send from here are from an Anonymous source. */
  given Actor.Anonymous = Actor.Anonymous

  /* Define the Server */
  val server = new Server

  /* Since StdIn.readLine is not supported on Scastie, we use predefined commands. */
  val commands = ConsoleReader(true,"peter","mary","joan","exit")

  /* Print a welcome message. */
  println("Demo started, type 'exit' to terminate.")

  /* Execution loop, returns if we must continue */
  def process(command: String): Boolean =
    /* See if this command is exit. */
    val exit = command == "exit"
    /* Send Terminate to the server if we need to exit,
     * or open a new connection with the given name. */
    server ! (if exit then Server.Terminate else Server.Connect(command))
    /* return if we must continue */
    !exit

  /* Loop for each command, give the app some room to handle the request. */
  def main(args: Array[String]): Unit =
    while process(commands.readln) do Thread.sleep(200)
