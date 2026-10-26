package s2a.leucine.wiki.TimingAid

// Scastie: https://scastie.scala-lang.org/devlaam/080kif2EQCG6CbjfhPwSjg

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt,SECONDS,MILLISECONDS}
import scala.concurrent.duration.FiniteDuration
import s2a.leucine.actors.*
import s2a.leucine.wiki.Shared.*

given actorContext: ActorContext = ActorContext.system

class Provider(name: String, interval: FiniteDuration) extends SelectActor(Provider,name), TimingAid :

  /* Send a message after some given time. */
  post(Provider.Send,interval):Unit

  /* Send to the client that we are connected. */
  println(s"Provider constructed for connection $name")

  /* Handle the messages, which is only the posted letter in this case. */
  def receive(letter: Letter, sender: Sender): Unit = letter match
    case Provider.Send =>
      /* Report the new message */
      val datetime = Instant.now().toString
      println(s"Provider $name says: $datetime")
      /* Send a new message after a new interval seconds. */
      post(Provider.Send,interval):Unit

  /* If this actor is stopped, we must close the connection. */
  override def stopped(cause: Actor.Stop, complete: Boolean) =
    println(s"Provider stopped, goodbye $name.")

object Provider extends SelectDefine :
  /* Only accept messages from myself, required for timing */
  type Accept = Provider
  /* Base type of all Provider Letters */
  sealed trait Letter extends Actor.Letter[Accept]
  /* Letter that tells we must send a new time message. */
  case object Send extends Letter



class Server(runtime: FiniteDuration) extends SelectActor(Server,"server"), TimingAid :

  /* Keep all providers here */
  var providers : List[Provider] = Nil

  /* Make sure this demo ends after runtime */
  post(Server.Terminate,runtime):Unit

  /* Send to the client that we have started. */
  println(s"Server Constructed, will run for $runtime.")

  /* Handle all incoming letters. */
  protected def receive(letter: Letter, sender: Sender): Unit = letter match
    /* The new connection will come in as a letter with a connection name. */
    case Server.Connect(name,interval) =>
      println(s"Accepted a connection with name: $name.")
      /* We see the providers as workers and generate automatic names for them. */
      providers = new Provider(name,interval) :: providers
      /* Since the new provider sends only messages to itself, it must be started */
      providers.head.start()
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
  /* Only accept messages from myself (required for the timing) and outside. */
  type Accept = Server | Anonymous
  /* Base type of all Server Letters */
  sealed trait Letter extends Actor.Letter[Accept]
  /* Letter that requires a connection */
  case class Connect(name: String, interval: FiniteDuration) extends Letter
  /* Letter that indicates the connection is over. */
  case object Terminate extends Letter


object Main :

  /* All messages send from here are from an Anonymous source. */
  given Actor.Anonymous = Actor.Anonymous

  /* Define the Server, demo runs for 60 seconds */
  val server = new Server(60.seconds)

  /* Commands to be used for a demo when there is no StdIn available */
  val commands = ConsoleReader(true,"peter 200","mary 100","joan 80","exit")

  println("Demo started, type 'exit' to terminate.")

 /* Analyze the user input (from Console) to see what must be done. Return if we need to continue */
  def process(cmd: String, timeUnit: TimeUnit): Boolean =
    /* Read the first part as string and the second, if available as integer. */
    cmd.split(" ") match
      case Array(cmd)       => Some(cmd)
      case Array(name,time) => time.toIntOption.map((name,_))
      case _                => None
    /* Now see what the contents of the input where */
    match
      /* The user requests a new connection */
      case Some((name:String,sec: Int)) if sec>0 => server ! Server.Connect(name,FiniteDuration(sec,timeUnit)); true
      /* The user needs help */
      case Some("help")                          => println("Type '<connection name> <time in seconds>' or 'exit'."); true
      /* The user wants to stop */
      case Some("exit")                          => server ! Server.Terminate; false
      /* In all other cases we do not understand the users wishes, so provide a way to obtain help. */
      case _                                     => println("Unknown or invalid command, type 'help'."); true

  /* Loop for each command, give the app some room to handle the request. */
  def main(args: Array[String]): Unit =
    while process(commands.readln,if commands.manual then SECONDS else MILLISECONDS) do Thread.sleep(600)
