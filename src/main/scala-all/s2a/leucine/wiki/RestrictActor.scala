package s2a.leucine.wiki.RestrictActor

// Scastie: https://scastie.scala-lang.org/devlaam/ArK57VpNRj2i9PKfQ5vdmg/1

import scala.util.Random
import s2a.leucine.actors.*
import s2a.leucine.wiki.Shared.*


given actorContext: ActorContext = ActorContext.system


class Text(access: Access, noise: Noise) extends RestrictActor(Text,"Text") :
  println("Text Actor Started.")

  /* Receive method that handles the incoming requests. */
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = (letter,sender) match
    /* If we receive this letter from an anonymous source, we interpret it as a request for a new password. */
    case (Text.Lipsum(name,password), _: Anonymous) => access ! Access.Pair(name,password)
    /* We received a the verdict from the Access actor if we may allow the user. If not, communicate this, otherwise generate new random text. */
    case (Text.User(name,allow), _: Access)         => if allow then noise ! Noise.Request(name,20,6) else println(s"User $name refused.")
    /* If we receive this letter from the Noise actor, we interpret it as random text */
    case (Text.Lipsum(name,text), _: Noise)         => println(s"Some text for $name: $text")
    /* This cannot be reached, but some versions of the compiler are not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")

/** Companion object where letters and accepted sender actors are defined. We keep no state. */
object Text extends RestrictDefine :
  /* We only accept letters from the Noise or Access actors or from an anonymous source. */
  type Accept = Access | Noise | Anonymous
  /* Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  /* Letter that contains information about the user, content is sender related. */
  case class Lipsum(name: String, content: String) extends Letter[Noise | Anonymous]
  /* Letter that verifies if the user was allowed. */
  case class User(name: String, allow: Boolean) extends Letter[Access]


/** Service that keeps a list of new passwords to be handed out. Starts empty */
class Register(access: Access, noise: Noise) extends RestrictActor(Register,"Register") :
  println("Register Actor Started.")

  /* Supply of new passwords */
  var supply: List[String] = Nil

  /* Receive method that handles the incoming requests. */
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Receive = (letter,sender) match
    /* A request for new passwords is made. */
    case (Register.Request(name), _: Anonymous) =>
      /* It may be that we have no passwords left, test this first. */
      if supply.isEmpty
      then
        /* If so, let the user know we do not accept users at the moment */
        println(s"Sorry $name, at the moment we do not accept new accounts, please try again later.")
        /* and at the same time order new passwords. */
        noise ! Noise.Request("",3,4)
      else
        /* If we have passwords available, let the user know the new password. */
        println(s"Welcome $name, you may now use this service with the password: ${supply.head}")
        /* Signal the Access manager that a new user has been welcomed. */
        access ! Access.Pair(name,supply.head)
        /* Remove the password from the list, so it is not issued again. */
        supply = supply.tail
    /* In case new passwords arrive, store them in the supply */
    case (Register.Passwords(values), _: Noise) => supply = values
    /* This cannot be reached, but some versions of the compiler are not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")

/** Companion object where letters and accepted sender actors are defined. We keep state manually. */
object Register extends RestrictDefine :
  /* We only accept letters from the Noise actor or from an anonymous source. */
  type Accept = Noise | Anonymous
  /* Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  /* Letter that contains new passwords. */
  case class Passwords(values: List[String]) extends Letter[Noise]
  /* Letter requests a (new) password for the user. */
  case class Request(name: String) extends Letter[Anonymous]


/** This is your ideal white noise char string generator. */
class Noise extends RestrictActor(Noise,"Noise") :
  println("Noise Actor Started.")

  /* Constructs a string with 'length' random chars. */
  def make(length: Int): String = Random.alphanumeric.take(length).mkString

  /* Receive method that handles the incoming requests. */
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = (letter,sender) match
    /* Return a sequence of size random strings each of the same length. */
    case (Noise.Request(_,size,length), source: Register) =>
      val result = List.fill(size)(make(length))
      source ! Register.Passwords(result)
    /* Return a random piece of text of 'size' number of words, each not longer than 'length' chars. */
    case (Noise.Request(key,size,length), source: Text) =>
      val result = List.fill(size)(make(Random.nextInt(length)+1)).mkString(" ")
      source ! Text.Lipsum(key,result)
    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")


/** Companion object where letters and accepted sender actors are defined. We keep no state. */
object Noise extends RestrictDefine :
  /* Only the Access and Text actors may request for random content. */
  type Accept = Register | Text
  /* Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  /* Letter that requests for size new random char strings. */
  case class Request(key: String, size: Int, length: Int) extends Letter[Accept]

/** This is your access controller. Only existing users are granted access. */
class Access extends RestrictActor(Access,"Access") :
  println("Access Actor Started.")

  /* Currently registered users. */
  var store: Map[String,String] = Map.empty

  /* See if a user is present in the store, and if so the password is valid. */
  def checkUser(name: String, password: String): Boolean = store.get(name).map(_ == password).getOrElse(false)

  /* Receive method that handles the incoming requests. */
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = (letter,sender) match
    /* If the pair message comes from the Register actor, we store it as a new/updated user */
    case (Access.Pair(name,password),_: Register) => store += name -> password
    /* If the pair message comes from the Text Actor we must verify if the user has the correct credentials */
    case (Access.Pair(name,password),source: Text) => source ! Text.User(name,checkUser(name,password))
    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")

/** Companion object where letters and accepted sender actors are defined. We keep state manually. */
object Access extends RestrictDefine :
  /* We only accept letters from the Register or Text actors. */
  type Accept = Register | Text
  /* Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  /* A letter to send name and password to me. */
  case class Pair(name: String, password: String) extends Letter[Accept]


/* Main entry point for the ChatGRT demo. */
object Main :
  /* We must provide a default sender. */
  given Actor.Anonymous = Actor.Anonymous
  /* Service that generates a random character words. */
  private val noise    = new Noise
  /* Service that keeps a list of the approved accounts. */
  private val access   = new Access
  /* Service that keeps a list of new passwords to be handed out. */
  private val register = new Register(access,noise)
  /* Service that constructs random texts. */
  private val text     = new Text(access,noise)

  /* Stop all actors */
  private def stop(): Unit = List(noise,access,register,text).foreach(_.stop(Actor.Stop.Direct))

  /* Commands to be used for a demo when there is no StdIn available */
  val commands = ConsoleReader(true,"signup ruud","help","signup ruud","signup peter","text ruud 1234","exit")

  /* Analyze the user input (from Console) to see what must be done. Return if we need to continue */
  def process(cmd: String): Boolean = cmd.split(" ") match
    /* The user request for a new account */
    case Array("signup",name)    => register ! Register.Request(name); true
    /* The user requires some text */
    case Array("text",name,pass) => text ! Text.Lipsum(name,pass); true
    /* The user needs help */
    case Array("help")           => println("Type 'signup <name>' or 'text <name> <password>' or 'exit'."); true
    /* The user wants to stop */
    case Array("exit")           => stop(); println("ChatGRT stops."); false
    /* In all other cases we do not understand the users wishes, so provide a way to obtain help. */
    case _                       => println("Unknown command, type 'help'."); true

  /* Loop for each command, give the app some room to handle the request. */
  def main(args: Array[String]): Unit =
    while process(commands.readln) do Thread.sleep(200)
