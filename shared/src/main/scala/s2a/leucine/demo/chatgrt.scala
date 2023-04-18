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
import scala.util.Random
import scala.io.StdIn
import s2a.leucine.actors.*



/** This is your ideal white noise char string generator. */
class Noise extends StandardActor(Noise,"Noise") :
  println("Noise Actor Started.")

  /* Constructs a string with 'length' random chars. */
  def make(length: Int): String = Random.alphanumeric.take(length).mkString

  /* Receive method that handles the incoming requests. */
  def receive[T <: Sender](letter: Noise.Letter[T], sender: T) = (letter,sender) match

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


object Noise extends StandardDefine :

  /* Only the Access and Text actors may request for random content. */
  type Accept = Register | Text

  /* Noise.Letter is the base type for all letters: */
  sealed trait Letter[T <: Accept] extends Actor.Letter[T]

  /* Letter that requests for size new random char strings. */
  case class Request(key: String, size: Int, length: Int) extends Letter[Accept]




/** This is your access controller. Only existing users are granted access. */
class Access extends StandardActor(Access,"Server") :
  println("Access Actor Started.")

  /* Currently registered users. */
  var store: Map[String,String] = Map.empty

  def checkUser(name: String, password: String) = store.get(name).map(_ == password).getOrElse(false)

  def receive[T <: Sender](letter: Access.Letter[T], sender: T) = (letter,sender) match

    /* If the pair message comes from the Register actor, we store it as a new/updated user */
    case (Access.Pair(name,password),source: Register) => store += name -> password

    /* If the pair message comes from the Text Actor we must verify if the user has the correct credentials */
    case (Access.Pair(name,password),source: Text) => source ! Text.User(name,checkUser(name,password))

    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")


object Access extends StandardDefine :
  type Accept = Register | Text

  /* Logger.Letter is the base type for all letters: */
  sealed trait Letter[T <: Accept] extends Actor.Letter[T]

  case class Pair(name: String, password: String) extends Letter[Accept]




class Text(access: Access, noise: Noise) extends StandardActor(Text,"Text") :
  println("Text Actor Started.")

  def receive[T <: Sender](letter: Text.Letter[T], sender: T) = (letter,sender) match

    case (Text.Lipsum(name,password),source: Anonymous) => access ! Access.Pair(name,password)
    case (Text.Lipsum(name,text),source: Noise)         => println(s"Some text for $name: $text")
    case (Text.User(name,allow),source: Access)         => if allow then noise ! Noise.Request(name,20,6) else println(s"User $name refused.")
    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")

object Text  extends StandardDefine :

  type Accept = Access | Noise | Anonymous

  /* Logger.Letter is the base type for all letters: */
  sealed trait Letter[T <: Accept] extends Actor.Letter[T]

  /* Letter that contains information about the user, content is sender related. */
  case class Lipsum(name: String, content: String) extends Letter[Noise | Anonymous]

  /* Letter that verifies if the user was allowed. */
  case class User(name: String, allow: Boolean) extends Letter[Access]





class Register(access: Access, noise: Noise) extends StandardActor(Register,"Register") :
  println("Register Actor Started.")

  /* Supply of new passwords */
  var supply: List[String] = Nil


  def receive[T <: Sender](letter: Register.Letter[T], sender: T) = (letter,sender) match
    case (Register.Passwords(values),source: Noise) => supply = values

    case (Register.Request(name),source: Anonymous) =>
      if supply.isEmpty
      then
        println(s"Sorry $name, at the moment we do not accept new accounts, please try again later.")
        noise ! Noise.Request("",3,4)
      else
        println(s"Welcome $name, you may now use this service with the password: ${supply.head}")
        access ! Access.Pair(name,supply.head)
        supply = supply.tail

    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")

object Register extends StandardDefine :

  type Accept = Noise | Anonymous

  /* Logger.Letter is the base type for all letters: */
  sealed trait Letter[T <: Accept] extends Actor.Letter[T]

  /* Letter that contains new passwords. */
  case class Passwords(values: List[String]) extends Letter[Noise]

  /* Letter requests a (new) password for the user. */
  case class Request(name: String) extends Letter[Anonymous]


object Chatgrt :
  given Actor.Anonymous = Actor.Anonymous

  /* Define the loop actors */
  val noise    = new Noise
  val access   = new Access
  val register = new Register(access,noise)
  val text     = new Text(access,noise)

  def stop(): Unit =
    List(noise,access,register,text).foreach(_.stop(Actor.Stop.Direct))
    println("ChatGRT stopped.")

  def request(action: String => Unit): Unit = CLI.talk("ChatGRT ready, your command: ", action)

  def process(cmd: String)(using Actor): Unit = cmd.split(" ") match
    case Array("signup",name)    => register ! Register.Request(name)
    case Array("text",name,pass) => text ! Text.Lipsum(name,pass)
    case Array("help")           => println("Type 'signup <name>' or 'text <name> <password>' or 'exit'.")
    case _                       => println("Unknown command, type 'help'.")

