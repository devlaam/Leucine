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


class Register(access: Access, noise: Noise) extends RestrictActor(Register,"Register") :
  println("Register Actor Started.")

  /* Supply of new passwords */
  var supply: List[String] = Nil

  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = (letter,sender) match
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


object Register extends RestrictDefine, Stateless :

  type Accept = Noise | Anonymous

  /* Logger.Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]

  /* Letter that contains new passwords. */
  case class Passwords(values: List[String]) extends Letter[Noise]

  /* Letter requests a (new) password for the user. */
  case class Request(name: String) extends Letter[Anonymous]

