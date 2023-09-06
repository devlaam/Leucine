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

import s2a.leucine.actors.*


/** Service that keeps a list of new passwords to be handed out. Starts empty */
class Register(access: Access, noise: Noise) extends RestrictActor(Register,"Register") :
  println("Register Actor Started.")

  /* Supply of new passwords */
  private var supply: List[String] = Nil

  /* Receive method that handles the incoming requests. */
  final protected def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = (letter,sender) match
    /* In case new passwords arrive, store them in the supply */
    case (Register.Passwords(values),source: Noise) => supply = values
    /* A request for new passwords is made. */
    case (Register.Request(name),source: Anonymous) =>
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
    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")


/** Companion object where letters and accepted sender actors are defined. We keep our state manually. */
object Register extends RestrictDefine, Stateless :

  /* We only accept letters from the Noise actor or from an anonymous source. */
  type Accept = Noise | Anonymous

  /* Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]

  /* Letter that contains new passwords. */
  case class Passwords(values: List[String]) extends Letter[Noise]

  /* Letter requests a (new) password for the user. */
  case class Request(name: String) extends Letter[Anonymous]
