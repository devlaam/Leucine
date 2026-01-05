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


/** This is your access controller. Only existing users are granted access. */
class Access extends RestrictActor(Access,"Access") :
  println("Access Actor Started.")

  /* Currently registered users. */
  private var store: Map[String,String] = Map.empty

  /* See if a user is present in the store, and if so the password is valid. */
  private def checkUser(name: String, password: String): Boolean = store.get(name).map(_ == password).getOrElse(false)

  /* Receive method that handles the incoming requests. */
  final protected def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = (letter,sender) match
    /* If the pair message comes from the Register actor, we store it as a new/updated user */
    case (Access.Pair(name,password),_: Register) => store += name -> password
    /* If the pair message comes from the Text Actor we must verify if the user has the correct credentials */
    case (Access.Pair(name,password),source: Text) => source ! Text.User(name,checkUser(name,password))
    /* This cannot be reached, but the compiler is not able to verify. */
    case (_,_) => assert(false,"Code should not come here.")


/** Companion object where letters and accepted sender actors are defined. We keep our state manually. */
object Access extends RestrictDefine, Stateless :

  /* We only accept letters from the Register or Text actors. */
  type Accept = Register | Text

  /* Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]

  /* A letter to send name and password to me. */
  case class Pair(name: String, password: String) extends Letter[Accept]
