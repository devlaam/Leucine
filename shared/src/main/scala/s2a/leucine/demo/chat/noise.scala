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

import scala.util.Random
import s2a.leucine.actors.*


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
object Noise extends RestrictDefine, Stateless :

  /* Only the Access and Text actors may request for random content. */
  type Accept = Register | Text

  /* Noise.Letter is the base type for all letters: */
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]

  /* Letter that requests for size new random char strings. */
  case class Request(key: String, size: Int, length: Int) extends Letter[Accept]
