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
