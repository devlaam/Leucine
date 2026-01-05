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


/* Main entry point for the ChatGRT demo. */
object Chatgrt :
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

  /* Stop this demo */
  def stop(): Unit =
    /* Tell all actors to stop directly */
    List(noise,access,register,text).foreach(_.stop(Actor.Stop.Direct))
    /* Tell the user we did stop the actors. */
    println("ChatGRT stopped.")

  /* Method to get input from the user. With CLI thus may block (JVM/Native) */
  def request(action: String => Unit): Unit = CLI.talk("ChatGRT ready, your command: ", action)

  /* Analyze the user input (from Console) to see what must be done. */
  def process(cmd: String): Unit = cmd.split(" ") match
    /* The user request for a new account */
    case Array("signup",name)    => register ! Register.Request(name)
    /* The user requires some text */
    case Array("text",name,pass) => text ! Text.Lipsum(name,pass)
    /* The user needs help */
    case Array("help")           => println("Type 'signup <name>' or 'text <name> <password>' or 'exit'.")
    /* In all other cases we do not understand the users wishes, so provide a way to obtain help. */
    case _                       => println("Unknown command, type 'help'.")
