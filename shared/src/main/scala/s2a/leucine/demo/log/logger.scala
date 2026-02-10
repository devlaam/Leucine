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


/**
 * The Printer will only receive messages and never send one. Also, we do not
 * care from which actor the message is originating. */
private class Printer extends AcceptActor(Printer,"printer") :
  import Printer.*
  Logger.trace(Logger.GroupTicker)

  /* Soft colour of the messages that are printed. Can be changed by during runtime. */
  private var device: Device = Device.XML

  /* Report that this printer has been disabled. */
  final protected override def stopped(cause: Actor.Stop, complete: Boolean) = println("Stopped Printer")

  /* Report that this printer has started. */
  println("Started Printer")

  /* receive method that handles the incoming printer and control messages. */
  final protected def receive(letter: Letter, sender: Sender): Unit =
    Logger.trace(Logger.GroupTicker)
    letter match
      case Message(colour,text)  => println(colour.format(device,text))
      case Switch(device)        => this.device = device


object Printer extends AcceptDefine, Stateless:
  import Auxiliary.toUnit
  Logger.trace(Logger.GroupTicker)

  /* This are the devices the printer is able to handle. */
  enum Device :
    case ANSI, XML, HTML

  /* This are the colors the printer is able to handle. */
  sealed trait Colour :
    /* Definition to format a piece of text according to the selected color. */
    def format(device: Device, text: String): String
    /* Implementation to format a piece of text according to the selected color. */
    protected def sub(device: Device, text: String, ansi: Int, xml: String ): String = device match
      case Device.ANSI  => s"\u001B[${ansi}m${text}\u001B[0m"
      case Device.XML   => s"<${xml}>${text}</${xml}>"
      case Device.HTML  => s"<span style=\"color:${xml};\">${text}</span>"

  /* This are the colors the printer is able to handle. */
  object Red     extends Colour { def format(device: Device, text: String): String = sub(device,text,31,"red") }
  object Blue    extends Colour { def format(device: Device, text: String): String = sub(device,text,34,"blue") }
  object Green   extends Colour { def format(device: Device, text: String): String = sub(device,text,32,"green") }
  object Default extends Colour { def format(device: Device, text: String): String = text}

  /* Make this class sealed so that the compile can check at the receiver method if
   * we were complete in the implementation of all message types */
  sealed trait Letter extends Actor.Letter[Actor]

  /* Class to send a record message to the printer. */
  private case class Message(colour: Colour, text: String) extends Letter :
    Logger.trace(Logger.GroupTicker)
    /* Make a reasonable entry for this record. This is done in the Printer Actor context. */
    def show: String = s"The $colour says: \"$text\""

  /* Message to dynamically switch the device of the messages. */
  private case class Switch(device: Device) extends Letter

  /* The actual printer is hidden from the user. */
  private val printer = new Printer

  /* The printer may stop if there is nothing left to record. */
  printer.stop(Actor.Stop.Final)

  /* Below are the methods that you use in your program. So for the user these feel just like
   * ordinary record calls. Under the hood they are handled by the actor so that the normal flow
   * of the program can continue.  */

  /** Colour to report important information, to be addressed by the administrator. */
  def red(text: => String): Unit =
    Logger.trace(Logger.GroupTicker)
    printer.send(Message(Red,text)).toUnit

  /** Colour to report important information, to be addressed by the operator. */
  def blue(text: => String): Unit =
    Logger.trace(Logger.GroupTicker)
    printer.send(Message(Blue,text)).toUnit

  /** Colour to report regular use. */
  def green(text: => String): Unit =
    Logger.trace(Logger.GroupTicker)
    printer.send(Message(Green,text)).toUnit

  /** Colour to report all other less important stuff. */
  def default(text: => String): Unit =
    Logger.trace(Logger.GroupTicker)
    printer.send(Message(Default,text)).toUnit

  /** Change the record colour */
  def switch(device: Device): Unit =
    Logger.trace(Logger.GroupTicker)
    printer.send(Switch(device)).toUnit
