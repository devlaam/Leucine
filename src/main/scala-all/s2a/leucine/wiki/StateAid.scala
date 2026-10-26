package s2a.leucine.wiki.StateAid

// Scastie: https://scastie.scala-lang.org/devlaam/HW3MzsTHTquJGfipojeFRw

import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*

given actorContext: ActorContext = ActorContext.system


class Printer extends AcceptActor(Printer,"printer"), StateAid[Printer.State](Printer.initial):
  import Printer.*

  /* Report that this logger has started. */
  println("Started Printer Actor")

  /* Receive method that handles the incoming printer and control messages, it must return the new state */
  def receive(letter: Letter, sender: Sender): Receive = letter match
    case Message(colour,text)  => println(format(colour,state.device,text)); state.printed
    case Switch(device)        => state.change(device)
    case Prints                => println(state.show); state


object Printer extends AcceptDefine :

  /* These are the devices the printer is able to handle. */
  enum Device :
    case ANSI, XML, HTML

  /* These are the colors the printer is able to handle. */
  enum Colour(val ansi: Int, val style: String) :
    case Red   extends Colour(31,"red")
    case Green extends Colour(32,"green")
    case Blue  extends Colour(34,"blue")

  /* Format the text according to the parameters for output. */
  def format(colour: Colour, device: Device, text: String): String = device match
      case Device.ANSI  => s"\u001B[${colour.ansi}m${text}\u001B[0m"
      case Device.XML   => s"<${colour.style}>${text}</${colour.style}>"
      case Device.HTML  => s"<span style=\"color:${colour.style};\">${text}</span>"

  /* Make this class sealed so that the compile can check at the receiver method if
   * we were complete in the implementation of all message types */
  sealed trait Letter extends Actor.Letter[Actor]

  /* Class to send a colored message to the printer. */
  case class Message(colour: Colour, text: String) extends Letter

  /* Message to dynamically switch the device of the messages. */
  case class Switch(device: Device) extends Letter

  /* Message to show the number of sheets printed. */
  case object Prints extends Letter

  /* The State holds two parameters, the number of prints until now
   * and the current active device. */
  case class State(device: Device, prints: Int) extends Actor.State :
    /* Signal that we have printed a text. */
    def printed: State = copy(prints = prints + 1)
    /* Change the printer device */
    def change(device: Device): State = copy(device = device)
    /* Tell me how many prints were made. */
    def show: String = s"Printed $prints times."

  /* Start printing with an ANSI device, at zero prints. */
  def initial: State = State(Device.ANSI,0)


object Main :
  import Printer.*

  /* Define the printer */
  val printer = new Printer

  /* And make use of it like this: */
  def main(args: Array[String]): Unit =
    printer ! Message(Colour.Red,"This should be red.")
    printer ! Message(Colour.Blue,"This should be blue.")
    printer ! Switch(Device.XML)
    printer ! Message(Colour.Green,"Now it should be xml green.")
    printer ! Switch(Device.HTML)
    printer ! Message(Colour.Red,"Red formatted as HTML.")
    printer ! Prints
    printer.stop(Actor.Stop.Finish)
    ActorGuard.watch(false,1.second)
