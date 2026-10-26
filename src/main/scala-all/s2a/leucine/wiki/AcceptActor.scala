package s2a.leucine.wiki.AcceptActor

// Scastie: https://scastie.scala-lang.org/devlaam/2Gl6kXHeQ86N93Bip5mIyA/5

import scala.concurrent.duration.DurationInt
import s2a.leucine.actors.*

given actorContext: ActorContext = ActorContext.system


class Printer extends AcceptActor(Printer,"printer") :
  import Printer.*

  /* Report that this logger has started. */
  println("Started Printer Actor")

  /* Device to which the messages are printed. Can be changed by during runtime. */
  private var device: Device = Device.ANSI

  /* Receive method that handles the incoming printer and control messages. */
  def receive(letter: Letter, sender: Sender): Unit = letter match
    case Message(colour,text)  => println(format(colour,device,text))
    case Switch(device)        => this.device = device


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
  def format(colour: Colour, device: Device, text: String) = device match
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


object Main :

  /* Define the printer */
  val printer = new Printer

  /* And make use of it like this: */
  def main(args: Array[String]): Unit =
    printer ! Printer.Message(Printer.Colour.Red,"This should be in red.")
    printer ! Printer.Switch(Printer.Device.XML)
    printer ! Printer.Message(Printer.Colour.Green,"Now it should be xml green.")
    printer.stop(Actor.Stop.Finish)
    ActorGuard.watch(false,1.second)
