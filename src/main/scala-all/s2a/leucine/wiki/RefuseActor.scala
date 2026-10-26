package s2a.leucine.wiki.RefuseActor

// To be added on Scastie, when issue #50 is done.

import scala.collection.immutable.SortedMap
import s2a.leucine.actors.*
import s2a.leucine.wiki.Shared.*

given ActorContext = ActorContext.system

/** This actor tests if a number is prime. It simulates a number cruncher. */
class Primer(number: Int, server: Server) extends RefuseActor() :

  /* There is no need to test beyond the square root for divisors. */
  private val limit = Math.sqrt(number)

  /* In started (also executed in a separate thread) we can test some conditions
   * about our number crunching problem. Here, if the number is even, there is
   * no need to start at all. */
  final override def started(): Unit =
    /* Report what we are doing */
    println(s"Primer test 2 on $number.")
    /* Test if the number is even */
    if number % 2 == 0 then
      /* If so report the number is not prime */
      server ! Server.Result(number,false)
      /* Stop this actor now. */
      stop(Actor.Stop.Finish)

  /* This is the location to put your number crunching. The loop is repeatedly
   * called with increasing loops counter (starting at zero) until you manually
   * terminate the process. */
  final override def process(loops: Long): Unit =
    /* Test only the odd number for division. */
    val div = 2*(loops + 1) + 1
    /* Report what we are doing */
    println(s"Primer test $div on $number.")
    /* If we have reached the limit this number is prime ... */
    if  div > limit then
      /* Let the server know. */
      server ! Server.Result(number,true)
      /* We are done. */
      stop(Actor.Stop.Finish)
    /* ... if not, we test the divisibility */
    else if number % div == 0 then
      /* if it divides this is not prime, let the server know */
      server ! Server.Result(number,false)
      /* We are done. */
      stop(Actor.Stop.Finish)
    /* wait a while for the next round Note: never do this in your application, it
     * keeps a thread occupied. If you need to pause, work with timers, but normally
     * there is no need, other calculations are interleaved on each round by the
     * scheduler. */
    Thread.sleep(100)

  /* Called when the calculations are over. */
  override def stopped(cause: Actor.Stop, complete: Boolean): Unit =
    /* We call Actor.Stop.Finish on a regular termination and Actor.Stop.Direct as
     * interruption. This is best practice only. Both have exactly the same effect,
     * but this allows us to see the cause. */
    if cause == Actor.Stop.Direct then  println(s"Primer test on $number interrupted.")


/* Coordinates the request from the user and puts Primers to work */
class Server extends SelectActor(Server,"server") :

  /* Helper definitions */
  private val nil: List[Int] = Nil
  private def rst(n: Int, b: Boolean): String = if b then s"$n is prime" else s"$n is not prime"
  private def frm(list: Iterable[Int]) = list.mkString(",")

  /* Keep all primers and results here. At the start there are no primers running, and we
   * know that 1 and 2 are primes.  */
  private var primers : SortedMap[Int,Primer]  = SortedMap.empty
  private var results : SortedMap[Int,Boolean] = SortedMap(1 -> true, 2 -> true)

  /* Send to the client that we have started. */
  println(s"Server Constructed")

  /* Aux method until issue #50 is solved. */
  private def create(number: Int): Primer =
    val primer = new Primer(number,this)
    primer.start()
    primer

  /* Handle the search for a new prime number */
  private def start(number: Int): Unit =
    /* Test for positive numbers */
    if number < 1
    then println(s"Only positive numbers can be tested.")
    /* Test if we are busy of already know the result for this number. */
    else (primers.contains(number),results.get(number)) match
      /* If the number is already calculated, use the stored result */
      case (_,Some(prime)) => println(s"Stored result: ${rst(number,prime)}")
      /* If we are currently crunching the number, tell the user to wait. */
      case (true,None)     => println(s"Crunching on: $number")
      /* Otherwise a new cruncher is started. */
      case (false,None)    => primers += number -> create(number)

  /* Show the achievements up to now */
  private def show(): Unit =
    val running = primers.keys
    val (primes,others) = results.foldLeft((nil,nil)){ case ((p,n),(i,b)) => if b then (i::p,n) else (p,i::n)}
    if primers.isEmpty then println(s"Not Crunching.")  else println(s"Crunching on ${frm(running)}.")
    if results.isEmpty then println(s"Nothing stored.") else println(s"Stored primes: ${frm(primes)}, non-primes: ${frm(others)}")

  /* Show the result on the latest completed number cruncher and update the storage. */
  private def result(number: Int, prime: Boolean): Unit =
    println(s"Completed: ${rst(number,prime)}")
    primers -= number
    results += number -> prime

  /* Terminate the servers. */
  private def teminate(): Unit =
    println("Server Termination Request")
    /* Stop all primers directly. */
    primers.values.foreach(_.stop(Actor.Stop.Direct))
    /* Stop the server actor directly. */
    stop(Actor.Stop.Direct)

  /* Handle all incoming letters. */
  final protected def receive(letter: Letter, sender: Sender): Unit = (sender,letter) match
    case (_,Server.Start(number))                => start(number)
    case (_,Server.Show)                         => show()
    case (_: Primer,Server.Result(number,prime)) => result(number,prime)
    case (_,Server.Terminate)                    => teminate()
    case _                                       => println(s"Unhandled letter: $letter, $sender")

  /* Last goodbyes */
  final override def stopped(cause: Actor.Stop, complete: Boolean) =
    /* Decently say goodbye */
    println("Server stopped.")


/* This is the natural location to define all the letters the actor may receive. */
object Server extends SelectDefine :
  /* Only accept messages from myself (required for the Timing) and outside. */
  type Accept = Primer | Anonymous
  /* Base type of all Server Letters, sealed enables the compiler to see if we handled them all. */
  sealed trait Letter extends Actor.Letter[Accept]
  /* Letter that starts the prime test on this number */
  case class Start(number: Int) extends Letter
  /* Letter the communicates the result back */
  case class Result(number: Int, prime: Boolean) extends Letter
  /* Letter to request the current state of mind. */
  case object Show extends Letter
  /* Letter that instructs the server to stop. */
  case object Terminate extends Letter


/* Main entry point for the demo. */
object Main :
  /* We must provide a default sender. */
  given Actor.Anonymous = Actor.Anonymous
  /* Service that accepts new prime tests requests. */
  private val server    = new Server

  /* Commands to be used for a demo when there is no StdIn available */
  val commands = ConsoleReader(true,"start 9991","start 9771","start 9997","show")

  /* Analyze the user input (from Console) to see what must be done. Return if we need to continue */
  def process(cmd: String): Boolean =
    /* Read the first part as string and the second, if available as integer. */
    cmd.split(" ") match
      case Array(cmd)        => Some(cmd)
      case Array(cmd,number) => number.toIntOption.map((cmd,_))
      case _                 => None
    /* Now see what the contents of the input where */
    match
      /* The user starts a new cruncher */
      case Some(("start",number: Int)) => server ! Server.Start(number); true
      /* The user wants to see the results */
      case Some("show")                => server ! Server.Show; true
      /* The user needs help */
      case Some("help")                => println("Type 'start <number>','stop <number>','show' or 'exit'."); true
      /* The user wants to stop */
      case Some("exit")                => server ! Server.Terminate; false
      /* In all other cases we do not understand the users wishes, so provide a way to obtain help. */
      case _                           => println("Unknown command, type 'help'."); true

  /* Loop for each command, give the app some room to handle the request. */
  def main(args: Array[String]): Unit =
    while process(commands.readln) do Thread.sleep(200)
