package s2a.leucine.actors

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import utest.*

import s2a.control.{Buffer, Deferred, Helpers}
import s2a.leucine.actors.PlatformContext.Platform

trait ProcessAidRestrictTest(using ac: ActorContext) :

  class Ticker(val writeln: String => Unit, val done: () => Unit) extends RestrictActor(Ticker), RestrictProcess :
    import Actor.Post

    protected def process[Sender <: Accept]: Process[Sender] =
      case (Ticker.Start,_)    =>
        this ! Ticker.Count(0)
        switch(Process.Clear())
        switch(new Process.Push { def process[Sender <: Accept] = tick })
      case (Ticker.Count(n),_) => writeln(s"*$n")


    protected def tick[Sender <: Accept]: Process[Sender] =
      case (Ticker.Count(n),_) =>
        writeln(s"$n")
        this ! Ticker.Count(n+1)
        if      n<4  then switch(new Process.Push    { def process[Sender <: Accept] = tock })
        else if n==4 then switch(new Process.Replace { def process[Sender <: Accept] = tock })
        else              switch(Process.Pop() )

    protected def tock[Sender <: Accept]: Process[Sender] =
      case (Ticker.Count(n),_) =>
        this ! Ticker.Count(n+1)
        switch(new Process.Replace { def process[Sender <: Accept] = tick })

  object Ticker extends RestrictDefine, Stateless :
    type Accept = Actor
    sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
    case class  Count(n: Int) extends Letter[Accept]
    case object Start extends Letter[Accept]


  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, mix by pausing."){
      val expect = List("0","2","4","6","7","8","*9")
      val deferred = Deferred(buffer.readlns)
      val ticker = new Ticker(buffer.writeln,deferred.done)
      ticker ! Ticker.Start
      deferred.await()
      deferred.compare(_ ==> expect) } }


trait ProcessAidSelectTest(using ac: ActorContext) :

  class Ticker(val writeln: String => Unit, val done: () => Unit) extends SelectActor(Ticker), SelectProcess :
    import Actor.Post

    protected def process: Process =
      case (Ticker.Start,_)    =>
        this ! Ticker.Count(0)
        switch(Process.Clear())
        switch(new Process.Push { def process = tick })
      case (Ticker.Count(n),_) => writeln(s"*$n")


    protected def tick: Process =
      case (Ticker.Count(n),_) =>
        writeln(s"$n")
        this ! Ticker.Count(n+1)
        if      n<4  then switch(new Process.Push    { def process = tock })
        else if n==4 then switch(new Process.Replace { def process = tock })
        else              switch(Process.Pop() )

    protected def tock: Process =
      case (Ticker.Count(n),_) =>
        this ! Ticker.Count(n+1)
        switch(new Process.Replace { def process = tick })

  object Ticker extends SelectDefine, Stateless :
    type Accept = Actor
    sealed trait Letter extends Actor.Letter[Accept]
    case class  Count(n: Int) extends Letter
    case object Start extends Letter


  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, mix by pausing."){
      val expect = List("0","2","4","6","7","8","*9")
      val deferred = Deferred(buffer.readlns)
      val ticker = new Ticker(buffer.writeln,deferred.done)
      ticker ! Ticker.Start
      deferred.await()
      deferred.compare(_ ==> expect) } }


trait ProcessAidAcceptTest(using ac: ActorContext) :

  class Ticker(val writeln: String => Unit, val done: () => Unit) extends AcceptActor(Ticker), AcceptProcess :
    import Actor.Post

    protected def process: Process =
      case (Ticker.Start,_)    =>
        this ! Ticker.Count(0)
        switch(Process.Clear())
        switch(new Process.Push { def process = tick })
      case (Ticker.Count(n),_) => writeln(s"*$n")


    protected def tick: Process =
      case (Ticker.Count(n),_) =>
        writeln(s"$n")
        this ! Ticker.Count(n+1)
        if      n<4  then switch(new Process.Push    { def process = tock })
        else if n==4 then switch(new Process.Replace { def process = tock })
        else              switch(Process.Pop() )

    protected def tock: Process =
      case (Ticker.Count(n),_) =>
        this ! Ticker.Count(n+1)
        switch(new Process.Replace { def process = tick })

  object Ticker extends AcceptDefine, Stateless :
    sealed trait Letter extends Actor.Letter[Actor]
    case class  Count(n: Int) extends Letter
    case object Start extends Letter


  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, mix by pausing."){
      val expect = List("0","2","4","6","7","8","*9")
      val deferred = Deferred(buffer.readlns)
      val ticker = new Ticker(buffer.writeln,deferred.done)
      ticker ! Ticker.Start
      deferred.await()
      deferred.compare(_ ==> expect) } }



trait ProcessAidWideTest(using ac: ActorContext) :

  class Ticker(val writeln: String => Unit, val done: () => Unit) extends WideActor(Ticker), WideProcess :
    import Actor.Post

    protected def process: Process =
      case (Ticker.Start,_)    =>
        this ! Ticker.Count(0)
        switch(Process.Clear())
        switch(new Process.Push { def process = tick })
      case (Ticker.Count(n),_) => writeln(s"*$n")


    protected def tick: Process =
      case (Ticker.Count(n),_) =>
        writeln(s"$n")
        this ! Ticker.Count(n+1)
        if      n<4  then switch(new Process.Push    { def process = tock })
        else if n==4 then switch(new Process.Replace { def process = tock })
        else              switch(Process.Pop() )

    protected def tock: Process =
      case (Ticker.Count(n),_) =>
        this ! Ticker.Count(n+1)
        switch(new Process.Replace { def process = tick })

  object Ticker extends WideDefine, Stateless :
    sealed trait Letter extends Actor.Letter[Actor]
    case class  Count(n: Int) extends Letter
    case object Start extends Letter


  given Actor.Anonymous = Actor.Anonymous

  val tests = Tests {
    val buffer = Buffer[String]
    test("sending letters, mix by pausing."){
      val expect = List("0","2","4","6","7","8","*9")
      val deferred = Deferred(buffer.readlns)
      val ticker = new Ticker(buffer.writeln,deferred.done)
      ticker ! Ticker.Start
      deferred.await()
      deferred.compare(_ ==> expect) } }


object ProcessAidRestrictTest extends TestSuite, ProcessAidRestrictTest(using ActorContext.system)
object ProcessAidSelectTest extends TestSuite, ProcessAidSelectTest(using ActorContext.system)
object ProcessAidAcceptTest extends TestSuite, ProcessAidAcceptTest(using ActorContext.system)
object ProcessAidWideTest extends TestSuite, ProcessAidWideTest(using ActorContext.system)






