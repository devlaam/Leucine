# Leucine
![onJVM](https://github.com/devlaam/Leucine/actions/workflows/onJVM.yml/badge.svg?event=push)
![onJS](https://github.com/devlaam/Leucine/actions/workflows/onJS.yml/badge.svg?event=push)
![onNative](https://github.com/devlaam/Leucine/actions/workflows/onNative.yml/badge.svg?event=push)

Leucine is a small cross platform actor framework without any dependencies. The idea is that it should provide just enough
to be of good use, but nothing more. Above the results of the tests on different platforms for this branch. This Readme gives a
very brief introduction, please see the [wiki](https://github.com/devlaam/Leucine/wiki) for more details (still has to be extended,
expect this over the coming weeks). Versioning adheres to [Semantic Versioning](https://semver.org) to the best of my abilities.

## Motivation
The project is motivated by the recent [change of license](https://www.lightbend.com/akka/license-faq) of [Akka](https://akka.io) and the
not so recent inactivity of the nice port of Akka to JavaScript: [Akka.JS](https://github.com/akka-js/akka.js).
Please note that I used Akka with much pleasure and it is very high quality. It is also very large, probably I did not even use 10% of all its
possibilities. What I however do need  is cross platform abilities and an open source license. If you are on the same page, maybe
Leucine is useful for you as well.


## Basic features
Leucine is typed actor system, with the following properties:
* Actors represent a single unit of computation within one thread.
* Actors are named instances of classes defined by you and derived from one of the base types, depending on your needs.
* Messages (letters) can be send between actors, or from the outside to each actor, to get work done.
* Messages accepted by the actor are put into the mailbox and are processed in the order of arrival.
* Messages send between two actors are guaranteed to keep their order.
* Messages and Senders are typed and can be type-coupled, so you can restrict the message flow at compile time.
* Actors can be told to stop directly, after the current mailbox is depleted or based on some other condition like inactivity.
* Exceptions, initialization and termination generate callbacks which you can handle separately from the main line of code, or ignore.
* There is a monitor class which enables you to get insight in the inner working of your code for debugging or system supervision.
* It runs on JVM, JS and Native, and isolates you from their differences in treading implementation.

### Getting started
The five actor base types you can choose from are:
* `WideActor`: Accepts all letters from all other actors in the system
* `AcceptActor`: Accepts your letters from all other actors in the system
* `SelectActor`: Accepts your letters from a selected list of actors only
* `RestrictActor`: Accepts your letters from a list of actors where per letter the sender actor is restricted
* `RefuseActor`: Accepts no letters

All restrictions are enforced at compile time. The `WideActor` is comparable to an untyped actor and
is mainly for making the transition from an other actor framework to Leucine easier. Should preferably
not be used for new designs, because it makes the actors effectively untyped and thus less safe.
`RefuseActor` is there to offload work from an other actor in a separate thread.

In a simple application it could look something like this:
```Scala
given actorContext: ActorContext = ActorContext.system

/* Use an actor that accepts letters from anyone. */
class MyActor(name: String) extends AcceptActor(MyActor,name) :
  println(s"Actor $name started.")

  /* Handle all incoming letters. */
  protected def receive(letter: Letter, sender: Sender): Unit = letter match
    case MyActor.Text(data)   => println(s"Received text: $data.")
    case MyActor.Number(data) => println(s"Received number: $data.")

/* In the companion object define the base types, and indicate we keep no state. */
object MyActor extends AcceptDefine, Stateless :
  /* Base type of all MyActor Letters, sealed to see if we handled them all. */
  sealed trait Letter extends Actor.Letter[Actor]
  /* Letter that sends some text */
  case class Text(data: String) extends Letter
  /* Letter that sends a number */
  case class Number(data: Int) extends Letter
```
You can send a `letter` to actor `receiver` from actor `sender` with `receiver.send(letter,sender)`, or with the
short form `receiver ! letter` from within the `sender`. For example:
```Scala
object Main :
  def complete(): Unit = println("Demo Complete.")
  val myActor = MyActor("Test")

  def main(args: Array[String]): Unit =
    myActor ! MyActor.Text("Hello World")
    myActor ! MyActor.Number(42)
    myActor.stop(Actor.Stop.Finish)
    ActorGuard.watch(false,1.second,complete)
```
See this [run in Scastie](https://scastie.scala-lang.org/devlaam/rpMXSz4wTA2hnLG68swyBA/3).
Remark: Depending on the release used, there may be syntactical differences.

### Advanced features
The functionality of actors can be extended with mixins. There are:
* `Family` mixins, so you can set up a tree of actors that are accessible through their parents. There can be multiple family root's. An actor family has a
deterministic buildup and tear down sequence.
* `Stash` mixin, so you can put away a letter for handling later.
* `Timing` mixin, needed to send letters with a delay, and the possibility to asynchronously wait for an event to take place
* `Protect` mixin, to create back pressure when the mailbox reaches some threshold.
* `Monitor` mixin and related class, which probes your actor at intervals, and generates an overview of the workings of the whole system. It enables you to see: the time spend in each actor, number of children or worker actors, the messages being send around etc.

## Status
To explore what the possibilities are the best is to clone the repository:
```
$ git clone https://github.com/devlaam/Leucine
$ cd leucine
```
and run the demo's first. In your project you may use
```
libraryDependencies += "com.sense2act" %% "leucine" % "<latest-version>"
```

### Demos
The directory [s2a/leucine/demo](https://github.com/devlaam/Leucine/tree/develop/shared/src/main/scala/s2a/leucine/demo) contains some examples how to use the actors.
There are four demo's:
* Ticker: Runs a stateful actor through some ticks, and at the same time uses Logger actor as an example as well
* Server: Opens the raw TCP `localhost:8180` port for parallel connections and serves the time for 60 seconds.
* Crawler: Spawns some actors in a hierarchical fashion and sends messages up and down the pyramid.
* ChatGRT: Runs a chat bot that produces random output after you have signed up for an account.

All implementations (JVM,JS,Native) have their own Execution Context so you are isolated from the underlying threading model.
In an actor you may never block of course, but with `expect` you can handle waiting for external events.

It turned out not to be possible to run all demos from within SBT on equal footing.
Therefore these are best tested from the command line. We discuss the three different options one by one.

#### Demos on JVM
Compile and package the demo as follows:
```
leucine $ sbt leucineJVM/assembly
[info] welcome to sbt 1.8.2 (AdoptOpenJDK Java 11.0.10)
[info] ...
```
Now you should be able to run the demo's (requires Java to run):
```
leucine $ java -jar jvm/target/scala-3.2.1/main.jar
Started Actor examples on the JVM platform.
Please state the demo you want to run (ticker, server, crawler or chatgrt):
```
and then choose one of them. The `ticker`,`crawler` and `chatgrt` are stand alone demo's, the `server` requires an application
that is able to connect with raw TCP sockets on the localhost, port 8180. To get an impression of the debug capabilities
you can also `ticker debug` or `crawler debug`. The other demo's do not allow for this extra parameter.

#### Demos on NodeJS
Compile and package the demo as follows:
```
leucine $ sbt leucineJS/fullLinkJS
[info] welcome to sbt 1.8.2 (AdoptOpenJDK Java 11.0.10)
[info] ...
```
Now you should be able to run the demo's (requires Node JS to run):
```
leucine $ node js/target/scala-3.2.1/leucine-opt/main.js
Started Actor examples on the JS platform.
Please state the demo you want to run (ticker, server, crawler or chatgrt):
```
and then choose one of them. The `ticker`,`crawler` and `chatgrt` are stand alone demo's, the `server` requires an application
that is able to connect with raw TCP sockets on the localhost, port 8180. To get an impression of the debug capabilities
you can also `ticker debug` or `crawler debug`. The other demo's do not allow for this extra parameter.
And although projectJS is single threaded, the Actor implementation runs as if it is
working in parallel.

#### Demos as native executables
Compile and package the demo as follows:
```
leucine $ sbt leucineNative/nativeLink
[info] welcome to sbt 1.8.2 (AdoptOpenJDK Java 11.0.10)
[info] ...
```
Now you should be able to run the demo's (runs directly):
```
leucine $ native/target/scala-3.2.1/leucine-out
Started Actor examples on the Native platform.
Please state the demo you want to run (ticker, server, crawler or chatgrt):
```
and then choose one of them.  The `ticker`,`crawler` and `chatgrt` are stand alone demo's,  the `server` requires an application
that is able to connect with raw TCP sockets on the localhost, port 8180. To get an impression of the debug capabilities
you can also `ticker debug` or `crawler debug`. The other demo's do not allow for this extra parameter.
And although projectNative (currently 0.4.11) is still single threaded, the Actor implementation runs as if it is
working in parallel. When 0.5.0 comes out, we should have multi threading, but from the user of the actors point
of view, you will not notice the difference, except a higher execution speed.


Although compilation takes a lot longer on Native the run times are amazing.
The crawler demo on Native ran in 0.7ms on my laptop (compiled with 'release-full', see the file
[build.sbt](https://github.com/devlaam/Leucine/blob/develop/build.sbt)),
whereas the java version needed around 48ms!

## Future
This library will be a replacement for my other projects that use Akka at the moment. But Leucine will not try to
copy all of Akka or follow its conventions. Changes in the design may still happen, at least until release 1.0 is reached.
The features I need myself are incorporated by now, so the coming months are devoted to testing and minor syntax changes.
Feature requests are welcome if motivated, and of course, bug reports. Please do not send a PR without consultation.


