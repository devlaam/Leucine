# Leucine <img style="float: right;" src="https://github.com/devlaam/Leucine/actions/workflows/scala.yml/badge.svg?event=push">

## Motivation

The project is motivated by the recent [change of license](https://www.lightbend.com/akka/license-faq) of [Akka](https://akka.io) and the
not so recent inactivity of the nice port of Akka to JavaScript: [Akka.JS](https://github.com/akka-js/akka.js).
Please note i used Akka with much pleasure and it is very high quality. It is also very large, probably i did not even use 10% of all its
possibilities. What i however do need  is cross platform abilities and an open source license. If you are on the same page, maybe
Leucine is useful for you as well.


## Features
Leucine is typed actor system, where all letters to be received by an actor have to
be derived from a base type, best defined in the companion object of the actor.
It runs on JVM, JS and Native.

### Getting started
It could look something like this:
```Scala
class MyActor(val name: String) extends BasicActor[MyActor.Letter] :
  // some startup code

  /* Handle all incoming letters. */
  protected def receive(letter: MyActor.Letter): Unit = letter match
    case Text(data) => println(data)
    case Terminated => stopDirect()


object MyActor :
  /* Base type of all MyActor Letters, sealed to see if we handled them all. */
  sealed trait Letter extends Actor.Letter
  /* Letter that sends some text */
  case class Text(data: String) extends Letter
  /* Letter that indicates we are done. */
  case object Terminated extends Letter
```
And of course you can send a `letter` to actor `receiver` from actor `sender` with `receiver.send(letter,sender)`, or with the
short form `receiver ! letter` from within the `sender`.
There are three actor types where to derive from: `BasicActor`, `StandardActor` and `StateActor`.

### Advanced features
The functionality of actors can be extended with mixin's. There are:
* `Family` mixin's, so you can set up a tree of actors that are accessible through their children. There can be multiple family root's so the cleanup is done in an orderly manner.
* `Stash` mixin, so you can put away a letter for handling later.
* `Timing` mixin, needed to send letters with a delay, and the possibility to asynchronously wait for an event to take place
* `Monitor` mixin, which probes your actor at intervals, and generates an overview of the workings of the whole system. It enables you to see the time spend in an actor, number of children or worker actors etc.

### Demos

The directory [s2a/leucine/demo](https://github.com/devlaam/Leucine/tree/master/shared/src/main/scala/s2a/leucine/demo) contains some examples how to use the actors.
There are three demo's:
* Ticker: Runs a stateful actor through some ticks, and at the same time uses Logger actor as an example as well
* Server: Opens the raw TCP `localhost:8180` port for parallel connections and serves the time for 60 seconds.
* Crawler: Yet to be implemented

All implementations (JVM,JS,Native) have their own Execution Context so you are isolated from the underlying threading model.
In an actor you may never block of course, but with `expect` you can handle waiting for external events in your actor.

## Status

The project is in its infancy, so to explore what the possibilities are the best is to clone the repro:
```
$ git clone https://github.com/devlaam/Leucine
```
and compile & run the demo in sbt (in leucine/):
```
sbt:leucine> projects
[info] In file: /.../leucine/
[info]   * leucine
[info]     leucineJS
[info]     leucineJVM
[info]     leucineNative
```
select a project
```
sbt:leucine> project leucineJVM
[info] set current project to leucine (in build file: /.../leucine/)
```
compile
```
sbt:leucine> compile
[info] compiling 25 Scala sources to /.../leucine/jvm/target/scala-3.2.1/classes ...
[success] Total time: 2 s, completed ...
```
and run
```
sbt:leucine> run
[info] running s2a.leucine.demo.Main
Started Logger
Started Actor examples on the JVM platform.
Please specify 'ticker', 'server' or 'crawler' as argument at startup.
```
At the moment there are two demo's available: ticker and server. So you can try one of those:
```
sbt run ticker
sbt run server
```

If you switch to `projectNative` the ticker runs, but the server does not, due to
[a bug in the ServerSocket implementation](https://github.com/scala-native/scala-native/issues/3131).
Hopefully that will be solved soon.
And although projectNative is still single threaded, the Actor implementation runs as if it is working in parallel.

On projectJS the `ticker` runs by default. If you want to see the `server` in action, the source code has to be
changed, for JavaScript does not allow arguments at startup.

## Future
This library will be a replacement for my other projects that use Akka at the moment. I will not try
to copy Akka in any way, but changes in the design may still happen. I take feature requests as well, if
motivated, and of course, bug reports. Please do not send a PR without consultation.


