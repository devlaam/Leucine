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
import s2a.leucine.actors.*


/* The provider class sends a timestamp to the connection every two seconds. Since it is not part of a family we must define the
 * parent actor in its declaration. Further, it cannot have a constant name, so that is a parameter too. Lastly since the
 * socket is also fixed during its lifetime, this is also made a parameter. An other option would have been to send it in
 * a letter. The RestrictActor is used as a base actor, but since we do not really care who send the messages to the provider,
 * we could have chosen the AllowActor as well. This actor is part of a family but does not have children of its own. So
 * we mixin the FamilyLeaf, which requires specifying the parent actor type. We could also have chosen for FamilyBranch, and
 * simply ignoring the children. But less is more. */
class Provider(protected val socket: ClientSocket, protected val parent: Server) extends RestrictActor(Provider,!#), TimingAid, FamilyLeaf[Server], LogInfo :

  Logger.info(s"Provider Constructed, local=${socket.localPort}, remote=${socket.remotePort}")
  /* Send to the client that we are connected. The path is the full name of this actor. */
  socket.writeln(s"Provider $path Connected.")

  /* Send a message after two seconds. */
  post(Provider.Send,2.seconds)

  /* Handle the messages, which is only the posted letter in this case. */
  def receive[Sender <: Accept](letter: Letter[Sender], sender: Sender): Unit = letter match
    case Provider.Send =>
      val datetime = new Date().toString
      val message  = s"Provider $path says: $datetime"
      Logger.info(message)
      socket.writeln(message)
      /* Send a new message after two seconds. */
      post(Provider.Send,2.seconds)

  /* If this actor is stopped, we must close the connection. */
  override def stopped(cause: Actor.Stop, complete: Boolean) =
    println(s"Provider $path stopped.")
    socket.close()

object Provider extends RestrictDefine, Stateless:
  type Accept = Provider
  sealed trait Letter[Sender <: Accept] extends Actor.Letter[Sender]
  case object Send extends Letter[Accept]
