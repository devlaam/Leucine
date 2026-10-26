package s2a.leucine.wiki.SystemParameters

// There is no complete example for this feature yet.

import scala.concurrent.duration.{FiniteDuration,DurationInt}
import s2a.leucine.actors.*

object MySystem extends SystemParameters :
  val idleThreadPause: FiniteDuration = 10.millis
  val threadsPerCore: Int             = 1           /* Allow only one thread per code */
  val workerPrefix: String            = "#"
  val familyPathSeparator: Char       = '.'
  val maxMailboxSize: Int             = 100         /* Not more than 100 messages in the box */
  val silentStop: Int                 = 3
  val autoStart: Boolean              = true        /* Compliant with your code written for version < 0.7 */

given ActorContext = ActorContext.DirectImplementation(MySystem)
