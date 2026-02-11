package s2a.leucine.actors

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

import scala.concurrent.duration.{FiniteDuration, DurationInt}

/**
 * Setting definitions for the ActorLogger. You must implement these settings
 * and methods to get a working logger. This can be done by making use of on of
 * the predefined traits. */
trait ActorLoggerSettings :
  import ActorLogger.{Level, Timing, Entry, ShowGroups}

  /**
   * FixLevel defines the logging level used at compile time. Regular log statements with a higher
   * level will to removed from the code at compile time. Use this for example to eliminate info and
   * debug log messages by setting it to Level.Warn for a production release. */
  type FixPassLevel <: Level

  /**
   * directSpool can be set to true if you want to directly receive the log entries without making
   * use of the per thread collectors. Sometimes this is handy to zoom in on a critical bug or if you
   * have your own threaded log handler. For delayed logging (which is the purpose of the whole framework
   * inside the actors) this should be false. Note, this definition MUST be implemented with 'inline def'
   * or 'inline val', otherwise there will be compiler errors at the use site. And it MUST be implemented
   * in the top most logger object, thus not in some trait you mix in. This is due to a compiler bug.
   * See also the discussion on the Scala Users Forum.
   * Compiler Bug: https://github.com/scala/scala3/issues/25206
   * Discussion:   https://users.scala-lang.org/t/unclear-java-lang-abstractmethoderror/12204 */
  def directSpool: Boolean

  /**
   * Log entries contain information about the origin of their use (objects, classes and methods). With
   * FullPath to true these will contain full class paths. This can be handy, but also make to logging
   * bulky. Set to false for concise naming. Setting is effective at compile time, is system wide and
   * cannot be superseded by a local setting. */
  type FullPath <: Boolean

  /**
   * Traces contain the parameters and their values of their origin when FullParameters is set to true.
   * Even more than with FullPath, this can become very bulky. If set to false, each parameter is replaced
   * by a dot. The setting is used when no local preference is given. The latter will supersede this value.
   * Setting only influences the logging at the level Trace. */
  type FullParameters <: Boolean

  /**
   * If you need to log confidential data, for example during testing, you can use the info and beta level
   * logs with an optional confidential message and public message. Set ShowConfidential to true to see the
   * former, and to false the latter. The latter setting should be the default for a production release.  */
  type ShowConfidential <: Boolean

  /**
   * Debug and Trace log methods can be made part of a group. When not, this setting defines if the particular
   * call will generate a log entry for debug. Normally this is set to true, but by setting it to false, you
   * can focus on the special group enabled debug log entries. Elimination is done at compile time */
  type GroupDebugDefault <: Boolean

  /**
   * Debug and Trace log methods can be made part of a group. When not, this setting defines if the particular
   * call will generate a log entry for trace. Normally this is set to true, but by setting it to false, you
   * can focus on the special group enabled trace log entries. Elimination is done at compile time */
  type GroupTraceDefault <: Boolean

  /**
   * Each log entry contains information about its source, objects, classes and methods. Implement this filter so
   * you can zoom in on particular log entries by inspecting the path. It is a run time filter that works for all
   * levels. However, if a fatal event appears, the special call handleFatal will nevertheless be used, even
   * if you block the corresponding  entry here. The passed path depends on the setting of FullPath. Return true
   * to allow for the entry, return false to block it. If there is no need for this functionality, just return true.
   * Implementation is obligatory, even if unused. */
  def sourcePathFilter(level: Level, path: String): Boolean

  /**
   * Log entries that are made inside the execution of an actor (can be any class or object) contains information
   * about its actor name/path. With this filter you can zoom in on particular log entries by inspecting the path.
   * It is a run time filter that works for all levels. However, if a fatal event appears, the special call
   * handleFatal will nevertheless be used, even if you block the corresponding  entry here. Return true to allow
   * for the entry, return false to block it. If there is no need for this functionality, just return true.
   * Implementation is obligatory, even if unused. */
  def actorPathFilter(level: Level, path: String): Boolean

  /**
   * Special purpose group that is part of all logging groups. Supply as parameter for the debug or trace call to
   * ensure the entry passes for every setting of showGroups and GroupDebugDefault or GroupTraceDefault. */
  final transparent inline def AllGroups = ActorLogger.AllGroups

  /**
   * Implement this method with the **identical signature** to define the groups to be show in the logging which
   * have a membership group defined. Make use of the ShowGroups class to set the groups. Implement as follows:
   * - Two pass entries for the groups (defined as objects) with names MyFirstGroup and MySecondGroup:
   *   transparent inline def showGroups: ShowGroups((MyFirstGroup,MySecondGroup))
   * - Two pass entries for only one group:
   *   transparent inline def showGroups: ShowGroups((MySecondGroup))
   * - To block all self defined groups:
   *   transparent inline def showGroups: ShowGroups(())
   * You also use the latter syntax if the group selection is not needed. See  ShowGroups for more
   * documentation. Implementation is obligatory, even if unused. */
  transparent inline def showGroups: ShowGroups[?]

  /**
   * This must be implemented with a method that spools the log entries to the process method.
   * The log entries can be obtained with a call to retrieve(). See the different examples for
   * possible implementations. Spool is called by Leucine on a regular basis to offload the log
   * entries from the actor framework to you logging framework. The parameter completed will be
   * true upon the very last call. Note that your implementation must be protected against
   * concurrent entry. This is because each call with uses retrieve which provides each log entry
   * only once. Concurrent entry will not fail, but most likely mixes up the order in weird ways.*/
  def spool(completed: Boolean): Unit

  /**
   * Set localSettings to true to allow for changes in logging level and timing within the actors.
   * Usually, this is only relevant while developing. In deployment you want these settings to be
   * equal throughout the whole application. Setting this to false makes that so, without have
   * to revisit all logging code lines. */
  def localSettings: Boolean

  /**
   * Define the max number of logs allowed before spooling must start. Do not make this value
   * to low, since it every time logs are spooled, they interfere if even ever so slightly,
   * with the actor processing. Realistic values depend on the number of log statements in
   * you code, but 20 to 100 should be considered as realistic lower bounds. Note that logs
   * are also periodically spooled. So this number is effective only if the number of logs
   * builds up quickly in between. */
  def maxLogs: Int

  /**
   * Define how often you want the logs be spooled. Note that this defines a upper bound.
   * Spools may occur sooner for several reasons, for example if the maxLogs are reached
   * or if the application is about to terminate. */
  def spoolInterval: FiniteDuration

  /**
   * Define the default runtime active logging level (see ActorLogger.Level for documentation).
   * Although you may change the result at runtime, there are not guarantees as to when the change
   * will become effective. Usually this for making runtime changes at the very start of the application. */
  def passLevel: Level

  /**
   * Level (equal and) above which the log event is counted as incident in the actors.
   * Although you may change the result at runtime, there are not guarantees as to when the change
   * will become effective. Usually this for making runtime changes at the very start of the application. */
  def incidentLevel: Level

  /** Define the default runtime active logging timing (see ActorLogger.Timing for documentation).
   * Although you may change the result at runtime, there are not guarantees as to when the change
   * will become effective. Usually this for making runtime changes at the very start of the application. */
  def timing: Timing

  /**
   * This method is called for every log entry when the entries are spooled. Note that the implementation
   * must be re-entrant and thread save. If you correctly implemented spool() you may expect strict
   * sequential access to process, but each access can originate from an other thread. */
  def process(entry: Entry): Unit

  /**
   * Implement a handler for the event a fatal situation occurs. Note that the implementation must be
   * re-entrant and thread save. This method is not guaranteed to be strictly sequential. */
  def handleFatal(message: String): Unit


/**
 * Default logger settings you may use for your application in production.
 * It contains reasonable defaults for the relevant obligatory definitions of the settings. */
trait ProductionLoggerSettings :
  import ActorLogger.{Level, Timing}

  /** Set FixPassLevel to Level.Info to for a realistic information load. */
  type FixPassLevel = Level.Info

  /** Set FullPath to false to have concise object/class/method names. */
  type FullPath = false

  /** Setting not relevant when no trace information is passed. */
  type FullParameters = false

  /** Set ShowConfidential to true to see usernames and passwords in the logs. */
  type ShowConfidential = true

  /** Setting not relevant when no debug information is passed. */
  type GroupDebugDefault = false

  /** Setting not relevant when no trace information is passed. */
  type GroupTraceDefault = false

  /** Do not filter of the source path, so return true. */
  def sourcePathFilter(level: Level, path: String): Boolean = true

  /** Do not filter of the actor path, so return true. */
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** During production we do not closely follow the log production. */
  def maxLogs = 100

  /** During production we do not closely follow the log production. */
  def spoolInterval = 1.minute

  /** During production second level accuracy suffices. This is more efficient. */
  def timing: Timing = Timing.Recent

  /** Since FixPassLevel is already Level.Info, lower makes no sense here. */
  def passLevel: Level = Level.Info

  /** Warnings and above still count as incident. */
  def incidentLevel: Level = Level.Warn

  /** Disable any local settings in actors for more efficiency. */
  def localSettings: Boolean = false



/**
 * Default logger settings you may use for your application during beta testing production.
 * It contains reasonable defaults for the relevant obligatory definitions of the settings. */
trait BetaTestLoggerSettings :
  import ActorLogger.{Level, Timing}

  /** Set FixPassLevel to Level.Beta to ensure all beta logs (and above) pass during beta testing. */
  type FixPassLevel = Level.Beta

  /** Set FullPath to false to have concise object/class/method names. */
  type FullPath = false

  /** Setting not relevant when no trace information is passed. */
  type FullParameters = false

  /** Set ShowConfidential to true to see usernames and passwords in the logs. */
  type ShowConfidential = true

  /** Setting not relevant when no debug information is passed. */
  type GroupDebugDefault = false

  /** Setting not relevant when no trace information is passed. */
  type GroupTraceDefault = false

  /** Do not filter of the source path, so return true. */
  def sourcePathFilter(level: Level, path: String): Boolean = true

  /** Do not filter of the actor path, so return true. */
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** During beta testing we do not closely follow the log production. */
  def maxLogs = 100

  /** During production we do not closely follow the log production. */
  def spoolInterval = 1.minute

  /** Set timing to Millis to have a reasonable estimate about the moment the log was processed. */
  def timing: Timing = Timing.Millis

  /** Since FixPassLevel is already Level.Beta, lower makes no sense here. */
  def passLevel:  Level = Level.Beta

  /** Set the incident logging level to warn so we we count warning and more severe log events as incidents. */
  def incidentLevel: Level = Level.Warn

  /** Set local to true to allow for changes in logging/incident level and timing within the actors. */
  def localSettings: Boolean = true


/**
 * Default logger settings you may use for your application during development production.
 * It contains reasonable defaults for the relevant obligatory definitions of the settings. */
trait DevelopmentLoggerSettings :
  import ActorLogger.{Level, Timing}

  /** Set FixPassLevel to Level.Trace to ensure all logs pass during development. */
  type FixPassLevel = Level.Trace

  /** Set FullPath to true to obtain full info on object/class/method names. */
  type FullPath = true

  /** Set FullParameters to true so we see for each trace the parameters used in the call. */
  type FullParameters = true

  /** Set ShowConfidential to true to see usernames and passwords in the logs. */
  type ShowConfidential = true

  /** Set GroupDebugDefault to true to show all logs at debug level that are not member of a group. */
  type GroupDebugDefault = true

  /** Set GroupTraceDefault to true to show all logs at trace level that are not member of a group. */
  type GroupTraceDefault = true

  /** Please specify the filter on the sourcePath in your logger. */
  def sourcePathFilter(level: Level, path: String): Boolean

  /** Please specify the filter on the actorPath in your logger. */
  def actorPathFilter(level: Level, path: String): Boolean

  /** Set the number of maxLogs low, so we have responsive logging. */
  def maxLogs = 10

  /** Set the time between spools low, so we have responsive logging. */
  def spoolInterval = 5.seconds

  /** Set timing to Nanos to have accurate log entries. */
  def timing: Timing = Timing.Nanos

  /** Set default logging level to trace to see all logs during development. */
  def passLevel: Level = Level.Trace

  /** Set the incident logging level to warn so we we count warning and more severe log events as incidents. */
  def incidentLevel: Level = Level.Warn

  /** Set local to true to allow for changes in logging/incident level and timing within the actors. */
  def localSettings: Boolean = true

