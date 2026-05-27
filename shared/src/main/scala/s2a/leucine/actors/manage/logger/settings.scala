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

import scala.concurrent.duration.DurationInt

/* Discussion:
 * Implementing the settings yourself is best done by copying one of the predefined
 * settings and changing the values to your liking. In order to preserve the types
 * needed for compile time folding of the inlined coded it is best to make use of
 * 'final val' or 'transparent inline def' for your definitions.
 * See also the discussion on the Scala Users Forum.
 * Compiler Bug: https://github.com/scala/scala3/issues/25206
 * Discussion:   https://users.scala-lang.org/t/unclear-java-lang-abstractmethoderror/12204 */

/**
 * Default logger settings you may use for your application in production.
 * It contains reasonable defaults for the relevant obligatory definitions of the settings.
 * You may override the filters and runtime values timing and passLevel to the values you need.
 * If other values need to be changed, define all of them in your Logger object. */
trait ProductionLoggerSettings :
  import ActorLogger.{Level, Timing, ShowChannels, Spooling}
  import ActorLogger.Channel.{SysPrd, AppPrd, AppDvl}

  /** Set fixPassLevel to Level.Info to for a realistic information load. */
  final val fixPassLevel = Level.Info

  /** Set fullPath to false to have concise object/class/method names. */
  final val fullPath = false

  /** Setting not relevant when no trace information is passed. */
  final val fullParameters = false

  /** Set showConfidential to false to hide usernames and passwords in the logs. */
  final val showConfidential = false

  /** Do not filter on the source path, let logs all pass, so return true. */
  def sourcePathFilter(level: Level, path: String): Boolean = true

  /** Do not filter on the actor path, let logs all pass, so return true. */
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** Show only the default channels. */
  final val showChannels = ShowChannels((SysPrd, AppPrd, AppDvl))

  /** Use periodic spooling with high thresholds to minimize execution delay. */
  final val spooling = Spooling.Periodic(100,1.minute,Level.Error)

  /** During production second level accuracy suffices. This is more efficient. */
  def timing: Timing  = Timing.Recent

  /** Since FixPassLevel is already Level.Info, lower makes no sense here. */
  def passLevel: Level = Level.Info

  /** Set the incident logging level to warn so we count warning and more severe log events as incidents. */
  final val incidentLevel = Level.Warn

  /** Disable any local settings in actors for more efficiency. */
  final val localSettings = false



/**
 * Default logger settings you may use for your application during beta testing production.
 * It contains reasonable defaults for the relevant obligatory definitions of the settings.
 * You may override the filters and runtime values timing and passLevel to the values you need.
 * If other values need to be changed, define all of them in your Logger object. */
trait BetaTestLoggerSettings :
  import ActorLogger.{Level, Timing, ShowChannels,Spooling}
  import ActorLogger.Channel.{SysPrd, AppPrd, AppDvl}

  /** Set fixPassLevel to Level.Beta to ensure all beta logs (and above) pass during beta testing. */
  final val fixPassLevel = Level.Beta

  /** Set fullPath to false to have concise object/class/method names. */
  final val fullPath = false

  /** Setting not relevant when no trace information is passed. */
  final val fullParameters = false

  /** Set showConfidential to false to hide usernames and passwords in the logs. */
  final val showConfidential = false

  /** Do not filter of the source path, let logs all pass, so return true. */
  def sourcePathFilter(level: Level, path: String): Boolean = true

  /** Do not filter of the actor path, let logs all pass, so return true. */
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** Show only the default channels. */
  final val showChannels = ShowChannels((SysPrd, AppPrd, AppDvl))

  /** Use periodic spooling with high thresholds to minimize execution delay. */
  final val spooling = Spooling.Periodic(100,1.minute,Level.Warn)

  /** Set timing to Millis to have a reasonable estimate about the moment the log was processed. */
  def timing: Timing = Timing.Millis

  /** Since fixPassLevel is already Level.Beta, lower makes no sense here. */
  def passLevel: Level = Level.Beta

  /** Set the incident logging level to warn so we count warning and more severe log events as incidents. */
  final val incidentLevel = Level.Warn

  /** Enable local settings to allow for changes in logging/incident level and timing within the actors. */
  final val localSettings = true


/**
 * Default logger settings you may use for your application during development production.
 * It contains reasonable defaults for the relevant obligatory definitions of the settings.
 * You may override the filters and runtime values timing and passLevel to the values you need.
 * If other values need to be changed, define all of them in your Logger object. */
trait DevelopmentLoggerSettings :
  import ActorLogger.{Level, Timing, ShowChannels, Spooling}
  import ActorLogger.Channel.{SysPrd, AppPrd, AppDvl}

  /** Set fixPassLevel to Level.Trace to ensure all logs pass during development. */
  final val fixPassLevel = Level.Trace

  /** Set fullPath to true to obtain full info on object/class/method names. */
  final val fullPath = true

  /** Set fullParameters to true so we see for each trace the parameters used in the call. */
  final val fullParameters = true

  /** Set showConfidential to true to see usernames and passwords in the logs. */
  final val showConfidential = true

  /** Do not filter of the source path, let logs all pass, so return true. */
  def sourcePathFilter(level: Level, path: String): Boolean = true

  /** Do not filter of the source path, let logs all pass, so return true. */
  def actorPathFilter(level: Level, path: String): Boolean = true

  /** Show only the default channels. */
  final val showChannels = ShowChannels((SysPrd, AppPrd, AppDvl))

  /** Use periodic spooling with lower thresholds to have responsive logging. */
  final val spooling = Spooling.Periodic(10,5.seconds,Level.Warn)

  /** Set timing to Nanos to have accurate log entries. */
  def timing: Timing = Timing.Nanos

  /** Set default logging level to debug to see all logs during development. */
  def passLevel: Level = Level.Debug

  /** Set the incident logging level to warn so we count warning and more severe log events as incidents. */
  final val incidentLevel = Level.Warn

  /** Enable local settings to allow for changes in logging/incident level and timing within the actors. */
  final val localSettings = true

