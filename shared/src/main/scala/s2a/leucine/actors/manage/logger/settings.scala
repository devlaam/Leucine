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
  val maxLogs = 100

  /** During production second level accuracy suffices. This is more efficient. */
  val timing: Timing = Timing.Recent

  /** Since FixPassLevel is already Level.Info, lower makes no sense here. */
  val passLevel: Level = Level.Info

  /** Warnings and above still count as incident. */
  val incidentLevel: Level = Level.Warn

  /** Disable any local settings in actors for more efficiency. */
  val localSettings: Boolean = false



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
  val maxLogs = 100

  /** Set timing to Millis to have a reasonable estimate about the moment the log was processed. */
  val timing: Timing = Timing.Millis

  /** Since FixPassLevel is already Level.Beta, lower makes no sense here. */
  val passLevel:  Level = Level.Beta

  /** Set the incident logging level to warn so we we count warning and more severe log events as incidents. */
  val incidentLevel: Level = Level.Warn

  /** Set local to true to allow for changes in logging/incident level and timing within the actors. */
  val localSettings: Boolean = true


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
  val maxLogs = 10

  /** Set timing to Nanos to have accurate log entries. */
  val timing: Timing = Timing.Nanos

  /** Set default logging level to trace to see all logs during development. */
  val passLevel:  Level = Level.Trace

  /** Set the incident logging level to warn so we we count warning and more severe log events as incidents. */
  val incidentLevel: Level = Level.Warn

  /** Set local to true to allow for changes in logging/incident level and timing within the actors. */
  val localSettings: Boolean = true

