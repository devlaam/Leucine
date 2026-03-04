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

import scala.concurrent.duration.FiniteDuration

/**
 * Configuration definitions for the LogHandler. You must implement these settings
 * and methods to get a working logger. This can be done by making use of one of
 * the predefined traits. */
trait LogHandlerConfig :
  import ActorLogger.{Level, Entry, ShowGroups}

  /**
   * The fixPassLevel defines the logging level used at compile time. Regular log statements with a lower
   * level will to removed from the code at compile time. Use this for example to eliminate info and
   * debug log messages by setting it to Level.Warn for a production release. */
  def fixPassLevel: Level

  /**
   * With systemLogger you can choose if you want Leucine to produce logs. Usually you want to switch
   * this off, especially if your log level is Trace. */
  def systemLogger: Boolean

  /**
   * directSpool can be set to true if you want to directly receive the log entries without making
   * use of the per thread collectors. Sometimes this is handy to zoom in on a critical bug or if you
   * have your own threaded log handler. For delayed logging (which is the purpose of the whole framework
   * inside the actors) this should be false. */
  def directSpool: Boolean

  /**
   * Log entries contain information about the origin of their use (objects, classes and methods). With
   * fullPath to true these will contain full class paths. This can be handy, but also make to logging
   * bulky. Set to false for concise naming. Setting is effective at compile time, is system wide and
   * cannot be superseded by a local setting. */
  def fullPath: Boolean

  /**
   * Traces contain the parameters and their values of their origin when fullParameters is set to true.
   * Even more than with fullPath, this can become very bulky. If set to false, each parameter is replaced
   * by a dot. The setting is used when no local preference is given. The latter will supersede this value.
   * Setting only influences the logging at the level Trace. */
  def fullParameters: Boolean

  /**
   * If you need to log confidential data, for example during testing, you can use the info and beta level
   * logs with an optional confidential message and public message. Set showConfidential to true to see the
   * former, and to false the latter. The latter setting should be the default for a production release.  */
  def showConfidential: Boolean

  /**
   * Debug log methods can be made part of a group. When not, this setting defines if the particular
   * call will generate a log entry for debug. Normally this is set to true, but by setting it to false, you
   * can focus on the special group enabled debug log entries. Elimination is done at compile time. */
  def groupDebugDefault: Boolean

  /**
   * Trace log methods can be made part of a group. When not, this setting defines if the particular
   * call will generate a log entry for trace. Normally this is set to true, but by setting it to false, you
   * can focus on the special group enabled trace log entries. Elimination is done at compile time. */
  def groupTraceDefault: Boolean

  /**
   * Each log entry contains information about its source, objects, classes and methods. Implement this filter so
   * you can zoom in on particular log entries by inspecting the path. It is a run time filter that works for all
   * levels. However, if a fatal event appears, the special call handleFatal will nevertheless be used, even
   * if you block the corresponding  entry here. The passed path depends on the setting of fullPath. Return true
   * to allow for the entry, return false to block it. If there is no need for this functionality, just return true.
   * Implementation is obligatory, even if unused. */
  def sourcePathFilter(level: Level, path: String): Boolean

  /**
   * Log entries that are made inside the execution of an actor (can be any class or object) contains information
   * about its actor name/path. With this filter you can zoom in on particular log entries by inspecting the path.
   * Note that, if you construct a new actor in and other actor, the constructor code of your new actor, actually
   * runs in the constructing actor. So this may lead to missed logs if you set the filter to narrow.
   * It is a run time filter that works for all levels. However, if a fatal event appears, the special call
   * appFatal/sysFatal will nevertheless be used, even if you block the corresponding  entry here. Return true to allow
   * for the entry, return false to block it. If there is no need for this functionality, just return true.
   * Implementation is obligatory, even if unused. */
  def actorPathFilter(level: Level, path: String): Boolean

  /**
   * Implement this method with the **identical signature** to define the groups to be show in the logging which
   * have a membership group defined. Make use of the ShowGroups class to set the groups. Implement as follows:
   * - To pass entries for the groups (defined as objects) with names MyFirstGroup and MySecondGroup:
   *   transparent inline def showGroups: ShowGroups((MyFirstGroup,MySecondGroup))
   * - To pass entries for only one group:
   *   transparent inline def showGroups: ShowGroups((MySecondGroup))
   * - To block all self defined groups:
   *   transparent inline def showGroups: ShowGroups(())
   * You also use the latter syntax if the group selection is not needed. See  ShowGroups for more
   * documentation. Implementation is obligatory, even if unused. */
  transparent inline def showGroups: ShowGroups[?]

  /**
   * This method is called for every log entry when the entries are spooled. Note that the implementation
   * must be re-entrant and thread save. If you correctly implemented spool() you may expect strict
   * sequential access to process, but each access can originate from an other thread. */
  def process(entry: Entry): Unit

  /**
   * Implement a handler for the event a fatal situation occurs in your application. Note that the implementation
   * must be re-entrant and thread save. This method is not guaranteed to be strictly sequential. */
  def appFatal(message: String): Unit

  /**
   * Implement a handler for the event a fatal situation occurs in Leucine. This should not happen of course,
   * but I have replaced the former asserts with this call, to give the application the opportunity for grant the
   * last wishes. Note that the implementation must be re-entrant and thread save. This method is not guaranteed
   * to be strictly sequential (although one occurrence is bad enough) */
  def sysFatal(message: String): Unit

/**
 * Configuration definitions for the Log processing. You must implement these settings
 * and methods to get a working logger. This can be done by making use of one of
 * the predefined traits. */
trait LogProcessConfig :
  import ActorLogger.{Level, Timing}

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

  /**
   * Define the default runtime active logging timing (see ActorLogger.Timing for documentation).
   * Although you may change the result at runtime, there are not guarantees as to when the change
   * will become effective. Usually this for making runtime changes at the very start of the application. */
  def timing: Timing

  /**
   * Special purpose group that is part of all logging groups. Supply as parameter for the debug or trace call to
   * ensure the entry passes for every setting of showGroups and GroupDebugDefault or GroupTraceDefault. */
  final transparent inline def AllGroups = ActorLogger.AllGroups



