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

import scala.concurrent.duration.{Duration, FiniteDuration}

/* Unfortunately the current native implementation does not have threadpools or timers. We only have a
 * global execution context and a Thread.Sleep. So we have to implement a loop ourselves for the moment.
 * To that end we use the thread emulator. */

/** Context implementation for the Native Platform */
abstract class ContextImplementation  extends ContextEmulation, PlatformContext


object ContextImplementation :

  /** Returns the platform that is currently running, here Native. */
  def platform = PlatformContext.Platform.Native

  /** Sleep which returns to the caller */
  private[actors] def sleep(loop: => Unit, delay: FiniteDuration): Boolean =
    if delay != Duration.Zero then Thread.sleep(delay.toMillis)
    true
