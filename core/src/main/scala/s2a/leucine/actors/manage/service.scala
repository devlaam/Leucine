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
 * Interface for services that need to be started/stopped by the guard. Implement
 * this in any service you want to put under guards control. You may stop and start
 * any service monitor at will. */
trait Service :

  /**
   * Start the service. You must call this at least once, a service does not start
   * automatically. This can be done for example at the end of the constructor in the derived
   * class, or at the start of your application. Or, the preferred way, register this service
   * at the guard, so it can take care of starting and stopping for you. The parameter hello
   * should be true at first start, so the service may do some special preparation if needed. */
  def start(hello: Boolean): Unit

  /**
   * Stop the service. the parameter goodbye should be true if you want some special action
   * before the service stops. Which action that is, depends on the service. Typically this
   * is when you have no intention to start the service again, for example when yout application
   * is about to terminate. If the service is registered at the guard, the guard will take care
   * of this. */
  def stop(goodbye: Boolean): Unit

