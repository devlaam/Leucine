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


/** The NameActor implements all methods needed for naming an actor. */
transparent trait NameActor(using context: ActorContext) extends Actor, ActorDefs :

  /**
   * Generates an unique name of the structure ClassName#Hash. This can be used instead of
   * self invented names. It is given inside the actor constructor.  */
  private[actors] def uniqueName: String =
    val hash: Long = ##.toLong & 0xFFFFFFFFL
    s"${getClass.getSimpleName}#$hash"

  /**
   * Register this actor. Per default we do that in the actor guard. Other mixins may
   * override this too, for example adopt the actor by a family. */
  private[actors] def register(prename: String): String =
    /* If the name is empty, an unique name is given. If the name starts with a worker prefix a new
     * free worker name is generated. In these situations the actor is not put in the
     * index. But, if an actor under the prename already exists, its index entry overwritten. */
    val rename = Auxiliary.rename(prename,this,ActorGuard.worker,context.workerPrefix)
    /* Add this actor to the guard under its prename, index it when required. */
    ActorGuard.add(this,rename)
    /* Return the newly constructed name. */
    rename.name

  /** In the bare actor the path and name are equal. */
  def path: String = name

  /** Values contains if this actor is a worker based on its name prefix (# per default) */
  def isWorker: Boolean = name.startsWith(context.workerPrefix)
