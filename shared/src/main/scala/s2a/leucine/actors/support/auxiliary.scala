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
 * Contains small library methods needed on different places in the code
 * that lack a natural place to put them. */
object Auxiliary :

  /** Tiny class for renaming an actor. */
  class Rename(val name: String, val inIndex: Boolean)
  object Rename :
    val empty = Rename("",false)

  /** Use this to silence any warnings of discarded values. */
  extension (any: Any)
    inline def toUnit = ()

  /**
   * Splits the string in two parts at the separator. If the separator is not
   * present the whole string is at the first element of the tuple. The second will
   * be empty. Otherwise the tuple is: (part_before_separator , part_after_separator).
   * The separator itself is never present in the first part, and maybe present in
   * the second, if there were multiple occasions. This routine is NOT equivalent
   * with value.splitAt(value.indexOf(separator)) */
  private[actors] def splitAt(value: String, separator: Char): (String,String) =
    /* The index is -1 if the separator is not found, and a value between (including)
     * 0 and size-1 if it is present. */
    val index = value.indexOf(separator)
    /* If the separator is not present, return the whole string at the start, otherwise
     * return the prefix in front and the rest at the back. The rest may contain further
     * occasions of the separator. */
    if index < 0 then (value,"") else (value.substring(0,index), value.substring(index+1))

  /** Generates a new name based on a preliminary name and tells you if this name should be indexed. */
  private[actors] def rename(prename: String, actor: NameActor, worker: Worker, prefix: String): Rename =
    /* If there is no prename, generate a unique name, based on the child's class name, but prohibit indexing. */
    if      prename.isEmpty            then Rename(actor.uniqueName,false)
    /* If we want a worker, generate a new free worker name and  prohibit indexing. */
    else if prename.startsWith(prefix) then Rename(worker.name(prefix),false)
    /* If this actor was give a name by hand use that, and try to index it. */
    else                                    Rename(prename,true)
