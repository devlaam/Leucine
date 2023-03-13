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
 * Contains library methods needed on different places in the code
 * tha lack a natural place to put them. */
object Auxiliary :

  /**
   * Splits the string in two parts at the separator. If the separator is not
   * present the whole string is at the first element of the tuple. The second will
   * be empty. Otherwise the tuple is: (part_before_separator , part_after_separator).
   * The separator itself is never present in the first part, and maybe present in
   * the second, if there were multiple occasions. This routine is NOT equivalent
   * with value.splitAt(value.indexOf(sepatator)) */
  private[actors] def splitAt(value: String, separator: Char): (String,String) =
    /* The index is -1 if the separator is not found, and a value between (including)
     * 0 and size-1 if it is present. */
    val index = value.indexOf(separator)
    /* If the seperator is not present, return the whole string at the start, otherwise
     * return the prefix in front and the rest at the back. The rest may contain further
     * occasions of the separator. */
    if index < 0 then (value,"") else (value.substring(0,index), value.substring(index+1))

