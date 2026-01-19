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

import scala.quoted.{quotes, Quotes, Expr}

/**
 * Use this module to statically obtain the full class name that contains the  method *calling*
 * CallingClass.fullName. Statically here means: computed at compile time via this Scala 3 macro,
 * and the resulting string literal will be embedded into the compiled byte code. */
object CallingClass:

 /**
  * An inline method whose body is replaced at compile time ${ ... } splices in the result of
  * the macro expansion. (The macro produces an Expr[String].) */
  inline def fullName: String = ${ classNameImpl }

  /**
   * Macro implementation: runs at compile time. Quotes gives access to the compilers reflection
   * API, and the results in a quoted expression (Expr[String]) for the class name. */
  private def classNameImpl(using Quotes): Expr[String] =
    import quotes.reflect.Symbol

    /**
     * Helper method: walk "up" the ownership chain to find the nearest enclosing class definition
     * symbol. Base case: if there's no symbol, stop and return noSymbol. If the current symbol
     * is a class definition, we found it. Otherwise, recurse to the owner for the enclosing definition. */
    def enclosingClass(sym: Symbol): Symbol =
      if      sym == Symbol.noSymbol then sym
      else if sym.isClassDef         then sym
      else enclosingClass(sym.owner)

    /**
     * The spliceOwner is the symbol of the definition where the splice ${ classNameImpl } appears
     * (the caller site). We then climb to the enclosing classsymbol. */
    val cls = enclosingClass(Symbol.spliceOwner)

    /**
     * Compute the class name as a plain String. Fallback when we couldn't determine an enclosing class.
     * Otherwise, take the fully-qualified name (incl. packages). */
    val name = if cls == Symbol.noSymbol then "???" else cls.fullName

    /** Lift the computed String into an Expr[String] so the compiler can inline it as constant at the call site. */
    Expr(name)
