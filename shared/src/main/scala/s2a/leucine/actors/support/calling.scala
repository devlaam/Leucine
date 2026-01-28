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
 * Use this module to statically obtain the full object/class/method name and parameter list.
 * Statically here means: computed at compile time via this Scala 3 macro, and the resulting
 * string literal will be embedded into the compiled byte code. */
object StaticInfo :

  sealed trait Kind
  object MyObject  extends Kind
  object MyClass   extends Kind
  object MyMethod  extends Kind
  object Unknown   extends Kind

  /* Common return string in case we cannot resolve the issue. */
  private val unKnown: String = "<?>"

  /**
   * Helper method: walk "up" the ownership chain to find the nearest enclosing class definition symbol.
   * Base case: if there's no symbol, stop and return noSymbol. If the current symbol is a class definition,
   * we found it. Otherwise, recurse to the owner for the enclosing definition. */
  private def enclosingClass(using q: Quotes)(symbol: q.reflect.Symbol): q.reflect.Symbol =
    import q.reflect.Symbol
    if      symbol == Symbol.noSymbol then symbol
    else if symbol.isClassDef         then symbol
    else enclosingClass(symbol.owner)

  /**
   * Helper method: walk "up" the ownership chain to find the nearest enclosing DefDef definition symbol.
   * Base case: if there's no symbol, stop and return noSymbol. If the current symbol is a DefDef definition,
   * we found it. Otherwise, recurse to the owner for the enclosing definition. */
  private def enclosingDef(using q: Quotes)(symbol: q.reflect.Symbol): q.reflect.Symbol =
    import q.reflect.Symbol
    if symbol == Symbol.noSymbol then symbol
    else if symbol.isDefDef then symbol
    else enclosingDef(symbol.owner)


  private def boolValue(boolExpr: Expr[Boolean], boolName: String)(using q: Quotes): Boolean =
    import q.reflect.report
    boolExpr.value match
      case Some(value) => value
      case None        => report.error(s"$boolName must be a compile time constant."); false


  private class Info(fullName: Boolean)(using q: Quotes) :
    import q.reflect.{Symbol, Ref, Flags}

    private val defSym = enclosingDef(Symbol.spliceOwner)
    private val clsSym = enclosingClass(Symbol.spliceOwner)

    private def isRealParam(symbol: Symbol): Boolean =
      !symbol.flags.is(Flags.Given) &&
      !symbol.flags.is(Flags.Implicit) &&
      !symbol.flags.is(Flags.Synthetic)

    private def getParams(symbol: Symbol): List[Symbol] = symbol.paramSymss.flatten.filter(isRealParam)

    private def getArgExpr(fullParams: Boolean, symbols: List[Symbol]):  Expr[List[String]] =
      def extract(symbol: Symbol) =
        val symbolRef = Expr(symbol.name + "=")
        val symbolVal = Ref(symbol).asExprOf[Any]
        '{ $symbolRef + $symbolVal.toString }
      val args = if fullParams then symbols.map(extract) else List(Expr("." * symbols.size))
      Expr.ofList(args)

    private val hasClass: Boolean   = clsSym != Symbol.noSymbol
    private val hasMethod: Boolean  = defSym != Symbol.noSymbol
    private val hasFlags: Boolean   = hasClass  && clsSym.flags.is(Flags.Module)
    private val hasInit: Boolean    = hasMethod && defSym.name == "<init>"

    private val isObject: Boolean  = !hasMethod && hasClass && hasFlags
    private val isClass: Boolean   = (!hasMethod && hasClass && !hasFlags) || (hasMethod && hasInit)
    private val isMethod: Boolean  = hasMethod && !hasInit

    val kind: Kind =
      if      isObject then MyObject
      else if isClass  then MyClass
      else if isMethod then MyMethod
      else                  Unknown

    val className  =
      if      !hasClass then unKnown
      else if fullName  then clsSym.fullName.stripSuffix("$")
      else                   clsSym.name.stripSuffix("$")

    val defName = if hasMethod then defSym.name else unKnown

    def classArguments(fullParams: Boolean): Expr[List[String]]  = getArgExpr(fullParams,getParams(clsSym.primaryConstructor))
    def methodArguments(fullParams: Boolean): Expr[List[String]] = getArgExpr(fullParams,getParams(defSym))


  /**
   * Macro implementation to find the then class name: runs at compile time. Quotes gives access to the
   * compilers reflection API, and the results in a quoted expression (Expr[String]) for the class name. */
  private def classInfoImpl(fullName: Expr[Boolean])(using Quotes): Expr[String] =
    import quotes.reflect.{Symbol, Flags, report}

    /**
     * The spliceOwner is the symbol of the definition where the splice ${ classNameImpl } appears
     * (the caller site). We then climb to the enclosing classsymbol. */
    val clsSym = enclosingClass(Symbol.spliceOwner)

    val hasClass = clsSym != Symbol.noSymbol
    val hasFlags = clsSym.flags.is(Flags.Module)

    /**
     * Compute the class name as a plain String. Fallback when we couldn't determine an enclosing class.
     * Otherwise, take the fully-qualified name (incl. packages). */
    val className  = if !hasClass then unKnown else
      fullName.value match
        case Some(true)  => clsSym.fullName.stripSuffix("$")
        case Some(false) => clsSym.name.stripSuffix("$")
        case None        => report.error("fullName must be a compile time constant."); unKnown

    /** Lift the computed String into an Expr[String] so the compiler can inline it as constant at the call site. */
    Expr(className)


  /**
   * Macro implementation to find the enclosing kind (): runs at compile time. Quotes gives access to the
   * compilers reflection API, and the results in a quoted expression (Expr[String]) for the class name. */
  private def kindInfoImpl()(using Quotes): Expr[Kind] = // '{ (MyObject : Kind) }

    Info(true).kind match
      case MyObject  => '{ MyObject }
      case MyClass   => '{ MyClass  }
      case MyMethod  => '{ MyMethod }
      case Unknown   => '{ Unknown  }

  /**
   * Macro implementation for trace: runs at compile time. Quotes gives access to the compilers reflection API,
   * and the results in a quoted expression (Expr[String]) for the object/class/method name + parameter list. */
  private def pathInfoImpl(fullPathName: Expr[Boolean])(using Quotes): Expr[String] =

    val full = boolValue(fullPathName,"fullPathName")
    val info = Info(full)

    info.kind match
      case MyObject  => Expr(info.className)
      case MyClass   => Expr(info.className)
      case MyMethod  => '{ ${Expr(info.className)} + "." + ${Expr(info.defName)} }
      case Unknown   => Expr(unKnown)


  /**
   * Macro implementation for trace: runs at compile time. Quotes gives access to the compilers reflection API,
   * and the results in a quoted expression (Expr[String]) for the object/class/method name + parameter list. */
  private def callInfoImpl(fullPathName: Expr[Boolean], fullParamContent: Expr[Boolean])(using Quotes): Expr[String] =

    def makeObjectLine(objectName: String): Expr[String] = Expr(objectName)

    def makeClassLine(className: String, arguments: Expr[List[String]]): Expr[String] =
      '{ ${Expr(className)} + "(" + $arguments.mkString(",") + ")"}

    def makeMethodLine(className: String, methodName: String, arguments: Expr[List[String]]): Expr[String] =
      '{ ${Expr(className)} + "." + ${Expr(methodName)} + "(" + $arguments.mkString(",") + ")" }

    val fullPath  = boolValue(fullPathName,"fullPathName")
    val fullParam = boolValue(fullParamContent,"fullParamContent")

    val info = Info(fullPath)

    info.kind match
      case MyObject  => makeObjectLine(info.className)
      case MyClass   => makeClassLine(info.className,info.classArguments(fullParam))
      case MyMethod  => makeMethodLine(info.className,info.defName,info.methodArguments(fullParam))
      case Unknown   => Expr(unKnown)


  /**
   * Macro implementation for trace: runs at compile time. Quotes gives access to the compilers reflection API,
   * and the results in a quoted expression (Expr[String]) for the object/class/method name + parameter list. */
  private def sourceInfoImpl(fullName: Expr[Boolean], withParams: Expr[Boolean], fullParams: Expr[Boolean])(using Quotes): Expr[String] =
    import quotes.reflect.{Symbol, Flags, Ref, report}

    def boolValue(boolExpr: Expr[Boolean], boolName: String): Boolean =
      boolExpr.value match
        case Some(value) => value
        case None        => report.error(s"$boolName must be a compile time constant."); false

    // Helper: keep only "regular" parameters (skip using/given/implicit/synthetic)
    def isRealParam(symbol: Symbol): Boolean =
      !symbol.flags.is(Flags.Given) &&
      !symbol.flags.is(Flags.Implicit) &&
      !symbol.flags.is(Flags.Synthetic)

    def getParams(symbol: Symbol): List[Symbol] =
      if boolValue(withParams,"withParams")
      then symbol.paramSymss.flatten.filter(isRealParam)
      else Nil

    def getArgExpr(symbols: List[Symbol]):  Expr[List[String]] =
      def extract(symbol: Symbol) =
        val symbolRef = Expr(symbol.name + "=")
        val symbolVal = Ref(symbol).asExprOf[Any]
        '{ $symbolRef + $symbolVal.toString }
      val args = fullParams.value match
        case Some(true)  => symbols.map(extract)
        case Some(false) => List(Expr("." * symbols.size))
        case None        => report.error("fullParams must be a compile time constant."); Nil
      Expr.ofList(args)

    def makeObjectLine(objectName: String): Expr[String] = Expr(objectName)

    def makeClassLine(className: String, arguments: Expr[List[String]]): Expr[String] =
      if boolValue(withParams,"withParams")
      then '{ ${Expr(className)} + "(" + $arguments.mkString(",") + ")"}
      else Expr(className)

    def makeMethodLine(className: String, methodName: String, arguments: Expr[List[String]]): Expr[String] =
      if boolValue(withParams,"withParams")
      then '{ ${Expr(className)} + "." + ${Expr(methodName)} + "(" + $arguments.mkString(",") + ")" }
      else '{ ${Expr(className)} + "." + ${Expr(methodName)} }

    val defSym = enclosingDef(Symbol.spliceOwner)
    val clsSym = enclosingClass(Symbol.spliceOwner)

    val hasFlags  = clsSym.flags.is(Flags.Module)
    val hasClass  = clsSym != Symbol.noSymbol
    val hasMethod = defSym != Symbol.noSymbol

    val className  = if !hasClass then unKnown else
      fullName.value match
        case Some(true)  => clsSym.fullName.stripSuffix("$")
        case Some(false) => clsSym.name.stripSuffix("$")
        case None        => report.error("fullName must be a compile time constant."); unKnown

    if hasMethod then
      val hasInit: Boolean            = defSym.name == "<init>"
      // Collect parameter symbols from all parameter lists
      val params: List[Symbol]       = getParams(defSym)
      val argums: Expr[List[String]] = getArgExpr(params)
      if hasInit then makeClassLine(className,argums) else makeMethodLine(className,defSym.name,argums)
    // Not inside a DefDef (method/ctor). But we *might* still be inside a class body.
    else if hasClass then
      // Try to synthesize the same shape as case-class toString: ClassName(v1,v2,...)
      val pricst: Symbol             = clsSym.primaryConstructor
      val params: List[Symbol]       = getParams(pricst)
      val argums: Expr[List[String]] = getArgExpr(params)
      if hasFlags then makeObjectLine(className) else makeClassLine(className,argums)
    else
      // Truly top-level / no enclosing class
      Expr(unKnown)

  /**
   * An inline method whose body is replaced at compile time ${ ... } splices in the result of
   * the macro expansion. (The macro produces an Expr[String].) */
  inline def kindInfo(): Kind = ${ kindInfoImpl() }

  /**
   * An inline method whose body is replaced at compile time ${ ... } splices in the result of
   * the macro expansion. (The macro produces an Expr[String].) */
  inline def pathInfo(inline fullPathName: Boolean): String = ${ pathInfoImpl('fullPathName) }

  /** Context-sensitive trace:
    * - inside constructor:  ClassName(arg1,arg2,...)
    * - inside method:       ClassName.methodName(a=..., c=...)
    * - otherwise:           this.toString (if available)
    */
  inline def callInfo(inline fullPathName: Boolean, inline fullParamContent: Boolean): String = ${ callInfoImpl('fullPathName,'fullParamContent) }

