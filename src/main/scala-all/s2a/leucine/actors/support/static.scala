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

import scala.quoted.{Quotes, Expr}

/**
 * Use this module to statically obtain the full object/class/method name and parameter list.
 * Statically here means: computed at compile time via this Scala 3 macro, and the resulting
 * string literal will be embedded into the compiled byte code. */
object Static :

  /**
   * Strip the path from argument. Example: "s2a.leucine.actors.BareActor$Envelope@30c9240b"
   * is transformed to "BareActor$Envelope" */
  private def strip(argument: String): String =
    val at   = argument.indexOf('@')
    val end  = if at >= 0 then at else argument.length
    val last = argument.lastIndexOf('.', end - 1)
    if last >= 0 then argument.substring(last + 1, end) else argument.substring(0, end)

  /**
   * A framework private manner to communicate the kind of enclosure for the log entry.
   * The log call can be enclosed in an Method, Class or Object. Reported is the enclosure
   * first encountered in the tree. */
  sealed trait Kind
  case object Object  extends Kind
  case object Class   extends Kind
  case object Method  extends Kind
  case object Unknown extends Kind

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
    else                                   enclosingClass(symbol.owner)

  /**
   * Helper method: walk "up" the ownership chain to find the nearest enclosing DefDef definition symbol.
   * Base case: if there's no symbol, stop and return noSymbol. If the current symbol is a DefDef definition,
   * we found it. Otherwise, recurse to the owner for the enclosing definition. */
  private def enclosingDef(using q: Quotes)(symbol: q.reflect.Symbol): q.reflect.Symbol =
    import q.reflect.Symbol
    if      symbol == Symbol.noSymbol then symbol
    else if symbol.isDefDef           then symbol
    else                                   enclosingDef(symbol.owner)


  /**
   * Helper method: try to find the value of an Expression. It can be true, false or something else.
   * In the latter situation we report a compiler error. This should not happen within Leucine. */
  private def boolValue(boolExpr: Expr[Boolean], boolName: String)(using q: Quotes): Boolean =
    import q.reflect.report
    boolExpr.value match
      case Some(value) => value
      case None        => report.error(s"$boolName must be a compile time constant."); false


  /**
   * Helper class to gather information about the enclosing kind. It determines if we deal with
   * an object, class or method here, and tries to obtain the full/short source path (dependent
   * on fullPath). If the ends in $ it is stripped (usually an object). Exposes two methods to
   * obtain more information about the parameters used. */
  private class Info(fullPath: Boolean)(using q: Quotes) :
    import q.reflect.{Symbol, Ref, Flags}

    /* Obtain the enclosing symbols by walking up the tree. Can fail. */
    private val defSym: Symbol = enclosingDef(Symbol.spliceOwner)
    private val clsSym: Symbol = enclosingClass(Symbol.spliceOwner)

    /**
     * See if we are dealing with user defined parameters here. We take a defensive stand against
     * any other the parameters and arguments. */
    private def isRealParam(symbol: Symbol): Boolean =
      !symbol.flags.is(Flags.Given) &&
      !symbol.flags.is(Flags.Implicit) &&
      !symbol.flags.is(Flags.Synthetic) &&
      !symbol.isTypeParam &&
      !symbol.isType &&
      symbol.isTerm

    /** Get a list of Symbols for the parameters */
    private def getParams(symbol: Symbol): List[Symbol] = symbol.paramSymss.flatten.filter(isRealParam)

    /**
     * Get a list of Strings for the parameter values. With  fullParams = true, we export the real, current
     * value for each parameter. This can become bulky quickly. So set fullParams to false to obtain a dot
     * for each parameter instead. Examples:
     * fullParams = true:   MyClass.myMethod(name="Klaas",age=42)
     * fullParams = false:  MyClass.myMethod(..) */
    private def getArgExpr(fullParams: Boolean, symbols: List[Symbol]):  Expr[List[String]] =
      def extract(symbol: Symbol): Expr[String] =
        val symbolRef = Expr(symbol.name + "=")
        val symbolVal = Ref(symbol).asExprOf[Any]
        if fullPath then '{ $symbolRef + $symbolVal.toString } else '{ $symbolRef + strip($symbolVal.toString) }
      val args = if fullParams then symbols.map(extract) else List(Expr("." * symbols.size))
      Expr.ofList(args)

    /* Intermediate steps to see what the results of the tree climbing were:
     * - we have a class/object if the clsSym is defined,
     * - we have a method if the defSym is defined,
     * - we have a module (an object or its class)
     * - we have a constructor initializer. */
    private val hasClass: Boolean   = clsSym != Symbol.noSymbol
    private val hasMethod: Boolean  = defSym != Symbol.noSymbol
    private val hasModule: Boolean  = hasClass  && clsSym.flags.is(Flags.Module)
    private val hasInit: Boolean    = hasMethod && defSym.name == "<init>"

    /* With the information above we can now determine its true nature:
     * - it is an object if has a class flagged as module and we are not in a method inside an object.
     * - it is a class when it is no object, or if we are in a constructor initializer method.
     * - it is an method when we see a method which is not a constructor initializer
     * In all other cases i don't know what we have. Did i miss anything? */
    private val isObject: Boolean  = !hasMethod && hasClass && hasModule
    private val isClass: Boolean   = (!hasMethod && hasClass && !hasModule) || (hasMethod && hasInit)
    private val isMethod: Boolean  = hasMethod && !hasInit

    /** Obtain the kind of the enclosure */
    val kind: Kind =
      if      isObject then Object
      else if isClass  then Class
      else if isMethod then Method
      else                  Unknown

    /** Obtain the className if we are in a class of the enclosure. */
    val className: String  =
      if      !hasClass then unKnown
      else if fullPath  then clsSym.fullName.stripSuffix("$")
      else                   clsSym.name.stripSuffix("$")

    /**
     * Obtain the method name if we have a method. Don't ask for this if you already know you are inside
     * an constructor initializer, for it will return "init" */
    val defName: String = if hasMethod then defSym.name else unKnown

    /** Request the arguments of the class parameters, with name and value if fullParams = true. */
    def classArguments(fullParams: Boolean): Expr[List[String]]  = getArgExpr(fullParams,getParams(clsSym.primaryConstructor))

    /** Request the arguments of the method parameters, with name and value if fullParams = true. */
    def methodArguments(fullParams: Boolean): Expr[List[String]] = getArgExpr(fullParams,getParams(defSym))


  /**
   * Macro implementation to find the enclosing kind: runs at compile time. Quotes gives access to the
   * compilers reflection API, and the results in a quoted expression (Expr[String]) for the class name. */
  private def kindInfoImpl()(using Quotes): Expr[Kind] =

    /* Choose the correct response for each kind of enclosure */
    Info(true).kind match
      case Object  => '{ Object  }
      case Class   => '{ Class   }
      case Method  => '{ Method  }
      case Unknown => '{ Unknown }


  /**
   * Macro implementation to find the source path of the enclosing object, class or method: runs at compile time.
   * Quotes gives access to the compilers reflection API, and the results in a quoted expression (Expr[String])
   * for the object/class/method name. */
  private def pathInfoImpl(fullPathName: Expr[Boolean])(using Quotes): Expr[String] =

    /* Test and obtain the value of the boolean parameter: fullPathName.
     * Generates a compiler error if the value cannot be determined at compile time. */
    val full: Boolean = boolValue(fullPathName,"fullPathName")

    /* Obtain information about the enclosing structure. */
    val info: Info = Info(full)

    /* Choose the correct response for each kind of enclosure */
    info.kind match
      case Object  => Expr(info.className)
      case Class   => Expr(info.className)
      case Method  => '{ ${Expr(info.className)} + "." + ${Expr(info.defName)} }
      case Unknown => Expr(unKnown)


  /**
   * Macro implementation to find the source path and arguments of the enclosing object, class or method: runs at
   * compile time. Quotes gives access to the compilers reflection API, and the results in a quoted expression
   * (Expr[String]) for the object/class/method name + argument list. */
  private def callInfoImpl(fullPathName: Expr[Boolean], fullParamContent: Expr[Boolean])(using Quotes): Expr[String] =

    /** Construct an expression in case we have an object (just the name) */
    def makeObjectLine(objectName: String): Expr[String] = Expr(objectName)

    /** Construct an expression in case we have a class (name and arguments). Absent arguments generate (). */
    def makeClassLine(className: String, arguments: Expr[List[String]]): Expr[String] =
      '{ ${Expr(className)} + "(" + $arguments.mkString(",") + ")"}

    /** Construct an expression in case we have a method (name and arguments). Absent arguments generate (). */
    def makeMethodLine(className: String, methodName: String, arguments: Expr[List[String]]): Expr[String] =
      '{ ${Expr(className)} + "." + ${Expr(methodName)} + "(" + $arguments.mkString(",") + ")" }

    /* Test and obtain the value of the boolean parameter: fullPathName.
     * Generates a compiler error if the value cannot be determined at compile time. */
    val fullPath: Boolean  = boolValue(fullPathName,"fullPathName")

    /* Test and obtain the value of the boolean parameter: fullParamContent.
     * Generates a compiler error if the value cannot be determined at compile time. */
    val fullParam: Boolean = boolValue(fullParamContent,"fullParamContent")

    /* Obtain information about the enclosing structure. */
    val info: Info = Info(fullPath)

    /* Choose the correct response for each kind of enclosure */
    info.kind match
      case Object  => makeObjectLine(info.className)
      case Class   => makeClassLine(info.className,info.classArguments(fullParam))
      case Method  => makeMethodLine(info.className,info.defName,info.methodArguments(fullParam))
      case Unknown => Expr(unKnown)


  /** Inline method to find the enclosing kind at compile time */
  inline def kindInfo: Kind = ${ kindInfoImpl() }

  /**
   * Inline method to find the source path of the enclosing object, class or method at compile time.
   * With fullPathName = true we obtain the full source path from the root, otherwise only class and
   * or method name are given. */
  inline def pathInfo(inline fullPathName: Boolean): String = ${ pathInfoImpl('fullPathName) }

  /**
   * Inline method to find the source path of the enclosing object, class or method at compile time, with
   * parameter names and argument values for fullParamContent = true. These are reduced to dots when
   * fullParamContent is false. With fullPathName = true we obtain the full source path from the root,
   * otherwise only class and method name are given. */
  inline def callInfo(inline fullPathName: Boolean, inline fullParamContent: Boolean): String = ${ callInfoImpl('fullPathName,'fullParamContent) }

