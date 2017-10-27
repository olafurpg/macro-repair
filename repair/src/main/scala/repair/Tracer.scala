package repair

import scala.language.experimental.macros

import scala.macros.Untypechecker
import scala.reflect.macros.whitebox
import scala.tools.nsc.util

case class AssertEntry[T](label: String, thunk: (TestValue => Unit) => T)
case class TestValue(name: String, tpeName: String, value: Any)

/**
  * Macro implementation to take a block of code and trace through it,
  * converting it into an AssertEntry and inserting debug loggers.
  */
object Tracer {
  def trace[T](e: T): List[TestValue] = macro traceMacro[T]
  def traceMacro[T](c: whitebox.Context)(
      e: c.Expr[T]): c.Expr[List[TestValue]] = {
    import c.universe._
    apply(c)(q"repair.Tracer.traceRuntime", e)
  }
  def traceRuntime[T](entries: AssertEntry[T]*): List[TestValue] = {
    val buf = List.newBuilder[TestValue]
    entries.foreach { entry =>
      entry.thunk(value => buf += value)
    }
    buf.result()
  }

  def apply2[T](c: whitebox.Context)(
      func: c.Tree,
      exprs: c.Expr[T]*): c.Expr[List[TestValue]] = {
    import c.universe._
    import internal.decorators._
    c.inferImplicitValue()
    //    def tpdVal(name: String, tpe: Type): ValDef = {
    //      val nme = c.freshName[TermName](name)
    //      val sym = internal
    //        .newTermSymbol(c.internal.enclosingOwner, nme)
    //        .setInfo(tpe)
    //      val vll = internal.valDef(sym).setType(tpe)
    //    }
    exprs.map { expr =>
      val function = Function()
      q"""_root_.repair.AssertEntry(
            ${expr.tree.pos.lineContent.trim},

           )"""
    }
    val trees = List.empty[c.Tree]
    c.Expr(q"$func(...$trees)")
  }

  def apply[T](c: whitebox.Context)(
      func: c.Tree,
      exprs: c.Expr[T]*): c.Expr[List[TestValue]] = {
    import c.universe._
    val loggerName = c.fresh(newTermName("$log"))
    def wrapWithLoggedValue(tree: c.Tree, tpe: c.Type): c.universe.Tree = {
      import c.universe._
      val tempName = c.freshName[TermName]("$temp")
      q"""{
      val $tempName = $tree
      $loggerName(repair.TestValue(
        ${tree.toString()},
        ${show(tpe)},
        $tempName
      ))
      $tempName
    }"""
    }
    val untypechecker = new Untypechecker[c.type](c)

    import compat._
    object tracingTransformer extends Transformer {
      var stack = List.empty[Tree]
      override def transform(tree: Tree): Tree = {
        tree match {
          case t: CaseDef => t
          case i @ Ident(name)
              if i.symbol.pos != NoPosition
                && i.pos != NoPosition
              // only trace identifiers coming from the same file,
              // since those are the ones people probably care about
                && i.symbol.pos.source == i.pos.source
              // Don't trace methods, since you cannot just print them "standalone"
              // without providing arguments
                && !i.symbol.isMethod
              // Don't trace identifiers which are synthesized by the compiler
              // as part of the language implementation
                && !i.symbol.isImplementationArtifact
              // Don't trace "magic" identifiers with '$'s in them
                && !name.toString.contains('$') =>
            stack.foreach(untypechecker.untypecheckOne)
            wrapWithLoggedValue(tree, tree.tpe.widen)
          case _ =>
            stack = tree :: stack
            val result = super.transform(tree)
            stack = stack.tail
            result
        }
      }
    }

    val trees = exprs.map(expr => q"""_root_.repair.AssertEntry(
        ${expr.tree.pos.lineContent.trim},
        (($loggerName: ${tq""}) => ${tracingTransformer.transform(expr.tree)})
      )""")

//    val result =
//      c.Expr[List[TestValue]](c.resetLocalAttrs(q"""$func(..$trees)"""))
//    val tree = q"""$func(..$trees)"""
    val tree = c.untypecheck(q"""$func(..$trees)""")
//    val tree = untypechecker.untypecheck(q"""$func(..$trees)""")

    val result = c.Expr[List[TestValue]](tree)
    println(showCode(result.tree))
    result
  }

}
