package repair

import scala.language.experimental.macros

import scala.reflect.macros.whitebox
import scala.tools.nsc
import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

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

  implicit class XtensionBang[A](val a: A) extends AnyVal {
    def unary_![B]: B = a.asInstanceOf[B]
  }

  def apply[T](c: whitebox.Context)(
      func: c.Tree,
      exprs: c.Expr[T]*): c.Expr[List[TestValue]] = {
    import c.universe._
    import compat._
    def tpe(symbol: Symbol, args: Type*) =
      TypeRef(NoPrefix, symbol, args.toList)
    val loggerName = c.fresh(newTermName("$log"))
    val loggerTpe = {
      val function1 = c.mirror.staticClass("scala.Function1")
      val unit = c.mirror.staticClass("scala.Unit")
      val testValue = c.mirror.staticClass("repair.TestValue")
      tpe(function1, tpe(testValue), tpe(unit))
    }
    val loggerSym =
      c.internal.newTermSymbol(c.internal.enclosingOwner, loggerName)
    c.internal.setInfo(loggerSym, loggerTpe)
    val loggerTree = Ident(loggerSym)
    val loggerVal = c.internal.valDef(loggerSym)
//    c.internal.setType(loggerVal, loggerTpe)
    val cc = c.asInstanceOf[scala.reflect.macros.contexts.Context]
    val typer = cc.global.analyzer
      .newTyper(
        cc.callsiteTyper.context.makeNewScope(
          loggerVal.asInstanceOf[cc.Tree],
          loggerSym.asInstanceOf[cc.Symbol]))

    def wrapWithLoggedValue(tree: c.Tree, tpe: c.Type): c.universe.Tree = {
      import c.universe._
      val tempName = c.freshName[TermName]("$temp")
      val result = q"""{
      val $tempName = $tree
      $loggerTree(repair.TestValue(
        ${tree.toString()},
        ${show(tpe)},
        $tempName
      ))
      $tempName
    }"""
      pprint.log(result)
      val typed = typer.typed(
        result.asInstanceOf[cc.Tree],
        tree.tpe.asInstanceOf[cc.Type])
      pprint.log(typed)
//      cc.internal.changeOwner(typed, !tree.symbol, typed.symbol)
      !typed
    }
//    def transform(tree: Tree): Tree = c.internal.typingTransform(tree) {
//      case (i @ Ident(name), api)
//          if i.symbol.pos != NoPosition
//            && i.pos != NoPosition
//          // only trace identifiers coming from the same file,
//          // since those are the ones people probably care about
//            && i.symbol.pos.source == i.pos.source
//          // Don't trace methods, since you cannot just print them "standalone"
//          // without providing arguments
//            && !i.symbol.isMethod
//          // Don't trace identifiers which are synthesized by the compiler
//          // as part of the language implementation
//            && !i.symbol.isImplementationArtifact
//          // Don't trace "magic" identifiers with '$'s in them
//            && !name.toString.contains('$') =>
//        api.typecheck(wrapWithLoggedValue(i, i.tpe.widen))
////      case (i: Assign, api) => api.recur(tree)
//      case (_, api) => api.recur(tree)
//    }
    object tracingTransformer extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
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
            wrapWithLoggedValue(tree, tree.tpe.widen)
//          case i: Typed =>
//            i.tpe match {
//              case t: AnnotatedType
//                 Don't worry about multiple chained annotations for now...
//                if t.annotations.map(_.tpe) == Seq(typeOf[utest.asserts.Show]) =>
//
//                val newTpe = t.underlying
//
//                wrapWithLoggedValue(c)(tree, loggerName, newTpe.widen)
//              case _ => super.transform(tree)
//            }

          // Don't recurse and trace the LHS of assignments
//          case i: Assign => super.transform(i.rhs)
          case _ => super.transform(tree)
        }
      }
    }

    val trees = exprs.map(expr => q"""_root_.repair.AssertEntry(
        ${expr.tree.pos.lineContent.trim},
        ${Function(loggerVal :: Nil, tracingTransformer.transform(expr.tree))}
      )""")

//    val transformed = c.untypecheck(q"""$func(..$trees)""")
    val transformed = q"""$func(..$trees)"""
    val result = c.Expr[List[TestValue]](transformed)
//    println(showCode(result.tree))
    pprint.log(result.tree, height = 200)
    result
  }

}
