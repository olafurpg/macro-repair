package repair

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

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

  def apply[T](c: whitebox.Context)(
      func: c.Tree,
      exprs: c.Expr[T]*): c.Expr[List[TestValue]] = {
    import c.universe._
//    val loggerName = c.freshName("$log")
//    val loggerSym =
//      c.internal.newTermSymbol(c.internal.enclosingOwner, loggerName)

    import compat._
    class tracingTransformer(loggerName: Symbol) extends Transformer {
      def wrapWithLoggedValue(tree: c.Tree, tpe: c.Type): c.universe.Tree = {
        import c.universe._
        val expr = c.Expr(tree)
        val tempExpr: c.Expr[TestValue => Unit] =
          c.Expr(Ident(loggerName))
        val name = c.Expr[String](Literal(Constant(tree.toString())))
        reify {
          val tmp = expr.splice
          tempExpr.splice(TestValue(name.splice, name.splice, tmp))
          tmp
        }.tree
//        q"""{
//      val $tempName = $tree
//      ${loggerName.name}(repair.TestValue(
//        ${tree.toString()},
//        ${show(tpe)},
//        $tempName
//      ))
//      $tempName
//    }"""
      }
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
          case i: Assign => super.transform(i.rhs)

          case _ => super.transform(tree)
        }
      }
    }

    val trees = exprs.map(expr =>
      reify {
        AssertEntry(
          c.Expr[String](q"${expr.tree.pos.lineContent.trim}").splice,
          loggername =>
            c.Expr[T](new tracingTransformer(reify { loggername }.tree.symbol)
                .transform(expr.tree))
              .splice
        )
    })
//      q"""_root_.repair.AssertEntry(
//        ${},
//        ((${loggerName.name}: ${tq""}) => ${tracingTransformer.transform(
//        expr.tree)})
//      )""")

    val tree = c.untypecheck(q"""$func(..$trees)""")
//    val tree = q"""$func(..$trees)"""
    val result = c.Expr[List[TestValue]](tree)
    println(showCode(result.tree))
    result
  }

}
