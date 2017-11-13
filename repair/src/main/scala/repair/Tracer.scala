package repair

import scala.language.experimental.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox

case class AssertEntry[T](label: String, thunk: (TestValue => Unit) => T)
case class TestValue(name: String, tpeName: String, value: Any)

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


    object tracingTransformer extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          case i @ Ident(name) if i.tpe <:< typeOf[Array[String]] =>
            i.symbol.owner
            wrapWithLoggedValue(tree, tree.tpe.widen)
          case _ => super.transform(tree)
        }
      }
    }

    val trees = exprs.map(expr => q"""_root_.repair.AssertEntry(
        ${expr.tree.pos.lineContent.trim},
        (($loggerName: ${tq""}) => ${tracingTransformer.transform(expr.tree)})
      )""")

    // How do we get rid of untypecheck here?
    val result = c.Expr[List[TestValue]](c.untypecheck(q"""$func(..$trees)"""))
    println(showCode(result.tree, printTypes = true))
    result
  }

}
