package repair

import scala.language.experimental.macros

import scala.reflect.macros.blackbox

object F {
  def f[T](t: T): Option[T] = macro impl[T]
  def impl[T: c.WeakTypeTag](c: blackbox.Context)(
      t: c.Expr[T]): c.Expr[Option[T]] = {
    import c.universe._
    val bound = collection.mutable.Buffer.empty[(c.Tree, ValDef)]
    val OptionGet = typeOf[Option[_]].member(TermName("get"))
    val transformed = c.internal.typingTransform(t.tree) {
      case (t @ q"$fun.$get", api) if t.symbol == OptionGet =>
        val tempName = c.freshName(TermName("tmp"))
        // use api.currentOwner instead of c.internal.enclosingOwner
        val tempSym = c.internal.newTermSymbol(api.currentOwner, tempName)
        c.internal.setInfo(tempSym, t.tpe)
        val tempIdent = Ident(tempSym)
        c.internal.setType(tempIdent, t.tpe)
        bound.append((fun, c.internal.valDef(tempSym)))
        tempIdent
      case (t, api) => api.default(t)
    }
    val embedded = q"${bound(0)._1}.map(${bound(0)._2} => $transformed )"
    println(embedded)
    c.Expr[Option[T]](embedded)
  }
}
