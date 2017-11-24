package repair

import scala.language.experimental.macros

import scala.reflect.macros.blackbox
import socrates.SocratesContext
import socrates.Term

object Main {
  @socrates.SocratesMacro
  def syntax(e: Any): String = macro impl2

  def impl(c: blackbox.Context)(e: c.Tree): c.Tree = {
    import c.universe._
    val result = Literal(Constant(showCode(e)))
    println(result)
    result
  }

  def impl2(c: SocratesContext)(e: Term): Term = {
    println("HELLO!")
    c match {
      case s: socrates.SocratesContext =>
        println("SMACRO! " + s.message)
      case s =>
        println(":(  " + s.getClass)
    }
    println(e)
    e
  }
}
