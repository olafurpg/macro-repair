package repair

import scala.language.experimental.macros

import socrates.SocratesContext
import socrates.api._

object Main {
  @socrates.SocratesMacro
  def identity(e: Any*): String = macro impl
  def impl(c: SocratesContext)(e: Term*): Term = {
    Lit.String(e.map(_.syntax).mkString("+"))
  }
}
