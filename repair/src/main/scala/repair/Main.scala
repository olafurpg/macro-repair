package repair

import scala.language.experimental.macros

import socrates.SocratesContext
import socrates.api._

object Main {
  @socrates.SocratesMacro
  def identity[A](e: A): String = macro impl[A]
  def impl[T: SocratesTypeTag](c: SocratesContext)(e: Term): Term = {
    Lit.String(typeTag[T].toString)
  }
}
