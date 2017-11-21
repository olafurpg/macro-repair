package socrates

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin

class SocratesPlugin(val global: Global) extends Plugin {
  val name = "scalamacros-plugins-scalac"
  val description = "Implementation of new-style Scala macros for scalac"
  val components = Nil
  object SocratesMacroPlugin extends global.analyzer.MacroPlugin {
//    override def expand
  }
  global.analyzer.addMacroPlugin(SocratesMacroPlugin)
}
