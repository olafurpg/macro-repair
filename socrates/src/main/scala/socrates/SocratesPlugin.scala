package socrates

import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.macros.blackbox
import scala.reflect.macros.compiler.DefaultMacroCompiler
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.reflect.internal.{Flags => gf}
import scala.util.control.NonFatal

class SocratesMacro extends scala.annotation.StaticAnnotation
class Term
trait SocratesContext {
  def message = "hello!"
}

class SocratesPlugin(val global: Global) extends Plugin { self =>
  val name = "socrates"
  val description = "Experimenting with MacroPlugin"
  val components = Nil
  import global._
  import analyzer._
  import global._
  import treeInfo._
  object SocratesMacroPlugin extends global.analyzer.MacroPlugin {
    private lazy val pluginMacroClassloader: ClassLoader = {
      val classpath = global.classPath.asURLs
      macroLogVerbose(
        "macro classloader: initializing from -cp: %s".format(classpath))
      ScalaClassLoader.fromURLs(classpath, this.getClass.getClassLoader)
    }

    private class PluginRuntimeResolver(sym: Symbol)
        extends MacroRuntimeResolver(sym) {
      override def resolveJavaReflectionRuntime(
          defaultClassLoader: ClassLoader): MacroRuntime = {
        // NOTE: defaultClassLoader only includes libraryClasspath + toolClasspath.
        // We need to include pluginClasspath, so that the new macro shim can instantiate
        // ScalacUniverse and ScalacExpansion.
        super.resolveJavaReflectionRuntime(pluginMacroClassloader)
      }
    }
    private val newMacroRuntimesCache =
      perRunCaches.newWeakMap[Symbol, MacroRuntime]
    private def newMacroRuntime(expandee: Tree): Option[MacroRuntime] = {
      macroLogVerbose(s"looking for macro implementation: ${expandee.symbol}")
      println(expandee.symbol)
      println(expandee.symbol.fullName)
      def mkResolver =
        new PluginRuntimeResolver(expandee.symbol).resolveRuntime()
//      Some(newMacroRuntimesCache.getOrElseUpdate(expandee.symbol, mkResolver))
      None
    }
    override def pluginsMacroRuntime(
        expandee: global.analyzer.global.Tree
    ): Option[global.analyzer.MacroRuntime] = {
      println("=> pluginsMacroRuntime")
      return None
      newMacroRuntime(expandee)
    }

    object SocratesShape {
      private def refPart(tree: Tree): Tree = tree match {
        case TypeApply(fun, _) => refPart(fun)
        case ref: RefTree => ref
        case _ => EmptyTree
      }

      def unapply(tree: Tree): Boolean = refPart(tree) match {
        case ref: RefTree =>
//          val qual = ref.qualifier
          // TODO(olafur) validate shape is:
          // (arg1: tpd.Term, arg2: tpd.Term)(implicit c: socrates.Context): Term
          true
      }
    }

    private case class MacroImplResolutionException(pos: Position, msg: String)
        extends Exception
    override def pluginsTypedMacroBody(
        typer: global.analyzer.Typer,
        ddef: global.analyzer.global.DefDef
    ): Option[global.analyzer.global.Tree] = {
//      return None
      println("pluginsTypedMacroBody")
      val untypedMacroImplRef = ddef.rhs.duplicate
      val isSocratesMacro = ddef.mods.annotations.exists { annot =>
        typer.typed(annot).tpe <:< typeOf[SocratesMacro]
      }
      if (!isSocratesMacro) None
      else {
        val macroDef = ddef.symbol
        def fail() = {
          if (macroDef != null) macroDef setFlag gf.IS_ERROR
          ddef setType ErrorType; EmptyTree
        }
        def success(macroImplRef: Tree) = {
          bindMacroImpl(macroDef, macroImplRef); macroImplRef
        }
        val macroDdef1: ddef.type = ddef
        val typer1: typer.type = typer
        val macroCompiler = new {
          val global: self.global.type = self.global
          val typer: self.global.analyzer.Typer =
            typer1.asInstanceOf[self.global.analyzer.Typer]
          val macroDdef: self.global.DefDef = macroDdef1
        } with DefaultMacroCompiler {
          override def resolveMacroImpl: global.Tree = {
            def tryCompile(
                compiler: MacroImplRefCompiler): scala.util.Try[Tree] = {
              try {
                println("Trying compile!")
                // +scalac deviation
                /* compiler.validateMacroImplRef(); skip validation */
                // -scalac deviation
                scala.util.Success(compiler.macroImplRef)
              } catch {
                case NonFatal(ex)
                    if ex.getClass.getName.contains("MacroImplResolution") =>
                  scala.util.Failure(ex)
              }
            }
            println("resolving... ")
            val vanillaImplRef = MacroImplRefCompiler(
              macroDdef.rhs.duplicate,
              isImplBundle = false)
            val vanillaResult = tryCompile(vanillaImplRef)
            try {
              vanillaResult.get
            } catch {
              case MacroImplResolutionException(pos, msg) =>
                context.error(pos, msg)
                EmptyTree
            }
          }
        }
        val macroImplRef = macroCompiler.resolveMacroImpl
        if (macroImplRef.isEmpty) fail() else success(macroImplRef)
        val typed = typer.silent(
          _.typed(markMacroImplRef(untypedMacroImplRef)),
          reportAmbiguousErrors = false)
        typed match {
          case SilentResultValue(macroImplRef @ SocratesShape()) =>
            Some(macroImplRef)
          case SilentResultValue(macroImplRef) =>
            reporter.error(
              macroImplRef.pos,
              showCode(macroImplRef, printTypes = true))
            None
          case SilentTypeError(err) =>
            reporter.error(err.errPos, err.errMsg)
            None
        }
      }
    }

    def mkMacroContext(
        typer: Typer,
        prefixTree: Tree,
        expandeeTree: Tree): MacroContext = {
      println("=> mkMacroContext")
      new {
        val universe: self.global.type = self.global
        val callsiteTyper: universe.analyzer.Typer =
          typer.asInstanceOf[global.analyzer.Typer]
        val expandee = universe.analyzer
          .macroExpanderAttachment(expandeeTree)
          .original orElse duplicateAndKeepPositions(expandeeTree)
      } with UnaffiliatedMacroContext with SocratesContext {
        val prefix = Expr[Nothing](prefixTree)(TypeTag.Nothing)
        override def toString: String =
          "MacroContext(%s@%s +%d)".format(
            expandee.symbol.name,
            expandee.pos,
            enclosingMacros.length - 1 /* exclude myself */ )
      }
    }

    override def pluginsMacroArgs(
        typer: global.analyzer.Typer,
        expandee: global.analyzer.global.Tree
    ): Option[global.analyzer.MacroArgs] = {
      println("=> pluginsMacroArgs")
      val isSocratesMacro =
        expandee.symbol.annotations.exists(_.tpe <:< typeOf[SocratesMacro])
      if (!isSocratesMacro) None
      else {
        val standardArgs = standardMacroArgs(typer, expandee)
        println("ARGS " + standardArgs.others)
        val treeInfo.Applied(core, targs, argss) = expandee
        val prefix = core match {
          case Select(qual, _) => qual; case _ => EmptyTree
        }
        val socratesContext = expandee.attachments
          .get[MacroRuntimeAttachment]
          .flatMap(_.macroContext)
          .getOrElse(mkMacroContext(typer, prefix, expandee))
        println("CTX " + socratesContext)
        Some(standardArgs.copy(c = socratesContext))
      }
    }
  }
  global.analyzer.addMacroPlugin(SocratesMacroPlugin)
}
