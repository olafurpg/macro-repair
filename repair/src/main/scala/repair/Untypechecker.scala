package scala.macros

import scala.reflect.internal.util
import scala.reflect.macros.contexts.Context
import scala.reflect.macros.whitebox

class Untypechecker[C <: whitebox.Context](val c: C) {
  val cc = c.asInstanceOf[Context]
  import cc._
  import cc.universe._
  val self = cc.global
  class ResetAttrs(brutally: Boolean, leaveAlone: Tree => Boolean) {
    // this used to be based on -Ydebug, but the need for logging in this code is so situational
    // that I've reverted to a hard-coded constant here.
    val debug = false
    val trace = scala.tools.nsc.util.trace when debug

    val locals = util.HashSet[Symbol](8)
    val orderedLocals = scala.collection.mutable.ListBuffer[Symbol]()
    def registerLocal(sym: Symbol) {
      if (sym != null && sym != NoSymbol) {
        if (debug && !(locals contains sym)) orderedLocals append sym
        locals addEntry sym
      }
    }

    class MarkLocals extends self.Traverser {
      def markLocal(tree: Tree) {
        if (tree.symbol != null && tree.symbol != NoSymbol) {
          val sym = tree.symbol
          registerLocal(sym)
          registerLocal(sym.sourceModule)
          registerLocal(sym.moduleClass)
          registerLocal(sym.companionClass)
          registerLocal(sym.companionModule)
          registerLocal(sym.deSkolemize)
          sym match {
            case sym: TermSymbol => registerLocal(sym.referenced)
            case _ => ;
          }
        }
      }

      override def traverse(tree: Tree) = {
        tree match {
          case _: DefTree | Function(_, _) | Template(_, _, _) =>
            markLocal(tree)
          case _ =>
            tree
        }

        super.traverse(tree)
      }
    }

    class Transformer extends self.Transformer {
      override def transform(tree: Tree): Tree = {
        if (leaveAlone != null && leaveAlone(tree))
          tree
        else
          super.transform {
            tree match {
              case tree if !tree.canHaveAttrs =>
                tree
              case tpt: TypeTree =>
                if (tpt.original != null)
                  transform(tpt.original)
                else {
                  val refersToLocalSymbols = tpt.tpe != null && (tpt.tpe exists (
                      tp => locals contains tp.typeSymbol))
                  val isInferred = tpt.wasEmpty
                  if (refersToLocalSymbols || isInferred) {
                    tpt.duplicate.clearType()
                  } else {
                    tpt
                  }
                }
              // If one of the type arguments of a TypeApply gets reset to an empty TypeTree, then this means that:
              // 1) It isn't empty now (tpt.tpe != null), but it was empty before (tpt.wasEmpty).
              // 2) Thus, its argument got inferred during a preceding typecheck.
              // 3) Thus, all its arguments were inferred (because scalac can only infer all or nothing).
              // Therefore, we can safely erase the TypeApply altogether and have it inferred once again in a subsequent typecheck.
              // UPD: Actually there's another reason for erasing a type behind the TypeTree
              // is when this type refers to symbols defined in the tree being processed.
              // These symbols will be erased, because we can't leave alive a type referring to them.
              // Here we can only hope that everything will work fine afterwards.
              case TypeApply(fn, args)
                  if args map transform exists (_.isEmpty) =>
                transform(fn)
              case EmptyTree =>
                tree
              case _ =>
                val dupl = tree.duplicate
                // Typically the resetAttrs transformer cleans both symbols and types.
                // However there are exceptions when we cannot erase symbols due to idiosyncrasies of the typer.
                // vetoXXX local variables declared below describe the conditions under which we cannot erase symbols.
                //
                // The first reason to not erase symbols is the threat of non-idempotency (scala/bug#5464).
                // Here we take care of references to package classes (scala/bug#5705).
                // There are other non-idempotencies, but they are not worked around yet.
                //
                // The second reason has to do with the fact that resetAttrs needs to be less destructive.
                // Erasing locally-defined symbols is useful to prevent tree corruption, but erasing external bindings is not,
                // therefore we want to retain those bindings, especially given that restoring them can be impossible
                // if we move these trees into lexical contexts different from their original locations.
                if (dupl.hasSymbolField) {
                  val sym = dupl.symbol
                  val vetoScope = !brutally && !(locals contains sym) && !(locals contains sym.deSkolemize)
                  val vetoThis = dupl.isInstanceOf[This] && sym.isPackageClass
                  if (!(vetoScope || vetoThis)) dupl.symbol = NoSymbol
                }
                dupl.clearType()
            }
          }
      }
    }

    def transform(x: Tree): Tree = {
      new MarkLocals().traverse(x)

      if (debug) {
        assert(locals.size == orderedLocals.size)
        val msg = orderedLocals.toList filter { _ != NoSymbol } map {
          "  " + _
        } mkString "\n"
        trace("locals (%d total): %n".format(orderedLocals.size))(msg)
      }

      new Transformer().transform(x)
    }
  }
  def untypecheckOne(tree2: c.Tree): c.Tree = {
    val tree = tree2.asInstanceOf[Tree]
    var done = false
    val result =
      new ResetAttrs(false, {
        case tree =>
          if (!done) {
            done = true
            pprint.log(tree.productPrefix)
            pprint.log(showCode(tree))
            true
          } else done
      }).transform(tree.asInstanceOf[cc.Tree])
    result.asInstanceOf[c.Tree]
  }
  def untypecheck(tree2: c.Tree): c.Tree = {
    val tree = tree2.asInstanceOf[Tree]
    val result =
      new ResetAttrs(true, {
//        case tree: Match => true
        case tree =>
          pprint.log(tree)
          false
      }).transform(tree.asInstanceOf[cc.Tree])
    result.asInstanceOf[c.Tree]
  }

}
