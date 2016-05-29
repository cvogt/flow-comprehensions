package org.cvogt.flow

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import org.cvogt.flow.transforms._

trait FlowMacroBase {
  val c: blackbox.Context

  val allTransforms = Seq[Transform](
    ImplicitExtractions,
    PostfixExtractions,
    ValidateLazy,
    Normalize,
    RewriteExtractions
  )
}

trait FlowMacroDebug extends FlowMacroBase { base: FlowMacroBase =>
  import base.c.universe._

  val visualizer = new Visualizer[c.type](c)

  val debug =
      sys.props.get("flat_debug_transforms").getOrElse("").split(",").toSet

  val verbose =
      sys.props.get("flat_debug_verbose") match {
        case Some("true") => true
        case _ => false
      }

  val browse =
    sys.props.get("flat_browse_transforms").getOrElse("").split(",").toSet

  //   /** like identity but prints desugared code and tree */
  //   def debugMacro(tree: Tree): Tree = {
  //     println("code:\n  "+tree)
  //     println("Tree:\n  "+showRaw(tree))
  //     tree
  //   }


  def showSym(s: Symbol) = {
    println(c.universe.show(s))
    println(c.universe.showDecl(s))
    println(showRaw(s, printIds=true, printOwners=true, printTypes=true))
    println(showRaw(s.info, printIds=true, printOwners=true, printTypes=true))
  }

  // def show(t: Tree): Tree = {
  //   println(showRaw(c.macroApplication.symbol, printIds=true, printOwners=true, printTypes=true))
  //   println(showCode(c.macroApplication, printIds=true,printOwners=true,printTypes=true,printRootPkg=true))
  //   println(showRaw(c.macroApplication, printIds=true, printOwners=true,printTypes=true))
  //   val q"$identity { $ctx => ${v@q"val $x = 3"}; $y }" = t
  //   showSym(ctx.symbol)
  //   showSym(ctx.symbol.owner)
  //   showSym(ctx.symbol.owner.owner)
  //   q"()"
  // }

  def show(t: Tree): Tree = {
    println(showCode(t, printIds=true, printOwners=true, printTypes=true))
    println(showRaw(t, printIds=true, printOwners=true, printTypes=true))
    showSym(t.symbol)
    showSym(t.symbol.owner)
    q"()"
  }

  def gui(t: Tree): Tree = {
    visualizer.visualize("gui" -> t)
    q"()"
  }
}
