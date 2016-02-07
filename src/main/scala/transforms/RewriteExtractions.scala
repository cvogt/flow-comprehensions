package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object RewriteExtractions extends Transform {
  override val isTyped = false
  override def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite] = {
    import transformContext._
    import macroContext.universe._
    List(
      Rewrite("do extraction") {
        case ValDef(mods, name, valTpe, Extract(extracted, tpe)) =>
          val param = TermName(macroContext.freshName("param"))
          TransformRest(rest => q"""
            $extracted.flatMap { ($param: $tpe) =>
              $mods val $name: $valTpe = $param
              $rest
            }
          """)
      },
      Rewrite("leave alone") { case t =>
        Accept
      }
    )
  }
}
