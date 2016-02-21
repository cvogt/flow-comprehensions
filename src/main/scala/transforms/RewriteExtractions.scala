package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object RewriteExtractions extends Transform {
  override def isTyped = false
  override def rules[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rule] = {
    import transformContext._
    import transformContext.macroContext
    import transformContext.macroContext.universe
    import universe._
    import macroContext.internal

    import visualizer._

    List(
      Rule("do extraction") {
        case v@ValDef(mods, paramName, valTpt, e@Extract(extracted, tpe)) =>
          TransformRest { rest =>
            q"""
              $extracted.flatMap { ($paramName: $tpe) => $rest }
            """
          }
      },
      Rule("don't change") { case t =>
        Accept
      }
    )
  }
}
