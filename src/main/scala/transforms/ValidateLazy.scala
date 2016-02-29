package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object ValidateLazy extends Transform {
  override def isTyped = true
  override def rules[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rule] = {
    import transformContext._
    import transformContext.macroContext
    import transformContext.macroContext.universe
    import universe._
    import macroContext.internal
    import visualizer._
    List(
      Rule("check function application") {
        case v@q"$mods val $nme: $tpe = ${ap@q"$pre.$op"}[..$targs](...$vargss)" =>
          val vparamss = ap.tpe.paramLists
          (vargss zip vparamss).foreach { case (vargs, vparams) =>
            (vargs zip vparams).foreach {
              case (arg, param) if hasExtracts(arg) && param.asTerm.isByNameParam =>
                macroContext.abort(
                  arg.pos,
                  s"Extration isn't allowed inside of arguments to by-name parameters"
                )
              case other => ()
            }
          }
          Accept
      },
      Rule("accept others") { case other => Accept }
    )
  }
}

