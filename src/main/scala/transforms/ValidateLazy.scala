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
    def findExtractInByName(t: Tree): Option[Tree] = {
      var badExtraction = Option.empty[Tree]
      new Traverser {
        override def traverse(t: Tree): Unit = t match {
          case Extract(body, tpe) => super.traverse(body)
          case v@q"$ap[..$targs](...$vargss)" => {
            val vparamss = ap.tpe.paramLists
              (vargss zip vparamss).foreach { case (vargs, vparams) =>
                (vargs zip vparams).foreach { case (arg, param) =>
                  if (hasExtracts(arg) && param.asTerm.isByNameParam) {
                    badExtraction = Some(arg)
                  } else super.traverse(t)
                }
              }
          }
          case other => super.traverse(other)
        }
      }.traverse(t)
      badExtraction
    }
    List(
      Rule("check function application") { case t =>
        val extractInByName = findExtractInByName(t)
        extractInByName.foreach(bad => macroContext.abort(bad.pos, "Cannot extract in by-name parameter"))
        Accept
      }
    )
  }
}

