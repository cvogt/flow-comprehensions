package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object PostfixExtractions extends Transform {
  override def isTyped = true
  override def rules[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rule] = {
    import transformContext._
    import transformContext.macroContext
    import transformContext.macroContext.universe
    import universe._
    import macroContext.internal
    import visualizer._
    val transformer = new Transformer {
      override def transform(t: Tree) = t match {
        case PostfixExtract(body, tpe) => q"$contextName.?[${super.transform(tpe)}](${super.transform(body)})"
        case other => super.transform(other)
      }
    }
    List(
      Rule("has postfix extracts") {
        case line if hasPostfixExtracts(line) =>
          RewriteTo(
            transformer.transform(line)
          )
      },
      Rule("no postfix extracts") {
        case line if noPostfixExtracts(line) => Accept
      }
    )
  }
}
