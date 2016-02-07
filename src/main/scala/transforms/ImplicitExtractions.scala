
package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object ImplicitExtractions extends Transform {
  override val isTyped = true
  override def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite] = {
    import transformContext._
    import macroContext.universe._
    List(
      Rewrite("accept") { case t =>
        Accept
      }
    )
  }
}
