
package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

abstract class Transform {

  def name: String = {
    val str = this.toString
    if (str.isEmpty) ""
    else str.head.toLower +: str.tail
  }

  def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite]
  def rewriteReturned[C <: blackbox.Context](transformContext: TransformContext[C]): transformContext.Rewrite =
    transformContext.Rewrite("leaving return value alone") { case t => transformContext.Accept }

  def isTyped: Boolean

}
