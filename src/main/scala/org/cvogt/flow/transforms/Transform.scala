
package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

abstract class Transform {

  def isTyped: Boolean

  def name: String = {
    val str = this.toString
    if (str.isEmpty) ""
    else str.head.toLower +: str.tail
  }

  def rules[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rule]

}
