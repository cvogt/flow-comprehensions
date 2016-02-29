
package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object ImplicitExtractions extends Transform {
  override def isTyped = true
  override def rules[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rule] = {
    import transformContext._
    import macroContext.universe._
    List(
      Rule("don't change return") {
        case Ident(i) if i.encodedName.toString == returnName.encodedName.toString => Accept
      },
      Rule("don't change definitions") {
        case d: ValOrDefDef => Accept
      },
      Rule("extract") {
        case t if t.tpe != null && t.tpe.typeSymbol != null && t.tpe.typeSymbol == M.typeSymbol =>
          val tpe = M.typeSymbol.typeSignatureIn(t.tpe)
          val extracted = Extract(t, TypeTree())
          RewriteTo(extracted)
      },
      Rule("don't change") {
        case t => Accept
      }
    )
  }
}
