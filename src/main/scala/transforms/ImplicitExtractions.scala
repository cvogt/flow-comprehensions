
package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object ImplicitExtractions extends Transform {
  override def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite] = {
    import transformContext._
    import macroContext.universe._
    List(
      Rewrite("don't change return") {
        case Ident(i) if i.encodedName.toString == returnName.encodedName.toString => Accept
      },
      Rewrite("don't change definitions") {
        case d: ValOrDefDef => Accept
      },
      Rewrite("extract") {
        case t if t.tpe != null && t.tpe.typeSymbol != null && t.tpe.typeSymbol == M.typeSymbol =>
          val tpe = t.tpe.baseType(M.typeSymbol)
          val extracted = Extract(t, TypeTree(tpe.typeArgs(0)))
          RewriteTo(extracted)
      },
      Rewrite("don't change") {
        case t => Accept
      }
    )
  }
}
