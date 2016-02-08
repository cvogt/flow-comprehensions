package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object RewriteExtractions extends Transform {
  override def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite] = {
    import transformContext._
    import macroContext.universe._
    import macroContext.internal
    List(
      Rewrite("do extraction") {
        case v@ValDef(mods, name, valTpe, Extract(extracted, tpe)) =>

          val paramName = TermName(macroContext.freshName("param"))
          val functionSymbol =
            internal.newTermSymbol(v.symbol.owner, TermName(macroContext.freshName("anon")), v.pos)
          internal.setInfo(functionSymbol, NoType)

          val paramSymbol =
            internal.newTermSymbol(functionSymbol, paramName, v.pos, Flag.SYNTHETIC)
          internal.setInfo(paramSymbol, v.symbol.info)

          val param = ValDef(Modifiers(Flag.PARAM|Flag.SYNTHETIC), paramName, valTpe, EmptyTree)
          internal.setType(param, v.tpe)
          internal.setSymbol(param, paramSymbol)

          val paramIdent = Ident(paramName)
          internal.setSymbol(paramIdent, paramSymbol)
          internal.setType(paramIdent, v.tpe)

          val reassign = ValDef(mods, name, valTpe, paramIdent)
          internal.setSymbol(reassign, v.symbol)

          TransformRest { rest =>

            internal.changeOwner(rest, v.symbol.owner, functionSymbol)
            internal.setOwner(v.symbol, functionSymbol)

            val function = q"""
              { $param =>
                $reassign
                $rest
              }
              """

            internal.setSymbol(function, functionSymbol)

            q"""$extracted.flatMap { $function } """
          }

      },
      Rewrite("don't change") { case t =>
        Accept
      }
    )
  }
}
