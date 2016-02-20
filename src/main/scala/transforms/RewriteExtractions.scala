package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object RewriteExtractions extends Transform {

  override def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite] = {
    import transformContext._
    import transformContext.macroContext
    import transformContext.macroContext.universe
    import universe._
    import macroContext.internal

    import visualizer._

    List(
      Rewrite("do extraction") {
        case v@ValDef(mods, paramName, valTpe, e@Extract(extracted, tpe)) =>
          TransformRest { rest =>

            val ctType = extracted.tpe
            val tType = e.tpe
            val cuType = rest.tpe

            val outerOwner = v.symbol.owner.owner
            println(outerOwner)

            val functionValDef: universe.ValDef = {
              val function: Function = {
                val functionType =
                  appliedType(
                    weakTypeOf[Function1[_, _]].typeConstructor,
                    List(tType, cuType)
                  )
                val functionSymbol =
                  internal
                    .newTermSymbol(
                    outerOwner,
                    TermName(macroContext.freshName("anon")),
                    v.pos,
                    Flag.SYNTHETIC
                  )
                internal.setInfo(functionSymbol, NoType)

                val param: ValDef = {
                  val paramSymbol = (
                    internal.newTermSymbol(
                      outerOwner,
                      paramName,
                      extracted.pos,
                      Flag.SYNTHETIC|Flag.PARAM
                    )
                  )
                  val param = (
                    ValDef(
                      Modifiers(Flag.PARAM|Flag.SYNTHETIC),
                      paramName,
                      v.tpt,
                      EmptyTree
                    )
                  )
                  internal.setSymbol(param, paramSymbol)
                  internal.setType(param, NoType)
                  internal.setInfo(paramSymbol, tType)
                  param
                }
                visualize("param"->param)
                val substRest = ( rest
                  // internal.substituteSymbols(
                  //   rest,
                  //   List(v.symbol),
                  //   List(param.symbol)
                  // )
                )
                val res = q""" { ($param) => rest } """
                internal.setType(res, functionType)
                internal.setSymbol(res, functionSymbol)
                res
              }
              val functionValName = TermName(macroContext.freshName("function"))
              moveUnderVal(outerOwner, functionValName, function)
            }

            val flatMapInstanceValDef: ValDef = {

              // The type of FlatMap instance we are hoping for
              val instanceType = appliedType(
                weakTypeOf[FlatMap[_, _, _]].typeConstructor,
                List(ctType, tType, cuType)
              )

              // Summons an instance which may have pending macro
              // expansions
              val unexpandedInstance = macroContext.inferImplicitValue(
                pt=instanceType,
                silent=true
              )
              if (unexpandedInstance == EmptyTree) {
                macroContext.abort(
                  e.pos,
                  s"Don't know how to flatMap over a value of type $ctType using a function of type $tType => $cuType.\n Consider defining an implicit instance of FlatMap[$ctType, $tType, $cuType]."
                )
              } else ()

              // Call typecheck to expand any pending macros
              val expanded = macroContext.typecheck(
                tree=unexpandedInstance,
                pt=instanceType
              )

              val valName = TermName(macroContext.freshName("flatMapInstance"))

              // make a tree that looks like: val $valName = $expanded
              moveUnderVal(outerOwner, valName, expanded)

            }

            val flatMapIdent = new ValDefOps(flatMapInstanceValDef).ident
            val functionIdent = new ValDefOps(functionValDef).ident

            val flatMapMethodSymbol = flatMapIdent.tpe.member(TermName("flatMap"))
            val flatMapMethod = q"$flatMapIdent.$flatMapMethodSymbol"
            internal.setType(flatMapMethod, flatMapMethodSymbol.typeSignatureIn(flatMapIdent.tpe))

            val returnExpr = q"$flatMapMethod($functionIdent)"
            internal.setType(returnExpr, cuType)

            val res = q"""
              $flatMapInstanceValDef
              $functionValDef
              $returnExpr
            """

            visualize(
              "res" -> res,
              "functionValDef" -> functionValDef,
              "flatMapValDef" -> flatMapInstanceValDef
            )

            res

          }

      },
      Rewrite("don't change") { case t =>
        Accept
      }
    )
  }
}
