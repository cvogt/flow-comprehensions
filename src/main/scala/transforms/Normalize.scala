package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object Normalize extends Transform {
  override val isTyped = false
  override def rewrites[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rewrite] = {
    import transformContext._
    import macroContext.universe._
    List(
      Rewrite("no extracts") {
        case line if noExtracts(line) => Accept
      },
      Rewrite("trivial extract") {
        case (v@q"$m val $n: $t = ${Extract(e, _)}") if noExtracts(e) => Accept
      },
      Rewrite("not valdef") {
        case line if !line.isInstanceOf[ValDef @unchecked] =>
          val nme = TermName(macroContext.freshName("anon"))
          RewriteTo(q"""
            val $nme = $line
            // return the value, in case it's the return of a block, but wrap in
            // an identity call to suppress expr-in-statement-positin warnings
            _root_.scala.Predef.identity($nme)
          """)
      },
      Rewrite("extracts in block") {
        case q"$mods val $nme: $tpe = {..$lines}" if lines.size > 1 =>
          val newBody = Extract(recur(q"..$lines"), TypeTree())
          RewriteTo(
            q"$mods val $nme: $tpe = $newBody"
          )
      },
      Rewrite("nested extracts") {
        case (v@q"$mods val $nme: $tpe1 = ${Extract(e, tpe2)}") if hasExtracts(e) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          RewriteTo(q"""
            val $tmpNme = $e
            val nme: $tpe1 = ${Extract(q"$tmpNme", tpe2)}
          """)
      },
      Rewrite("extract in line with op") {
        case q"$mods val $nme: $tpe = $pre.$op[..$targs](...$vargs)" if hasExtracts(pre) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          RewriteTo(q"""
            val $tmpNme = $pre
            $mods val $nme: $tpe = $tmpNme.$op[..$targs](...$vargs)
          """)
      },
      Rewrite("extract in pattern prefix") {
        case q"$mods val $nme: $tpe = $pre match { case ..$body }" if hasExtracts(pre) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          RewriteTo(q"""
            val $tmpNme = $pre
            $mods val $nme: $tpe = $tmpNme match { case ..$body }
          """)
      },
      Rewrite("extract in argument") {
        case q"$mods val $nme: $tpe = $pre.$op[..$targs](...$vargss)" if vargss.exists(_.exists(hasExtracts)) =>
          val newVargss = vargss.map(
            _.map {
              case normalArg if noExtracts(normalArg) => normalArg
              case AssignOrNamedArg(lhs, rhs) if hasExtracts(rhs) =>
                val tmpNme = TermName(macroContext.freshName("arg"))
                AssignOrNamedArg(lhs, q"$tmpNme")
              case arg =>
                val tmpNme = TermName(macroContext.freshName("arg"))
                q"tmpNme"
            }
          )
          val bindings = (newVargss.flatten zip vargss.flatten) flatMap {
            case (newArg, oldArg) if noExtracts(oldArg) => None
            case (AssignOrNamedArg(lhs, _), AssignOrNamedArg(_, rhs)) =>
              Some(q"val $lhs = $rhs")
            case (lhs, rhs) =>
              Some(q"val $lhs = $rhs")
          }
          RewriteTo(q"""
            ..$bindings
            $mods val $nme: $tpe = $pre.$op[..$targs](...$newVargss)
          """)
      }
    )
  }
}
