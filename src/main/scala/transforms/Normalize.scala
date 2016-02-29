package org.cvogt.flow
package transforms

import scala.reflect.macros.blackbox

case object Normalize extends Transform {
  override def isTyped = true
  override def rules[C <: blackbox.Context](transformContext: TransformContext[C]): List[transformContext.Rule] = {
    import transformContext._
    import transformContext.macroContext
    import transformContext.macroContext.universe
    import universe._
    import macroContext.internal
    import visualizer._
    List(
      Rule("no extracts") {
        case line if noExtracts(line) => Accept
      },
      Rule("trivial extract") {
        case (v@q"$m val $n: $t = ${Extract(e, _)}") if noExtracts(e) => Accept
      },
      Rule("not valdef") {
        case line if !line.isInstanceOf[ValDef @unchecked] =>
          val nme = TermName(macroContext.freshName("anon"))
          RewriteTo(q"""
            val $nme = $line
          """)
      },
      Rule("extracts in block") {
        case v@q"$mods val $nme: $tpe = {..$lines}" if lines.size > 1 =>
          val newBody = Extract(
            macroContext.typecheck(
              tree=recur(q"..$lines"),
              pt=appliedType(M, tpe.tpe)
            ),
            TypeTree(tpe.tpe)
          )
          val newValDef = q"$mods val $nme: $tpe = $newBody"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(newValDef)
      },
      Rule("nested extracts") {
        case (v@q"$mods val $nme: $tpe1 = ${Extract(e, tpe2)}") if hasExtracts(e) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          val newVal = q"val nme: $tpe1 = ${Extract(q"$tmpNme", tpe2)}"
          internal.setSymbol(newVal, v.symbol)
          RewriteTo(q"""
            val $tmpNme = $e
            $newVal
          """)
      },
      Rule("extract in line with op") {
        case v@q"$mods val $nme: $tpe = ${ap@q"$pre.$op"}[..$targs](...$vargs)" if hasExtracts(pre) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          val tmpIdent = Ident(tmpNme)
          internal.setType(tmpIdent, pre.tpe)
          val newAp = q"$tmpIdent.$op"
          internal.setType(newAp, ap.tpe)
          internal.setSymbol(newAp, ap.symbol)
          val newValDef = q"$mods val $nme: $tpe = $newAp[..$targs](...$vargs)"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            val $tmpNme: ${TypeTree(pre.tpe)} = $pre
            $newValDef
          """)
      },
      Rule("extract in pattern prefix") {
        case v@q"$mods val $nme: $tpe = $pre match { case ..$body }" if hasExtracts(pre) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          val newValDef = q"$mods val $nme: $tpe = $tmpNme match { case ..$body }"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            val $tmpNme = $pre
            $newValDef
          """)
      },
      Rule("extract in if condition") {
        case v@q"$mods val $nme: $tpe = if ($cond) $ifTrue else $ifFalse" if hasExtracts(cond) =>
          val tmpNme = TermName(macroContext.freshName("anon"))
          val newValDef = q"$mods val $nme: $tpe = if ($tmpNme) $ifTrue else $ifFalse"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            val $tmpNme = $cond
            $newValDef
          """)
      },
      Rule("extract in or") {
        case v@q"$mods val $nme: $tpe = $a || $b" if hasExtracts(a) || hasExtracts(b) =>
          val newValDef = q"$mods val $nme: $tpe = if ($a) true else $b"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            $newValDef
          """)
      },
      Rule("extract in and") {
        case v@q"$mods val $nme: $tpe = $a && $b" if hasExtracts(a) || hasExtracts(b) =>
          val newValDef = q"$mods val $nme: $tpe = if ($a) $b else false"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            $newValDef
          """)
      },
      Rule("extract in not") {
        case v@q"$mods val $nme: $tpe = !($a)" if hasExtracts(a) =>
          val newValDef = q"$mods val $nme: $tpe = if ($a) false else true"
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            $newValDef
          """)
      },
      Rule("raw extract in both if branches") {
        case v@q"$mods val $nme: $tpe = if ($cond) ${Extract(trueBody, trueTpt)} else ${Extract(falseBody, falseTpt)}" =>
          val lubTpt = TypeTree(lub(List(trueTpt.tpe, falseTpt.tpe)))
          val newValDef = q"""$mods val $nme: $tpe = ${Extract(q"if ($cond) $trueBody else $falseBody", lubTpt)}"""
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            $newValDef
          """)
      },
      Rule("extract in if true branch") {
        case v@q"$mods val $nme: $tpe = if ($cond) $ifTrue else $ifFalse" if hasExtracts(ifTrue) =>
          val newIfTrue = Extract(
            macroContext.typecheck(
              tree=recur(ifTrue),
              pt=appliedType(M, ifTrue.tpe)
            ),
            TypeTree(ifTrue.tpe)
          )
          val newValDef = q"""$mods val $nme: $tpe = if ($cond) $newIfTrue else $ifFalse"""
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            $newValDef
          """)
      },
      Rule("extract in if false branch") {
        case v@q"$mods val $nme: $tpe = if ($cond) $ifTrue else $ifFalse" if hasExtracts(ifFalse) =>
          val newIfFalse = Extract(
            macroContext.typecheck(
              tree=recur(ifFalse),
              pt=appliedType(M, ifFalse.tpe)
            ),
            TypeTree(ifFalse.tpe)
          )
          val newValDef = q"""$mods val $nme: $tpe = if ($cond) $ifTrue else $newIfFalse"""
          internal.setSymbol(newValDef, v.symbol)
          RewriteTo(q"""
            $newValDef
          """)
      },
      Rule("extract in argument") {
        case v@q"$mods val $nme: $tpe = $pre.$op[..$targs](...$vargss)" if vargss.exists(_.exists(hasExtracts)) =>
          val newVargss = vargss.map { vargs =>
            vargs.map {
              case normalArg if noExtracts(normalArg) => normalArg
              case AssignOrNamedArg(lhs, rhs) =>
                val tmpNme = TermName(macroContext.freshName("arg"))
                AssignOrNamedArg(lhs, q"$tmpNme")
              case arg =>
                val tmpNme = TermName(macroContext.freshName("arg"))
                q"$tmpNme"
            }
          }
          val bindings = (newVargss.flatten zip vargss.flatten) flatMap {
            case (newArg, oldArg) if noExtracts(oldArg) => None
            case (AssignOrNamedArg(lhs, _), AssignOrNamedArg(_, rhs)) =>
              Some(q"val $lhs = $rhs")
            case (Ident(lhs: TermName), rhs) =>
              val r = q"val $lhs = $rhs"
              Some(r)
          }
          val newValDef = q"$mods val $nme: $tpe = $pre.$op[..$targs](...$newVargss)"
          internal.setSymbol(newValDef, v.symbol)
          val res = new ListTreeOps(
            bindings :+ newValDef
          ).unify
          RewriteTo(res)
      },
      Rule("accept rest") {
        case line => Accept
      }
    )
  }
}
