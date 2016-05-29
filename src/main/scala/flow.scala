package org.cvogt.flow

import scala.language.experimental.macros
import scala.reflect.macros.blackbox


class FlowMacros(val c: blackbox.Context) extends FlowMacroBase
      with FlowMacroDebug with FlowMacroNameBasedConstruct { base: FlowMacroBase =>
  import base.c.universe._
  import base.c.weakTypeOf
  import base.c.internal

  def transform[M: WeakTypeTag, T: WeakTypeTag](returnName: Tree)(transforms: Tree*)(comprehension: Tree): Tree = {
    val q"($ctxNme: $tpe) => $body" = comprehension

    val m = weakTypeOf[M].typeConstructor
    val returnNme = returnName match {
      case Literal(Constant(nme: String)) => TermName(nme)
      case _ => c.abort(returnName.pos, "return name must be a String literal")
    }

    val transformNames = transforms map {
      case (t@Literal(Constant(transformName: String))) => transformName
      case other => c.abort(other.pos, s"transformation phases must be identified with literal Strings")
    }

    val currentTransformName = transformNames
    transforms match {
      case Nil =>
        internal.changeOwner(body, comprehension.symbol, comprehension.symbol.owner)
      case (t@Literal(Constant(currentTransformName: String))) :: remainingTransformations =>
        def ifDebug(body: => Unit): Unit = {
          if(debug(currentTransformName)) body
          else ()
        }
        def ifVerbose(body: => Unit): Unit = {
          if (debug(currentTransformName) && verbose) body
          else ()
        }
        def ifBrowse(body: => Unit): Unit = {
          if(browse(currentTransformName)) body
          else ()
        }

        allTransforms.find(_.name == currentTransformName) match {

          case Some(currentTransform) =>

            val utils = new TransformUtils[c.type](c) {
              val M: Type = m.typeConstructor
              val contextName: TermName = ctxNme
            }

            ifDebug {
              println("====================")
              println(s"running $currentTransformName")
              println(s"$currentTransform input:")
              println(showCode(comprehension))
            }

            case class Step(parts: List[Int] = List(1)) {
              def next = Step(
                parts.init :+ (parts.last + 1)
              )
              def sub = Step(parts :+ 1)
              override def toString = parts.mkString(".")
            }

            def traverse(
              done: List[Tree],
              remaining: List[Tree],
              step: Step
            ): Tree = {
              remaining match {
                case h :: t =>
                  val ctx = new TransformContext[c.type](c) {
                    val M: Type = m.typeConstructor
                    val contextName: TermName = ctxNme
                    val returnName = returnNme
                    val returnType = body.tpe
                    def recur(t: Tree): Tree = {
                      ifVerbose {
                        println(s"""$step - recurring on $t""")
                        println(s"""$step - transforming to prepare for recurrence as ${step.sub}""")
                      }
                      val nonEmptyBody = t match {
                        case EmptyTree => q"()"
                        case nonEmpty => nonEmpty
                      }
                      val returnName = TermName(c.freshName("return"))
                      val withCorrectReturnValue = {
                        val lines = nonEmptyBody.shard
                          (lines.init :+ q"val $returnName = ${liftM(lines.last)}" :+ q"$returnName").unify
                      }
                      ifVerbose {
                        println(s"""$step - transformed to $withCorrectReturnValue"""")
                        println(s"""$step - beginning recurrence"""")
                      }
                      traverse(Nil, withCorrectReturnValue.shard, step.sub)
                    }
                  }
                  import ctx._

                  ifVerbose {
                    println(s"""$step - input: $h""")
                  }
                  currentTransform.rules[c.type](ctx).find { rw =>
                    rw.pf.isDefinedAt(h)
                  } match {
                    case None =>
                      ifVerbose {
                        println(s"""$step - error: no matching rewrite rule"""")
                      }
                      c.abort(
                        c.enclosingPosition,
                        "no matching rewrite rule for "++showCode(h)
                      )
                    case Some(rw) =>
                      ifVerbose {
                        println(s"""$step - matching rule: "${rw.name}"""")
                      }
                      rw.pf(h) match {
                        case Accept =>
                          ifVerbose {
                            println(s"""$step - result: accept line""")
                          }
                          traverse(done :+ h, t, step.next)
                        case RewriteTo(replacement, reprocess) =>
                          ifVerbose {
                            println(s"""$step - result: rewrite line""")
                            if (reprocess) {
                              println(s"""$step - rewriteTo: ${showCode(replacement)}""")
                            } else {
                              println(s"""$step - accept as: ${showCode(replacement)}""")
                            }
                          }
                          if (reprocess) traverse(done, replacement.shard ++ t, step.next)
                          else traverse(done ++ replacement.shard, t, step.next)
                        case TransformRest(f) =>
                          ifVerbose {
                            println(s"""$step - result: transform rest""")
                            println(s"""$step - processing rest so we can do the transform""")
                          }
                          val input = traverse(Nil, t, step.sub)
                          val output = f(input)
                          ifVerbose {
                            println(s"""$step - transform input: ${showCode(input)}""")
                            println(s"$step - transform output: ${showCode(output)}")
                          }
                          traverse(done ++ output.shard, Nil, step.next)
                      }
                  }
                case Nil =>
                  ifVerbose {
                    println(s"""$step - done""")
                  }
                  val result = q"""
                    _root_.org.cvogt.flow.transform[$m]($returnName)(..$remainingTransformations) {
                      ($ctxNme: _root_.org.cvogt.flow.MonadContext[$m]) => ..$done
                    }
                  """
                  result
              }
            }

            val correctlyTyped = {
              if (currentTransform.isTyped) body
              else c.untypecheck(body)
            }

            ifBrowse {
              visualizer.visualize(s"$currentTransformName input" -> correctlyTyped)
            }

            val result = traverse(
              Nil,
              correctlyTyped.children,
              Step()
            )

            ifBrowse {
              visualizer.visualize(s"$currentTransformName output" -> result)
            }

            ifDebug {
              println(s"$currentTransform output:")
              println(showCode(result))
              println("====================")
            }
            result
          case None =>
            c.abort(t.pos, "unknown transformation phase")
        }
      case other :: rest =>
        c.abort(other.pos, s"transformation phases must be identified with literal Strings")
    }
  }


//   //   def doContextTransforms(next: Transformation) =
//   //     Transformation("context transform") { t =>

//   //       sealed trait ScopeOp
//   //       case class DefineVal(valDef: ValDef) extends ScopeOp
//   //       case class DefineObject(modDef: ModuleDef) extends ScopeOp
//   //       case class DefineDef(defDef: DefDef) extends ScopeOp
//   //       case class ImportSomething(imported: Import) extends ScopeOp

//   //       // For now, we'll ignore local classes and traits, but we should support them
//   //       case class DefineClass(defined: ClassDef) extends ScopeOp

//   //       def doContextTransform(
//   //         trees: List[Tree],
//   //         scopeOps: List[ScopeOp],
//   //         done: List[Tree]
//   //       ): Tree = trees match {
//   //         case Nil => next.transform(done.unify)
//   //         case (v: ValDef) :: rest if !v.mods.hasFlag(Flag.ARTIFACT) =>
//   //           doContextTransform(
//   //             rest,
//   //             scopeOps :+ DefineVal(v),
//   //             done :+ v
//   //           )
//   //         case (d: DefDef) :: rest =>
//   //           doContextTransform(
//   //             rest,
//   //             scopeOps :+ DefineDef(d),
//   //             done :+ d
//   //           )
//   //         case (m: ModuleDef) :: rest =>
//   //           doContextTransform(
//   //             rest,
//   //             scopeOps :+ DefineObject(m),
//   //             done :+ m
//   //           )
//   //         case (i: Import) :: rest =>
//   //           doContextTransform(
//   //             rest,
//   //             scopeOps :+ ImportSomething(i),
//   //             done :+ i
//   //           )
//   //         case q"""
//   //           $contextName.! { ($nme1: $tpe1) =>
//   //             $nme.$op { ..$stats; ($nme2: $tpe2) => $expr }
//   //           }
//   //         """ :: rest =>
//   //           val body = q"..$stats; $expr"
//   //           val prefix = next.transform(q"..$done")
//   //           val paramName = TermName(c.freshName("param"))
//   //           val transformed = q"$prefix.$op(($paramName: Any) => $body)"
//   //           val continued = doContextTransform(rest, Nil, Nil)
//   //           q"$transformed.flatMap { (paramName: Any) => $continued }"
//   //         case other :: rest =>
//   //           doContextTransform(
//   //             rest,
//   //             scopeOps,
//   //             done :+ other
//   //           )
//   //       }
//   //       doContextTransform(t.shard, Nil, Nil)
//   //     }


  def flat[M: c.WeakTypeTag, T: c.WeakTypeTag](comprehension: Tree): Tree = {
    if (verbose) {
      println(s"got: $comprehension")
    }

    val withParam = comprehension match {
      case q"($ctxParam) => $body" => comprehension
      case other =>
        val transformer = new Transformer {
          override def transform(t: Tree): Tree = t match {
            case q"org.cvogt.flow.`package`.allowOmitedContextParam[..$targs]($body)" =>
              super.transform(body)
            case other => super.transform(other)
          }
        }
        val transformed = transformer.transform(other)
        val paramName = TermName(c.freshName("ctx"))
        q"($paramName: ${TypeTree()}) => $transformed"
    }
    c.untypecheck(withParam) match {
      case q"($ctxParam) => $body" =>

        if (verbose) {
          println(s"transformed to: $body")
        }

        val utils = new TransformUtils[c.type](c) {
          val M = weakTypeOf[M].typeConstructor
          val contextName = ctxParam.name
        }
        import utils._

        val nonEmptyBody = body match {
          case EmptyTree => q"()"
          case nonEmpty => nonEmpty
        }

        val returnName = TermName(c.freshName("return"))

        val withCorrectReturnValue = {
          val lines = nonEmptyBody.shard
          (lines.init :+ q"val $returnName = ${liftM(lines.last)}" :+ q"$returnName").unify
        }

        val function = q"""{ ($ctxParam) =>
          $withCorrectReturnValue
        }
        """

        val transformNames = allTransforms.map(p => Literal(Constant(p.name)))
        val result = q"""
          _root_.org.cvogt.flow.transform[$M][${weakTypeOf[T]}](${Literal(Constant(returnName.toString))})(..$transformNames)($function)
        """
        if (verbose) {
          println(s"transformed to: $result")
        }

        result
    }
  }

  def echo[T](t: Tree): Tree = t

  def runtime(t: Tree): Tree = {
    val ru = internal.gen.mkRuntimeUniverseRef
    val rm = EmptyTree
    val rt = c.reifyTree(ru, rm, t)
    q"$rt.tree"
  }
}
