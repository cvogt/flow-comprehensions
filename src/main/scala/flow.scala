package org.cvogt.flow

import scala.language.higherKinds
import scala.annotation.compileTimeOnly
import scala.language.implicitConversions
import scala.language.experimental.macros

@compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
final class FlowContext private()

abstract class Constructor[M[_]]{
  def create[T](v: T): M[T]
}
object Constructor {
  def apply[M[_]](implicit c: Constructor[M]) = c
}

object `package`{

  implicit object OptionConstructor extends Constructor[Option]{
    def create[T](v: T) = Some(v)
  }
  import scala.concurrent._
  implicit def FutureConstructor: Constructor[Future] = new Constructor[Future]{
    def create[T](v: T) = Future.successful(v)
  }
  import collection.generic.CanBuildFrom
  implicit def CanBuildFromConstructor[M[_]](implicit cbf: CanBuildFrom[_, AnyRef, M[AnyRef]]) = new Constructor[M]{
    def create[T](v: T) = {
      val b = cbf.asInstanceOf[ CanBuildFrom[_, T, M[T]] ]()
      b += v
      b.result
    }
  }

  object implicits{
    import scala.language.implicitConversions
    /** careful, this can lead to surprising effects */
    implicit def autoEmbed[M[_],T](m: M[T]): T = ???
  }
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  implicit class Embed[M[_],T](m: M[T]){
    @compileTimeOnly("the prefix ? operator can only be used in a flow comprehension scope such as flat{...}, flow{...}")
    def unary_? : T = ???
  }

  def flat[M[_]] = new flat[M]
  def transform[M[_]] = new transform[M]
}

sealed abstract class Comprehension[M[_]] {
}
final class MonadContext[M[_]]{
  /** transform the surrounding monad at this point */
  @compileTimeOnly("The MonadContext type only makes sense in a flow comprehension scope and is supposed to be removed by the macro.")
  def !(transform: M[FlowContext] => M[FlowContext]) = ???
  /** extract a value from a given Monad */
  @compileTimeOnly("The MonadContext type only makes sense in a flow comprehension scope and is supposed to be removed by the macro.")
  def ?[T](monad: M[T]): T = ???
}
class flat[M[_]] extends Comprehension[M]{
  def apply[T](comprehension: MonadContext[M] => T): M[T] = macro FlowMacros.flat[M[_],T]
}
class transform[M[_]] {
  def apply[T]
    (transforms: String*)
    (comprehension: MonadContext[M] => M[T]): M[T] =
      macro FlowMacros.transform[M[_]]
}

import scala.reflect.macros.blackbox
trait Contextual {
  val c: blackbox.Context
}

class FlowMacros(val c: blackbox.Context) extends Contextual {
  import c.universe._
  import c.weakTypeOf

  val debug =
    sys.props.get("flat_debug_transforms").getOrElse("").split(",").toSet

  val verbose =
    sys.props.get("flat_debug_verbose") match {
      case Some("true") => true
      case _ => false
    }

  import org.cvogt.flow.transforms._

  val allTransforms = Seq(
    Normalize,
    RewriteExtractions
  )

  def transform[M: WeakTypeTag](transforms: Tree*)(comprehension: Tree): Tree = {

    comprehension match {

      case q"($ctxNme: $tpe) => $body" =>

        val m = weakTypeOf[M].typeConstructor

        transforms match {
          case Nil => body
          case (t@Literal(Constant(currentTransformName: String))) :: remainingTransformations =>
            def ifDebug(body: => Unit): Unit = {
              if(debug(currentTransformName)) body
              else ()
            }
            def ifVerbose(body: => Unit): Unit = {
              if (debug(currentTransformName) && verbose) body
              else ()
            }
            val ctx = new TransformContext[c.type](c) {
              val M: Type = m
              val contextName: TermName = ctxNme
              def recur(t: Tree): Tree = {
                q"""
                  _root_.org.cvorg.flow.transform[$m](..${transforms})(($ctxNme: _root_.org.cvogt.flow.MonadContext[$m]) => $t)
                """
              }
            }
            import ctx._

            allTransforms.find(_.name == currentTransformName) match {
              case Some(currentTransform) =>
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
                      ifVerbose {
                        println(s"""$step - input: $h"""")
                      }
                      currentTransform.rewrites[c.type](ctx).find { rw =>
                        rw.pf.isDefinedAt(h)
                      } match {
                        case None =>
                          ifVerbose {
                            println(s"""$step - error: no matching rewrite rule"""")
                          }
                          c.abort(c.enclosingPosition, "no matching rewrite rule for "++showCode(h))
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
                            case RewriteTo(replacement) =>
                              ifVerbose {
                                println(s"""$step - result: rewrite line""")
                                println(s"""$step - rewriteTo: ${showCode(replacement)}""")
                              }
                              traverse(done, replacement.shard ++ t, step.next)
                            case TransformRest(f) =>
                              ifVerbose {
                                println(s"""$step - result: recur""")
                              }
                              val input = traverse(Nil, t, step.sub)
                              val transformed = f(input)
                              val output = q"""
                                  _root_.org.cvogt.flow.transform[$m](..$remainingTransformations) {
                                    ($ctxNme: _root_.org.cvogt.flow.MonadContext[$m]) => $transformed
                                  }
                                """
                              ifVerbose {
                                println(s"""$step - transform input: ${showCode(input)}""")
                                println(s"$step - transform output: ${showCode(output)}")
                              }
                              output
                          }
                      }
                    case Nil =>
                      ifVerbose {
                        println(s"""$step - done""")
                      }
                      q"""
                        _root_.org.cvogt.flow.transform[$m](..$remainingTransformations) {
                          ($ctxNme: _root_.org.cvogt.flow.MonadContext[$m]) => ..$done
                        }
                      """
                  }
                }

                val result = traverse(
                  Nil,
                  if (currentTransform.isTyped) body.shard
                  else c.untypecheck(body).shard,
                  Step()
                )

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
  }

//     Phase("performImplicitExtracts") { (ctx, t) =>
//       import ctx._
//       def doTransformation(
//         pending: List[Tree],
//         done: List[Tree]
//       ): Tree = pending match {
//         case Nil => q"..$done"
//         case last :: Nil =>
//           val results = done :+ last
//           q"..$results"
//         case statement :: rest =>
//           val t = if (statement.tpe.typeSymbol == M.typeSymbol) {
//             val tpe = statement.tpe.baseType(M.typeSymbol)
//             Extract(statement, TypeTree(tpe.typeArgs(0)))
//           } else statement
//           doTransformation(rest, done :+ t)
//       }
//       next(doTransformation(t.shard, Nil))
//     },

//     Phase("comprehensiveComprehsions") { (ctx, t) =>
//       import ctx._
//       next(t)
//     },


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



//   /** like identity but prints desugared code and tree */
//   def debugMacro(tree: Tree): Tree = {
//     println("code:\n  "+tree)
//     println("Tree:\n  "+showRaw(tree))
//     tree
//   }

  def flat[M: c.WeakTypeTag, T: c.WeakTypeTag](comprehension: Tree): Tree = {
    comprehension match {
      case q"($contextName: $tpe) => $body" =>

        val m = weakTypeOf[M].typeConstructor

        val nonEmptyBody = body match {
          case EmptyTree => q"()"
          case nonEmpty => nonEmpty
        }

        def liftM(t: Tree): Tree =
          q"_root_.org.cvogt.flow.Constructor[$m].create($t)"

        val withCorrectReturnValue = {
          val q"""
            ..$rest
            $last
          """ = nonEmptyBody
          q"""
            ..$rest
            ${liftM(last)}
          """
        }

        val debug = sys.props.get("flat_debug").map(_.toBoolean).getOrElse(false)

        val M = weakTypeOf[M].typeConstructor

        val transformNames = allTransforms.map(p => Literal(Constant(p.name)))
        q"""
          _root_.org.cvogt.flow.transform[$M][${weakTypeOf[T]}](..$transformNames)(($contextName: $tpe) => $withCorrectReturnValue)
        """
    }
  }
}
