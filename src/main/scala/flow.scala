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
    (phases: String*)
    (comprehension: MonadContext[M] => T): M[T] =
      macro FlowMacros.transform[M[_]]
}

import scala.reflect.macros.blackbox
class FlowMacros(val c: blackbox.Context) {
  import c.universe._
  import c.weakTypeOf

  val debug =
    sys.props.get("flat_debug_transformations").getOrElse("").split(",").toSet

  def transform[M: WeakTypeTag](phases: Tree*)(comprehension: Tree): Tree = {
    comprehension match {
      case q"($ctxNme: $tpe) => $body" =>
        val m = weakTypeOf[M].typeConstructor
        phases match {
          case Nil => body
          case (t@Literal(Constant(currentPhaseName: String))) :: rest =>
            allPhases.find(_.name == currentPhaseName) match {
              case Some(currentPhase) =>
                val context = new TransformContext {
                  override def next(t: Tree): Tree = {
                    if (rest.isEmpty) t
                    else q"transform[$m](..$rest) { ($ctxNme: ${TypeTree()}) => $t }"
                  }
                  override def recur(t: Tree): Tree =
                    currentPhase.transform(this, body)
                  override val M = m
                  override val contextName = ctxNme
                }
                if (debug(currentPhaseName)) {
                  println("====================")
                  println(s"running $currentPhaseName")
                  println("input:")
                  println(showCode(comprehension))
                }
                val result = currentPhase.transform(context, body)
                if (debug(currentPhaseName)) {
                  println("output:")
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

  abstract class TransformContext {

    def recur(t: Tree): Tree
    def next(t: Tree): Tree
    val M: Type
    val contextName: TermName

    def liftM(t: Tree): Tree =
      q"_root_.org.cvogt.flow.Constructor[$M].create($t)"

    def hasExtracts(t: Tree): Boolean = {
      var foundExtraction = false
      new Traverser {
        override def traverse(t: Tree): Unit = {
          if (foundExtraction || isExtract(t)) foundExtraction = true
          else super.traverse(t)
        }
      }.traverse(t)
      foundExtraction
    }

    def noExtracts(t: Tree): Boolean = !hasExtracts(t)

    def isExtract(t: Tree): Boolean = t match {
      case q"$contextName.?[$tpe]($body)" => true
      case other => false
    }

    def notExtract(t: Tree): Boolean = !isExtract(t)

    object Extract {
      def apply(body: Tree, tpe: Tree): Tree = q"$contextName.?[$tpe]($body)"
      def unapply(t: Tree): Option[(Tree, Tree)] = t match {
        case q"$contextName.?[$tpe]($body)" => Some((body, tpe))
        case other => None
      }
    }

    implicit class TreeOps(t: Tree) {
      def shard: List[Tree] = {
        val q"..${result: List[Tree]}" = t
        result
      }
    }

    implicit class ListTreeOps(t: List[Tree]) {
      def unify: Tree = q"..$t"
    }
  }


  case class Phase(name: String)(val transform: (TransformContext, Tree) => Tree)

  val allPhases: List[Phase] = List(

    Phase("performImplicitExtracts") { (ctx, t) =>
      import ctx._
      def doTransformation(
        pending: List[Tree],
        done: List[Tree]
      ): Tree = pending match {
        case Nil => q"..$done"
        case last :: Nil =>
          val results = done :+ last
          q"..$results"
        case statement :: rest =>
          val t = if (statement.tpe.typeSymbol == M.typeSymbol) {
            val tpe = statement.tpe.baseType(M.typeSymbol)
            Extract(statement, TypeTree(tpe.typeArgs(0)))
          } else statement
          doTransformation(rest, done :+ t)
      }
      next(doTransformation(t.shard, Nil))
    },

    Phase("comprehensiveComprehsions") { (ctx, t) =>
      import ctx._
      next(t)
    },

    Phase("normalize") { (ctx, t) =>
      import ctx._
      def doNormalize(pending: List[Tree], done: List[Tree]): Tree = pending match {

        case Nil => done.unify

        case first :: rest if noExtracts(first) =>
          doNormalize(rest, done :+ first)

        case h :: rest if hasExtracts(h) && !h.isInstanceOf[ValDef @unchecked] =>
          val nme = TermName(c.freshName("anon"))
          val preserveFinalExpress = {
            if (rest.isEmpty) List(q"$nme")
            else Nil
          }
          doNormalize(q"val $nme = $h" +: preserveFinalExpress ::: rest, done)

        case q"..$body" :: rest if body.size > 1 && (hasExtracts(body.unify)) =>
          doNormalize(
            rest,
            Extract(recur(body.unify), TypeTree()).shard
          )

        case (v@q"$mods val $nme: $tpe1 = ${Extract(e, tpe2)}") :: rest
            if hasExtracts(e) =>
          val tmpNme = TermName(c.freshName("anon"))
          val split = List(
            q"val $tmpNme = $e",
            q"val nme: $tpe1 = ${Extract(q"$tmpNme", tpe2)}"
          )
          doNormalize(split ++ rest, done)

        case (v@q"$mods val $nme: $tpe = ${Extract(e, _)}") :: rest if noExtracts(e) =>
          doNormalize(rest, done :+ v)

        case q"$mods val $nme: $tpe = $pre.$op[..$targs](...$vargs)" :: rest
            if hasExtracts(pre) =>
          val tmpNme = TermName(c.freshName("anon"))
          val split = List(
            q"val $tmpNme = $pre",
            q"$mods val $nme: $tpe = $tmpNme.$op[..$targs](...$vargs)"
          )
          doNormalize(split ++ rest, done)

        case q"$mods val $nme: $tpe = $pre match { case ..$body }" :: rest
            if hasExtracts(pre) =>
          val tmpNme = TermName(c.freshName("anon"))
          val split = List(
            q"val $tmpNme = $pre",
            q"$mods val $nme: $tpe = $tmpNme match { case ..$body }"
          )
          doNormalize(split ++ rest, done)

        case q"$mods val $nme: $tpe = $pre.$op[..$targs](...$vargs)" :: rest
            if vargs.exists(_.exists(hasExtracts)) =>
          val (newArgss, defs) = {
            vargs.foldLeft((List.empty[List[Tree]], List.empty[ValDef])) {
              case ((processedArgss, defs), newArgs) =>
                val (p, d) =
                  newArgs.foldLeft(List.empty[Tree], List.empty[ValDef]) {
                    case ((processedArgs, defs), newArg) =>
                      if (hasExtracts(newArg)) {
                        val tmpNme = TermName(c.freshName("arg"))
                        newArg match {
                          case AssignOrNamedArg(lhs, rhs) =>
                            (
                              processedArgs :+ AssignOrNamedArg(lhs, q"$tmpNme"),
                              defs :+ q"val $tmpNme = $rhs"
                            )
                          case other =>
                            (
                              processedArgs :+ q"$tmpNme",
                              defs :+ q"val $tmpNme = $other"
                            )
                        }
                      } else (processedArgs :+ newArg, defs)
                  }
                (processedArgss :+ p, defs ++ d)
            }
          }

          doNormalize(
            defs ::: q"$mods val $nme: $tpe = $pre.$op[..$targs](...$newArgss)" :: rest,
            done
          )
      }
      next(doNormalize(t.shard, Nil))
    },

    Phase("rewriteExtractions") { (ctx, t) =>
      import ctx._
      val reversed = c.untypecheck(t).shard.reverse
      def rewrite(
        remaining: List[Tree],
        done: List[Tree]
      ): Tree = remaining match {
        case Nil => done.unify
        case (v@ValDef(_, `contextName`, _, _)) :: rest =>
          c.abort(v.pos, s"Duplicate definition for $contextName")
        case (v@DefDef(_, `contextName`, _, _, _, _)) :: rest =>
          c.abort(v.pos, s"Duplicate definition for $contextName")
        case ValDef(
          mods, name, valTpe, Extract(extracted, tpe)
        ) :: rest if !mods.hasFlag(Flag.LAZY) =>
          val param = TermName(c.freshName("param"))
          rewrite(
            rest,
            q"""
              $extracted.flatMap { ($param: $tpe) =>
                $mods val $name: $valTpe = $param
                ..$done
              }
            """.shard
          )
        case q"$contextName.?[$paramTpe]($extracted)" :: rest =>
          val param = TermName(c.freshName("param"))
          rewrite(
            rest,
            q"""
                $extracted.flatMap { ($param: $paramTpe) =>
                  $done
                }
            """ +: done
          )
        case other :: rest =>
          rewrite(
            rest,
            other +: done
          )
      }
      next(rewrite(liftM(reversed.head) :: reversed.tail, Nil))
    }
  )

  //   def doContextTransforms(next: Transformation) =
  //     Transformation("context transform") { t =>

  //       sealed trait ScopeOp
  //       case class DefineVal(valDef: ValDef) extends ScopeOp
  //       case class DefineObject(modDef: ModuleDef) extends ScopeOp
  //       case class DefineDef(defDef: DefDef) extends ScopeOp
  //       case class ImportSomething(imported: Import) extends ScopeOp

  //       // For now, we'll ignore local classes and traits, but we should support them
  //       case class DefineClass(defined: ClassDef) extends ScopeOp

  //       def doContextTransform(
  //         trees: List[Tree],
  //         scopeOps: List[ScopeOp],
  //         done: List[Tree]
  //       ): Tree = trees match {
  //         case Nil => next.transform(done.unify)
  //         case (v: ValDef) :: rest if !v.mods.hasFlag(Flag.ARTIFACT) =>
  //           doContextTransform(
  //             rest,
  //             scopeOps :+ DefineVal(v),
  //             done :+ v
  //           )
  //         case (d: DefDef) :: rest =>
  //           doContextTransform(
  //             rest,
  //             scopeOps :+ DefineDef(d),
  //             done :+ d
  //           )
  //         case (m: ModuleDef) :: rest =>
  //           doContextTransform(
  //             rest,
  //             scopeOps :+ DefineObject(m),
  //             done :+ m
  //           )
  //         case (i: Import) :: rest =>
  //           doContextTransform(
  //             rest,
  //             scopeOps :+ ImportSomething(i),
  //             done :+ i
  //           )
  //         case q"""
  //           $contextName.! { ($nme1: $tpe1) =>
  //             $nme.$op { ..$stats; ($nme2: $tpe2) => $expr }
  //           }
  //         """ :: rest =>
  //           val body = q"..$stats; $expr"
  //           val prefix = next.transform(q"..$done")
  //           val paramName = TermName(c.freshName("param"))
  //           val transformed = q"$prefix.$op(($paramName: Any) => $body)"
  //           val continued = doContextTransform(rest, Nil, Nil)
  //           q"$transformed.flatMap { (paramName: Any) => $continued }"
  //         case other :: rest =>
  //           doContextTransform(
  //             rest,
  //             scopeOps,
  //             done :+ other
  //           )
  //       }
  //       doContextTransform(t.shard, Nil, Nil)
  //     }



  /** like identity but prints desugared code and tree */
  def debugMacro(tree: Tree): Tree = {
    println("code:\n  "+tree)
    println("Tree:\n  "+showRaw(tree))
    tree
  }

  def flat[M: c.WeakTypeTag, T: c.WeakTypeTag](comprehension: Tree): Tree = {
    comprehension match {
      case q"($contextName: $tpe) => $body" =>

        val nonEmptyBody = body match {
          case EmptyTree => q"()"
          case nonEmpty => nonEmpty
        }

        val debug = sys.props.get("flat_debug").map(_.toBoolean).getOrElse(false)

        val M = weakTypeOf[M].typeConstructor

        val phaseNames = allPhases.map(p => Literal(Constant(p.name)))
        q"""
          _root_.org.cvogt.flow.transform[$M][${weakTypeOf[T]}](..$phaseNames)($comprehension)
        """
    }
  }
}
