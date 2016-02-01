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
  //@compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  // We can't make this implicit because then it is used whenever something does not have an apply
  // which leads to weird error messages, e.g. Baz.apply(5) Expected FlowContext Found Int
  //implicit def enableFlowScope[T](t: T): FlowContext => T = ???

  implicit object OptionConstructor extends Constructor[Option]{
    def create[T](v: T) = Some(v)
  }
  import scala.concurrent._
  implicit def FutureConstructor: Constructor[Future] = new Constructor[Future]{
    def create[T](v: T) = Future.successful(v)
  }
  import collection.generic.CanBuildFrom
  /** Generic Constructor for anything that has a CanBuildFrom.*/
  implicit def CanBuildFromConstructor[M[_]](implicit cbf: CanBuildFrom[_, AnyRef, M[AnyRef]]) = new Constructor[M]{
    // Requires some cheating on the type-level. Assuming it's sound in relevant cases due to lack of counter examples.
    // Worst case, we have to replace this with manual instances for all important types.
    // That would probably also be more performant because of fewer allocations.
    def create[T](v: T) = {
      val b = cbf.asInstanceOf[ CanBuildFrom[_, T, M[T]] ]()
      b += v
      b.result
    }
  }

  //@compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  //implicit def unpackFlowContextMonad[M[_],T](t: T): M[FlowContext] => T = ???

  object implicits{
    import scala.language.implicitConversions
    /** careful, this can lead to surprising effects */
    implicit def autoEmbed[M[_],T](m: M[T]): T = ???
  }
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  implicit class Embed[M[_],T](m: M[T]){
    /**
    Adds a monad to the surrounding comprehension
    
    Analog to .value in sbt and await in scala-async

    Alternative name candidates or potential aliases: value, ~, ?, &, embed, flow, in, enter, open, dive, each

    flat[M]{ c =>
      val x = c?xs
      ,,,
    }

    is identical to 

    for{
      x <- xs
      ...
    } yield ...

    */
    @compileTimeOnly("the prefix ? operator can only be used in a flow comprehension scope such as flat{...}, flow{...}")
    def unary_? : T = ???
  }
  /*
  implicit class Embed2[M[_],K[_],T](m: K[M[T]]){
    //@compileTimeOnly("the prefix ? operator can only be used in a flow comprehension scope such as flat{...}, flow{...}")
    def unary_? : T = ???
  }
  */

  /**
  Works just like for-comprehensions with extra power.

  Alternative name candidates: $, seq, >>

  Works sequentially, meaning in

  <code>
  flat[Future]{ c =>
    val a = c?Future(now)
    val b = now
    val c = c?Future(now)
    (a,b,c)
  }
  </code>

  a < b < c

  */
  def flat[M[_]] = new flat[M]
  /*
  // FIXME: Concerns: changing data flow could be unexpected to readers of the code. How can we ease that?

  /**
  Like for-comprehensions, but non-sequential when possible for better performance.

  Alternative name candidates: $$

  Works non-sequentially, meaning in

  <code>
  flat[Future]{ c =>
    val a = c?Future(now)
    val b = now
    val c = c?Future(now)
    (a,b,c)
  }
  </code>

  it is not guaranteed (even unlikely) that a < b < c.

  @see https://github.com/jedesah/computation-expressions#an-example-where-it-is-better-to-use-a-for-comprehension
  */
  class flow[M[_]]
  def flow[M[_]] = new flow[M]
  */
}

sealed abstract class Comprehension[M[_]]{ // FIXME: what happens if calling a method on this type that is implemented by macros in children? no such method error?
  //def apply[T](scope: M[FlowContext] => T): M[T]
  //def apply[T](scope: => T): M[T]
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
  //def apply[T](scope: => T): M[T] = ???
}
object debugMacro{
  def apply[T](tree: T): T = macro FlowMacros.debugMacro
}
/*
class flow[M[_]] extends Comprehension[M]{
  @compileTimeOnly("")
  def apply[T](comprehension: MonadContext[M] => T): M[T] = ???
  //def apply[T](scope: => T): M[T] = ???
}
*/

import scala.reflect.macros.blackbox
class FlowMacros(val c: blackbox.Context){
  import c.universe._
  import c.weakTypeOf

  val pkg = q"org.cvogt.flow.`package`"
  val conv = q"$pkg.omitFlowContextMonad"

  // object RemoveFlowContextTypeAnnotations extends Transformer {
  //   override def transform(tree: Tree) = {
  //     val t = tree match {
  //       case q"$mods val $name : $ttree = $_" if ttree.symbol == weakTypeOf[FlowContext].typeSymbol =>
  //         q"$mods val $name : ${TypeTree()}"
  //       case q"$lhsT[..$ttrees](...$argsT)" if ttrees.map(_.symbol).contains( weakTypeOf[FlowContext].typeSymbol ) =>
  //         q"$lhsT(...$argsT)"
  //       case q"$lhsT.$rhsT[..$ttrees](...$argsT)" if ttrees.map(_.symbol).contains( weakTypeOf[FlowContext].typeSymbol ) =>
  //         q"$lhsT.$rhsT(...$argsT)"
  //       case other => other
  //     }
  //     super.transform(t)
  //   }
  // }

  class Transformations(
    contextName: TermName,
    M: Type,
    liftM: Tree => Tree,
    debug: Boolean = true
  ) {

    case class Transformation(description: String)(_transform: Tree => Tree) {
      def transform(t: Tree): Tree = {
        if (debug) {
          println(s"==============")
          println(s"$description input:\n"++showCode(t))
          println(s"==============")
        }
        val res = _transform(t)
        if (debug) {
          println(s"==============")
          println(s"$description output:\n"++showCode(res))
          println(s"==============")
        }
        res
      }
    }

    val doNothing = new Transformation("identity")(identity) {
      override def transform(t: Tree) = t
    }

    def untypecheck(next: Transformation) =
      Transformation("untypecheck") { t =>
        next.transform(c.untypecheck(t))
      }

    def doImplicitExtracts(next: Transformation) =
      Transformation("implicit extracts") { t =>
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
        next.transform(doTransformation(t.shard, Nil))
      }

    def doContextTransforms(next: Transformation) =
      Transformation("context transform") { t =>

        sealed trait ScopeOp
        case class DefineVal(valDef: ValDef) extends ScopeOp
        case class DefineObject(modDef: ModuleDef) extends ScopeOp
        case class DefineDef(defDef: DefDef) extends ScopeOp
        case class ImportSomething(imported: Import) extends ScopeOp

        // For now, we'll ignore local classes and traits, but we should support them
        case class DefineClass(defined: ClassDef) extends ScopeOp

        def doContextTransform(
          trees: List[Tree],
          scopeOps: List[ScopeOp],
          done: List[Tree]
        ): Tree = trees match {
          case Nil => next.transform(done.unify)
          case (v: ValDef) :: rest if !v.mods.hasFlag(Flag.ARTIFACT) =>
            doContextTransform(
              rest,
              scopeOps :+ DefineVal(v),
              done :+ v
            )
          case (d: DefDef) :: rest =>
            doContextTransform(
              rest,
              scopeOps :+ DefineDef(d),
              done :+ d
            )
          case (m: ModuleDef) :: rest =>
            doContextTransform(
              rest,
              scopeOps :+ DefineObject(m),
              done :+ m
            )
          case (i: Import) :: rest =>
            doContextTransform(
              rest,
              scopeOps :+ ImportSomething(i),
              done :+ i
            )
          case q"""
            $contextName.! { ($nme1: $tpe1) =>
              $nme.$op { ..$stats; ($nme2: $tpe2) => $expr }
            }
          """ :: rest =>
            val body = q"..$stats; $expr"
            val prefix = next.transform(q"..$done")
            val paramName = TermName(c.freshName("param"))
            val transformed = q"$prefix.$op(($paramName: Any) => $body)"
            val continued = doContextTransform(rest, Nil, Nil)
            q"$transformed.flatMap { (paramName: Any) => $continued }"
          case other :: rest =>
            doContextTransform(
              rest,
              scopeOps,
              done :+ other
            )
        }
        doContextTransform(t.shard, Nil, Nil)
      }

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
        val q"..$result" = t
        result
      }
    }

    implicit class ListTreeOps(t: List[Tree]) {
      def unify: Tree = q"..$t"
    }

    def normalize(next: Transformation): Transformation = Transformation("normalize") { t =>

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
            Extract(normalize(next).transform(body.unify), TypeTree()).shard
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
      next.transform(doNormalize(t.shard, Nil))
    }

    def doExtractions(next: Transformation) = Transformation("extractions") { t =>
      val reversed = t.shard.reverse
      def doTransformation(
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
          doTransformation(
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
          doTransformation(
            rest,
            q"""
                $extracted.flatMap { ($param: $paramTpe) =>
                  $done
                }
            """ +: done
          )
        case other :: rest =>
          doTransformation(
            rest,
            other +: done
          )
      }
      doTransformation(liftM(reversed.head) :: reversed.tail, Nil)
    }

    val all = List(
      doImplicitExtracts(_),
      untypecheck(_),
      doContextTransforms(_),
      normalize(_),
      doExtractions(_)
    ).foldRight(doNothing)((t, next) => t(next))

  }

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
        val liftM: Tree => Tree = t =>
          q"_root_.org.cvogt.flow.Constructor[$M].create($t)"

        val transforms = new Transformations(contextName, M, liftM, debug)

        transforms.all.transform(nonEmptyBody)

        // val M = weakTypeOf[M]
        // val companion = M.typeSymbol.companion
        // def unit(tree: Tree) = q"implicitly[Constructor[$M]].create($tree)"

      //   def transformExtract(tree: Tree): (List[Tree], Tree) = {
      //     object transformer extends Transformer {
      //       val extracted = collection.mutable.MutableList[Tree]()
      //       override def transform(tree: Tree) = {
      //         val t = tree match {
      //           case t2@q"$flowContextUsage.?[$tpe]($expr)" if flowContextUsage.symbol == flowContext.symbol => 
      //             val name = c.freshName
      //             val (statements, texpr) = transformExtract(expr)
      //             val v = q"val ${TermName(name)}: $tpe = $flowContextUsage.?[$tpe]($texpr)"
      //             extracted ++= (statements :+ v)
      //             q"${Ident(TermName(name))}"

      //           case other => other
      //         }
      //         super.transform(t)
      //       }
      //     }
      //     val res = transformer.transform(tree)
      //     (transformer.extracted.to[List], res)
      //   }

      //   // untypechecking only seems to work reliably upfront.
      //   // As a consequence, all of the following processes pairs of
      //   // corresponding typed trees and untyped tres so both variants
      //   // are available as needed. The untyped trees are used
      //   // to build the eventual resulting tree. The typed trees
      //   // are used to examine the type information of the input
      //   // trees.
      //   // value names ending in T mean typed trees from here on.
      //   // The same names excluding the T mean the corresponding untyped trees
      //   (e, c.untypecheck(e)) match {
      //     case (
      //       q"""..$statementsT; $resultT""", // <- 
      //       q"""..$statements;  $result """  // <- 
      //     ) =>
      //       /* The following folds over the comprehension statements from top to bottom.
      //        * Every step results in a new scope (the names of values bound by the
      //        * comprehension at this point) and a context, which is a function that takes
      //        * a tree and embeds it into another tree that binds the required
      //        * values. The context is effectively a continuation, which allows
      //        * folding forward throw the flat of statements while allowing
      //        * the following statment decide how it handles the continuation of previous
      //        * statments. This allows closing the iteration and assigning the result to
      //        * a val for example, in order to call contextual transformers
      //        * (sortBy, filter, ...) on the iteration up to this point.
      //       */

      //       val (additionalStatementsT, expressionT) = transformExtract(resultT)
      //       val (additionalStatements, expression) = transformExtract(result)

      //       val statementPairs = (
      //         (statementsT.flatMap(transformExtract(_) match{ case (list,one) => list :+ one }) ++ additionalStatementsT)
      //         zip
      //         (statements.flatMap(transformExtract(_) match{ case (list,one) => list :+ one }) ++ additionalStatements)
      //       )

      //       val (_, continuation) = statementPairs.foldLeft(
      //         ( List[(TermName,Tree)](), identity[Tree] _ )
      //       ){
      //         case (
      //           ( scope, context ),
      //           (
      //             valdefT @ q"val $nameT: $tpeT = $flowContextUsage.?[$t]($mT)",
      //             valdef  @ q"val $name : $tpe  = $flowContextUsageT.?[$tT]($m )"
      //           )
      //         )  if flowContextUsage.symbol == flowContext.symbol && flowContextUsageT.symbol == flowContext.symbol
      //          =>
      //           val param = q"val $name: ${TypeTree()}"
      //           (
      //             // omit generated aliases from being captured - users can't refer to them anyways
      //             if(name.toString.startsWith("fresh$macro$")) scope else ((name, tpeT) :: scope),
      //             continue => context(q"$m.flatMap( $param => $continue )")
      //           )
      //         case ( ( scope, context ), (q"$ctxT.!($transformerT)", q"$ctx.!($transformer)") ) if ctx.symbol == flowContext.symbol =>
      //           val boundNames = scope.map(_._1).map(Ident.apply _)

      //           object ReplaceFlowScope extends Transformer {
      //             override def transform(tree: Tree) = {
      //               val t = tree match {
      //                 case t@q"($arg) => $expr" if arg.tpt.tpe != null && arg.tpt.tpe =:= typeOf[FlowContext] =>
      //                   val pattern =  pq"(..${scope.map(_._1)})"
      //                   // FIXME: can we move the extractor below into the argument?
      //                   q"""{
      //                     arg =>
      //                       val $pattern = arg
      //                       ${transform(expr)}
      //                   }"""
      //                 case other => other
      //               }
      //               super.transform(t)
      //             }
      //           }
      //           val captured = context{
      //             unit(q"(..$boundNames)")
      //           }
      //           val params = scope.map{ case(name, tpe) => ValDef(Modifiers(Flag.PARAM),name,tpe,EmptyTree) }
      //           val before = transformer match {
      //             case q"($arg) => $expr" => 
      //               val q"$_ val $name: $_ = $_" = arg
      //               q"""
      //                 val $name = $captured
      //                 $expr
      //               """
      //             case q"$other" => q"$other($captured)"
      //           }
      //           //println("before:"+before)
      //           val closed =  ReplaceFlowScope.transform(before)
      //           //println("closed:"+closed)
      //           (scope, continue => {
      //             val func =
      //               if(params.size > 1)
      //                 q"((..$params) => $continue).tupled"
      //               else
      //                 q"((..$params) => $continue)"
      //             val name = c.freshName
      //             q"""
      //               val ${TermName(name)} = $closed
      //               ${Ident(TermName(name))}.flatMap{
      //                 $func            
      //               }
      //             """
      //           })
      //         case (
      //           ( scope, context ),
      //           (
      //             valdefT @ q"val $nameT: $tpeT = $otherT",
      //             valdef  @ q"val $name : $tpe  = $other"
      //           )
      //         ) =>
      //           (
      //             (name, tpeT) :: scope,
      //             continue => context(q"$valdef; $continue")
      //           )
      //         case ( ( scope, context ), (otherT, other) ) => 
      //           (scope, continue => context(q"$other; $continue"))
      //       }
      //       val res = continuation(unit(expression))
      //       //println(e)
      //       //println(res)
      //       //println(showRaw(res))
      //       RemoveFlowContextTypeAnnotations.transform(res)
      //   }
      // case x => throw new Exception(x.toString)
    }
  }
}
