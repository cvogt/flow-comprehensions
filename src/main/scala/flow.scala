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
    liftM: Tree => Tree
  ) {
    val doContextTransforms: Tree => Tree = identity
    val normalize: Tree => Tree = identity

    val doExtractions: Tree => Tree = { t =>
      val q"..${statements: List[Tree]}" = t
      val (processed, returned) = {
        if (statements.isEmpty) (List.empty[Tree], q"()": Tree)
        else (statements.init: List[Tree], statements.last: Tree)
      }
      processed.foldRight(liftM(returned)) { (statement, done) =>
        statement match {
          case v@ValDef(_, `contextName`, _, _) =>
            c.abort(v.pos, s"Duplicate definition for $contextName")
          case v@DefDef(_, `contextName`, _, _, _, _) =>
            c.abort(v.pos, s"Duplicate definition for $contextName")
          case ValDef(
            mods, name, valTpe, q"$contextName.?[$paramTpe]($extracted)"
          ) if !mods.hasFlag(Flag.LAZY) =>
            val param = TermName(c.freshName("param"))
            q"""
              $extracted.flatMap { ($param: $paramTpe) =>
                $mods val $name: $valTpe = $param
                $done
              }
            """
          case q"$contextName.?[$paramTpe]($extracted)" =>
            val param = TermName(c.freshName("param"))
            q"""
              $extracted.flatMap { ($param: $paramTpe) =>
                $done
              }
            """
          case other => q"$other; $done"
        }
      }
    }

    val transform: Tree => Tree =
      doContextTransforms andThen normalize andThen doExtractions
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

        val M = weakTypeOf[M].typeConstructor
        val liftM: Tree => Tree = t =>
          q"_root_.org.cvogt.flow.Constructor[$M].create($t)"

        val untyped = c.untypecheck(body)

        val transformer = new Transformations(contextName, liftM)

        transformer.transform(untyped)

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
