package org.cvogt.flow

import scala.language.higherKinds
import scala.annotation.compileTimeOnly
import scala.language.implicitConversions
import scala.language.experimental.macros

final class FlowContext private()

object `package`{
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  implicit def omitFlowContext[T](t: T): FlowContext => T = ???
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  implicit def omitFlowContextMonad[M[_],T](t: T): M[FlowContext] => T = ???

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

    Alternative name candidates or potential aliases: value, ~, &, embed, flow, in, enter, open, dive, each

    sequence{
      val x = ~xs
      ,,,
    }

    is identical to 

    for{
      x <- xs
      ...
    } yield ...

    */
    @compileTimeOnly("the prefix ~ operator can only be used in a flow comprehension scope such as sequence{...}, flow{...}")
    def unary_~ : T = ???
  }
  /*
  implicit class Embed2[M[_],K[_],T](m: K[M[T]]){
    //@compileTimeOnly("the prefix ~ operator can only be used in a flow comprehension scope such as sequence{...}, flow{...}")
    def unary_~ : T = ???
  }
  */

  /**
  Works just like for-comprehensions with extra power.

  Alternative name candidates: $, seq, >>

  Works sequentially, meaning in

  <code>
  sequence[Future]{
    val a = ~Future(now)
    val b = now
    val c = ~Future(now)
    (a,b,c)
  }
  </code>

  a < b < c

  */
  def sequence[M[_]] = new sequence[M]
  // FIXME: Concerns: changing data flow could be unexpected to readers of the code. How can we ease that?

  /**
  Like for-comprehensions, but non-sequential when possible for better performance.

  Alternative name candidates: $$

  Works non-sequentially, meaning in

  <code>
  flow[Future]{
    val a = ~Future(now)
    val b = now
    val c = ~Future(now)
    (a,b,c)
  }
  </code>

  it is not guaranteed (even unlikely) that a < b < c.

  @see https://github.com/jedesah/computation-expressions#an-example-where-it-is-better-to-use-a-for-comprehension
  */
  class flow[M[_]]
  def flow[M[_]] = new flow[M]

}

sealed abstract class Comprehension[M[_]]{ // FIXME: what happens if calling a method on this type that is implemented by macros in children? no such method error?
  //def apply[T](scope: M[FlowContext] => T): M[T]
  //def apply[T](scope: => T): M[T]
}
class sequence[M[_]] extends Comprehension[M]{
  def apply[T](comprehension: M[FlowContext] => T): M[T] = macro FlowMacros.sequence[M[_],T]
  //def apply[T](scope: => T): M[T] = ???
}

class flow[M[_]] extends Comprehension[M]{
  @compileTimeOnly("")
  def apply[T](comprehension: M[FlowContext] => T): M[T] = ???
  //def apply[T](scope: => T): M[T] = ???
}

import scala.reflect.macros.blackbox
class FlowMacros(val c: blackbox.Context){
  import c.universe._

  val pkg = q"org.cvogt.flow.`package`"
  val conv = q"$pkg.omitFlowContextMonad"
  /*
  object MyTransformer extends Transformer {
    override def transform(tree: Tree) = {
      val t = tree match {
        //case q"~$v" => 
        case _ => tree
      }
      super.transform(t)
    }
  }
  */

  def sequence[M: c.WeakTypeTag, T: c.WeakTypeTag](comprehension: Tree): Tree = {
    comprehension match {
      case q"($context) => $e" =>
        val companion = weakTypeOf[M].typeSymbol.companion
        def unit(tree: Tree) = q"$companion.apply($tree)" // FIXME: this isn't great for Futures, is it?

        // untypechecking only seems to work reliably upfront.
        // As a consequence, all of the following processes pairs of
        // corresponding typed trees and untyped tres so both variants
        // are available as needed. The untyped trees are used
        // to build the eventual resulting tree. The typed trees
        // are used to examine the type information of the input
        // trees.
        // value names ending in T mean typed trees from here on.
        // The same names excluding the T mean the corresponding untyped trees

        (e, c.untypecheck(e)) match {
          case (
            q"""..$statementsT; $resultT""", // <- 
            q"""..$statements;  $result """  // <- 
          ) =>
            /* The following folds over the comprehension statements from top to bottom.
             * Every step results in a new scope (the names of values bound by the
             * comprehension at this point) and a context, which is a function that takes
             * a tree and embeds it into another tree that binds the required
             * values. The context is effectively a continuation, which allows
             * folding forward throw the sequence of statements while allowing
             * the following statment decide how it handles the continuation of previous
             * statments. This allows closing the iteration and assigning the result to
             * a val for example, in order to call contextual transformers
             * (sortBy, filter, ...) on the iteration up to this point.
            */
            val (_, continuation) = (statementsT zip statements).foldLeft(
              ( List[(TermName,Tree)](), identity[Tree] _ )
            ){
              case (
                ( scope, context ),
                (
                  valdefT @ q"val $nameT: $tpeT = ~$eT($mT)", // FIXME, we need to check that $e is Embed
                  valdef  @ q"val $name : $tpe  = ~$e ($m )"
                )
              ) =>
                val param = q"val $name: $tpe"
                (
                  (name, tpeT) :: scope,
                  continue => context(q"$m.flatMap( $param => $continue )")
                )
              case ( ( scope, context ), (transformerT, transformer) ) if weakTypeOf[M].typeSymbol == transformerT.tpe.typeSymbol => // FIXME TypeSymbol only checks the higher-kinded type, but needs to also check the FlowContext type argument
                // applies the transformations to `leaf` instead of the magical context handle
                def refocus(tT: Tree, t: Tree, leaf: Tree): Tree = (tT,t) match {
                  case (x@Ident(_),_) if weakTypeOf[M].typeSymbol == tT.tpe.typeSymbol => leaf
                  case ( q"$lhsT.$rhsT", q"$lhs.$rhs" ) => q"${refocus(lhsT,lhs,leaf)}.$rhs"
                  case ( q"$lhsT.$rhsT(..$argsT)", q"$lhs.$rhs(..$args)" ) => q"${refocus(lhsT,lhs,leaf)}.$rhs(..$args)"
                }

                val captured = context{
                  val values = scope.map(_._1).map(Ident.apply _)
                  unit(q"(..$values)")
                }
                val params = scope.map{ case(name, tpe) => ValDef(Modifiers(Flag.PARAM),name,tpe,EmptyTree) }
                /// FIXME: use fresh name instead of lll
                (scope, continue => q"""
                  val lll = ${refocus(transformerT, transformer, captured)}
                  lll.flatMap{
                    ((..$params) => $continue).tupled
                  }
                """)
              case ( ( scope, context ), (otherT, other) ) => 
                (scope, continue => context(q"$other; $continue"))
            }
            val res = continuation(unit(q"$result"))
            //println(res)
            res
        }
      case x => throw new Exception(x.toString)
    }
  }
}
