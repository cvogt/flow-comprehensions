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
  def apply[T](scope: M[FlowContext] => T): M[T] = macro FlowMacros.sequence[M[_],T]
  //def apply[T](scope: => T): M[T] = ???
}

class flow[M[_]] extends Comprehension[M]{
  @compileTimeOnly("")
  def apply[T](scope: M[FlowContext] => T): M[T] = ???
  //def apply[T](scope: => T): M[T] = ???
}

import scala.reflect.macros.blackbox
class FlowMacros(val c: blackbox.Context){
  import c.universe._

  val pkg = q"org.cvogt.flow.`package`"
  val conv = q"$pkg.omitFlowContextMonad"
  object MyTransformer extends Transformer {
    override def transform(tree: Tree) = {
      val t = tree match {
        //case q"~$v" => 
        case _ => tree
      }
      super.transform(t)
    }
  }

  def sequence[M: c.WeakTypeTag, T: c.WeakTypeTag](scope: Tree): Tree = {
    val code = scope match {
      case q"($context) => $e" =>
        val companion = weakTypeOf[M].typeSymbol.companion
        //println("-"*80)
        //println(showRaw(e))
        val transformed = c.untypecheck(e) match {
          case q"""
            ..$statements
            $result
          """ =>
            statements.foldRight(
              q"$companion.apply($result)"
            ){
              case (valdef @ q"val $name: $tpe = ~$e($m)",r) =>
                val param = q"val $name: $tpe"
                q"$m.flatMap($param => $r)"
              case (other, r) => 
                q"""
                  $other
                  $r
                """
            }
      }
      transformed
      case x => throw new Exception(x.toString)
    }
    code
  }
}
