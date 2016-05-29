package org.cvogt.flow

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.language.higherKinds

@implicitNotFound("Could not find a Construct instance for context type ${C}. Consider defining one.")
trait Construct[C[_]] {
  def create[T](t: T): C[T]
}

object Construct extends ConstructInstances

trait ConstructInstances extends LowPriorityConstructImplicits {

  implicit object TheOptionConstruct extends Construct[Option] {
    def create[T](t: T): Option[T] = Some(t)
  }

  implicit object TheListConstruct extends Construct[List] {
    def create[T](t: T): List[T] = List(t)
  }

  implicit object TheSeqConstruct extends Construct[Seq] {
    def create[T](t: T): Seq[T] = Seq(t)
  }

  import scala.concurrent._
  object TheFutureConstruct extends Construct[Future] {
    def create[T](t: T): Future[T] = Future.successful(t)
  }

}

trait LowPriorityConstructImplicits {
  implicit def nameBasedConstruct[C[_]]: Construct[C] = macro FlowMacros.nameBasedConstruct[C[_]]
}

trait FlowMacroNameBasedConstruct extends FlowMacroBase { base: FlowMacroBase =>
  import base.c.universe._

  def nameBasedConstruct[C: WeakTypeTag]: Tree = {
    val C = weakTypeOf[C].typeConstructor
    val CCompanion = C.typeSymbol.companion
    val className = TypeName(c.freshName("Construct"))
    q"""
      class $className extends _root_.org.cvogt.flow.Construct[$C] {
        def create[T](t: T) = $CCompanion.apply(t)
      }
      new $className
    """
  }
}
