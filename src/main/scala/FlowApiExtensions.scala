package org.cvogt.flow

import scala.annotation.compileTimeOnly
import scala.language.higherKinds
import scala.language.implicitConversions

trait FlowApiImplicits {
  /** careful, this can lead to surprising effects */
  //implicit def autoEmbed[M[_], T](m: M[T]): T = ???

  /**
    * This function helps desugar flat[M] { x } blocks to flat[M] { c => x }
    */
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  implicit def allowOmitedContextParam[M[_], T](t: T): MonadContext[M] => T = ???

  /**
    * Implicit class for extending behaviour of a monad M[T] in the `flow` or `flat` block. These functions are available
    * only at compile time since they are interpreted and converted appropriately by the macro
    */
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  implicit class MonadExtensions[M[_], T](m: M[T]) {
    @compileTimeOnly("value calls can only be used where they will be handled by an enclosing `flat` block")
    def value: T = ???

    @compileTimeOnly("the prefix ? operator can only be used in a flow comprehension scope such as flat{...}, flow{...}")
    def unary_? : T = ???
  }

  // implicit def implicitExtract[M[_], T](mt: M[T])(implicit mc: MonadContext[M]): T = ???
  // implicit def extendImplicit[M[_], T, U](mt: M[T])(
  //   implicit mc: MonadContext[M], chain: T => U
  // ): U = ???
}

trait FlowApiContexts {
  @compileTimeOnly("implementation detail of flow comprehensions. don't use yourself")
  final class FlowContext private()

  final class MonadContext[M[_]]{
    /** transform the surrounding monad at this point */
    @compileTimeOnly("The MonadContext type only makes sense in a flow comprehension scope and is supposed to be removed by the macro.")
    def !(transform: M[FlowContext] => M[FlowContext]) = ???

    /** extract a value from a given Monad */
    @compileTimeOnly("The MonadContext type only makes sense in a flow comprehension scope and is supposed to be removed by the macro.")
    def ?[T](monad: M[T]): T = ???
  }
}
