package org.cvogt

import scala.language.experimental.macros
import scala.language.higherKinds

package object flow extends FlowApiContexts with FlowApiImplicits {

  def flat[M[_]] = new flat[M]

  def transform[M[_]] = new transform[M]

  def show(t: Any): Unit = macro FlowMacros.show

  def gui(t: Any): Unit = macro FlowMacros.gui

  def runtime(t: Any): reflect.runtime.universe.Tree = macro FlowMacros.runtime

  def echo[T](t: T): T = macro FlowMacros.echo[T]


  sealed abstract class Comprehension[M[_]]

  class flat[M[_]] extends Comprehension[M]{
    def apply[T](comprehension: MonadContext[M] => T): M[T] = macro FlowMacros.flat[M[T],T]
  }

  class transform[M[_]] {
    def apply[T]
      (returnName: String)
      (transforms: String*)
      (comprehension: MonadContext[M] => M[T]): M[T] = macro FlowMacros.transform[M[_], T]
  }
}
