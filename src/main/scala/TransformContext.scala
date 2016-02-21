
package org.cvogt.flow

import reflect.macros.blackbox

abstract class TransformContext[C <: blackbox.Context](override val macroContext: C) extends TransformUtils[C](macroContext) {

  import universe._

  val returnName: TermName
  val returnType: Type

  def recur(t: Tree): Tree

  sealed trait Action
  case object Accept extends Action
  case class RewriteTo(replacement: Tree, reprocess: Boolean = true) extends Action
  case class TransformRest(transformation: Tree => Tree) extends Action

  case class Rule(name: String)(val pf: PartialFunction[Tree, Action])

}
