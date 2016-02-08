
package org.cvogt.flow

import reflect.macros.blackbox

abstract class TransformContext[C <: blackbox.Context](override val macroContext: C) extends TransformUtils[C](macroContext) {

  import universe._

  val returnName: TermName

  def recur(t: Tree): Tree

  sealed trait Result
  case object Accept extends Result
  case class RewriteTo(replacement: Tree) extends Result
  case class TransformRest(transformation: Tree => Tree) extends Result

  case class Rewrite(name: String)(val pf: PartialFunction[Tree, Result])

}
