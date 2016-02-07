
package org.cvogt.flow

import reflect.macros.blackbox

abstract class TransformContext[C <: blackbox.Context](val macroContext: C) {

  val universe: macroContext.universe.type = macroContext.universe

  import universe._

  val M: Type
  val contextName: TermName
  def recur(t: Tree): Tree

  sealed trait Result
  case object Accept extends Result
  case class RewriteTo(replacement: Tree) extends Result
  case class TransformRest(transformation: Tree => Tree) extends Result

  case class Rewrite(name: String)(val pf: PartialFunction[Tree, Result])

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

  def isExtract(t: Tree): Boolean = t match {
    case q"$contextName.?[$tpe]($body)" => true
    case other => false
  }

  def noExtracts(t: Tree): Boolean = !hasExtracts(t)

  object Extract {
    def apply(body: Tree, tpe: Tree): Tree = q"$contextName.?[$tpe]($body)"
    def unapply(t: Tree): Option[(Tree, Tree)] = t match {
      case q"$contextName.?[$tpe]($body)" => Some((body, tpe))
      case other => None
    }
  }

  implicit class TreeOps(t: universe.Tree) {
    def shard: List[Tree] = t match {
      case q"..${result: List[Tree]}" => result
      case other => List(other)
    }
  }

  implicit class ListTreeOps(t: List[universe.Tree]) {
    def unify: Tree = q"..$t"
  }

}
