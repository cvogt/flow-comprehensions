
package org.cvogt.flow

import reflect.macros.blackbox

abstract class TransformUtils[C <: blackbox.Context](val macroContext: C) {

  val universe: macroContext.universe.type = macroContext.universe

  import universe._

  val M: Type
  val contextName: TermName

  def liftM(t: Tree): Tree = {
    type Id[T] = T
    val sym = symbolOf[org.cvogt.flow.Constructor[Id]]
    val tpe = t.tpe.widen
    val liftedTpe = appliedType(M, List(tpe))
    val constructorTpe = appliedType(sym, List(M))
    val ctor = try {
      macroContext.inferImplicitValue(constructorTpe, false, false, t.pos)
    } catch {
      case t: scala.reflect.macros.TypecheckException =>
        macroContext.abort(macroContext.enclosingPosition, s"Could not find a Constructor instance for $M")
    }
    val typedCtor = macroContext.typecheck(tree=ctor, pt=constructorTpe)
    println("===")
    println("typedCtor:")
    println(showCode(typedCtor, printIds=true, printOwners=true, printTypes=true))
    println(showRaw(typedCtor, printIds=true, printOwners=true, printTypes=true))
    println("===")
    val create = sym.typeSignature.member(TermName("create"))
    val r = q"$typedCtor.$create($t)"
    println("===")
    println("r:")
    println(showCode(typedCtor, printIds=true, printOwners=true, printTypes=true))
    println(showRaw(typedCtor, printIds=true, printOwners=true, printTypes=true))
    println("===")
    internal.setType(r, liftedTpe)
    r
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
    def nameReturnValue(
      owner: Symbol,
      desiredName: TermName
    ): Tree = {
      t match {
        case Block(statements, expr) =>
          val assignmentTpe = expr.tpe.widen
          val assignmentSymbol = internal.newTermSymbol(owner, desiredName)
          internal.setInfo(assignmentSymbol, assignmentTpe)
          val assignment = ValDef(Modifiers(Flag.SYNTHETIC), desiredName, TypeTree(), expr)
          internal.setType(assignment, assignmentTpe)

          val ident = Ident(desiredName)
          internal.setSymbol(ident, assignmentSymbol)
          internal.setType(ident, assignmentTpe)

          Block(statements :+ assignment, ident)

        case Typed(Block(statements, expr), tpt) =>
          val assignmentTpe = tpt.tpe.widen
          val assignmentSymbol = internal.newTermSymbol(owner, desiredName)
          internal.setInfo(assignmentSymbol, assignmentTpe)
          val assignment = ValDef(Modifiers(Flag.SYNTHETIC), desiredName, tpt, expr)
          internal.setType(assignment, assignmentTpe)

          val ident = Ident(desiredName)
          internal.setSymbol(ident, assignmentSymbol)
          internal.setType(ident, assignmentTpe)
          Block(statements :+ assignment, ident)

        case Typed(single, tpt) =>
          val assignmentTpe = tpt.tpe.widen
          val assignmentSymbol = internal.newTermSymbol(owner, desiredName)
          internal.setInfo(assignmentSymbol, assignmentTpe)
          val assignment = ValDef(Modifiers(Flag.SYNTHETIC), desiredName, tpt, single)
          internal.setType(assignment, assignmentTpe)

          val ident = Ident(desiredName)
          internal.setSymbol(ident, assignmentSymbol)
          internal.setType(ident, single.tpe)
          Block(List(assignment), ident)

        case single =>
          val assignmentTpe = single.tpe.widen
          val assignmentSymbol = internal.newTermSymbol(owner, desiredName)
          internal.setInfo(assignmentSymbol, assignmentTpe)
          val assignment = ValDef(Modifiers(Flag.SYNTHETIC), desiredName, TypeTree(), single)
          internal.setType(assignment, assignmentTpe)

          val ident = Ident(desiredName)
          internal.setSymbol(ident, assignmentSymbol)
          internal.setType(ident, single.tpe)
          Block(List(assignment), ident)
      }
    }
    def liftReturnValue(
      owner: Symbol,
      desiredName: TermName = TermName(macroContext.freshName("lifted"))
    ): Tree = {
      val tmpName = TermName(macroContext.freshName("unlifed"))
      nameReturnValue(owner, tmpName) match {
        case Block(statements, expr) =>
          val assignmentSymbol = internal.newTermSymbol(owner, desiredName)
          val liftedTpe = appliedType(M, expr.tpe.widen)
          internal.setInfo(assignmentSymbol, liftedTpe)
          val assignment = ValDef(Modifiers(Flag.SYNTHETIC), desiredName, TypeTree(), liftM(expr))
          internal.setType(assignment, liftedTpe)

          val ident = Ident(desiredName)
          internal.setSymbol(ident, assignmentSymbol)
          internal.setType(ident, liftedTpe)

          Block(statements :+ assignment, ident)
      }
    }
  }

  implicit class ListTreeOps(t: List[universe.Tree]) {
    def unify: Tree = q"..$t"
  }


}
