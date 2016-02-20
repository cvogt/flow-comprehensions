
package org.cvogt.flow

import reflect.macros.blackbox

abstract class TransformUtils[C <: blackbox.Context](val macroContext: C) {

  val universe: macroContext.universe.type = macroContext.universe

  import universe._

  val visualizer = new Visualizer[macroContext.type](macroContext)
  import visualizer._

  val M: Type
  val contextName: TermName

  def liftM(t: Tree): Block = {

    val tType = t.tpe.widen
    val cType = appliedType(M, List(tType))
    val constructType = appliedType(
      typeOf[Construct[_, _]].typeConstructor,
      List(tType, cType)
    )
    val constructInstance = macroContext.inferImplicitValue(
      pt=constructType,
      silent=true
    )
    if (constructInstance == EmptyTree) {
      macroContext.abort(t.pos, s"Could not find a way to construct a $cType from a $tType. Consider defining an implicit instance of Construct[$tType, $cType].")
    } else ()

    val fullyExpandedConstructInstance =
      macroContext.typecheck(
        tree=constructInstance,
        pt=constructType
      )

    val constructValName = TermName(macroContext.freshName("construct"))

    val constructValDef =
      moveUnderVal(t.symbol.owner, constructValName, fullyExpandedConstructInstance)

    val constructIdent = constructValDef.ident

    val constructMethodSymbol =
      typeOf[Construct[_, _]]
        .typeConstructor
        .member(TermName("construct"))

    val constructMethodType =
      constructMethodSymbol.typeSignatureIn(constructType)

    val constructMethod = q"""$constructIdent.$constructMethodSymbol"""
    internal.setType(constructMethod, constructMethodType)

    val result = q"""
      $constructMethod($t)
    """
    internal.setType(result, cType)
    Block(List(constructValDef), result)
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

  def makeValDef(owner: Symbol, name: TermName, valType: Type, rhs: Tree): ValDef = {

    val assignmentSymbol = internal.newTermSymbol(owner, name, rhs.pos, Flag.SYNTHETIC)
    internal.setInfo(assignmentSymbol, valType)

    val typeTree = TypeTree(valType)

    val valDef = ValDef(Modifiers(Flag.SYNTHETIC), name, typeTree, rhs)
    internal.setSymbol(valDef, assignmentSymbol)
    internal.setType(valDef, NoType)
    valDef
  }

  def doNameReturnValue(
    owner: Symbol,
    desiredName: TermName,
    statements: List[Tree],
    returnValue: Tree,
    returnType: Type
  ): Tree = {

    val assignment = moveUnderVal(owner, desiredName, returnValue)
    val ident = Ident(desiredName)
    internal.setSymbol(ident, assignment.symbol)
    internal.setType(ident, returnType)

    val block = Block(statements :+ assignment, ident)
    internal.setType(block, returnType)
    block
  }

  implicit class TreeOps(t: universe.Tree) {
    def shard: List[Tree] = t match {
      case Block(statements, returnValue) => statements :+ returnValue
      case Typed(Block(statements, returnValue), tpt) => statements :+ returnValue
      case other => List(other)
    }
    def nameReturnValue(
      owner: Symbol,
      desiredName: TermName
    ): Tree = {
      t match {
        case Typed(Block(statements, expr), tpt) =>
          doNameReturnValue(owner, desiredName, statements, expr, tpt.tpe)
        case Block(statements, expr) =>
          doNameReturnValue(owner, desiredName, statements, expr, expr.tpe.widen)
        case Typed(expr, tpt) =>
          doNameReturnValue(owner, desiredName, Nil, expr, tpt.tpe)
        case expr =>
          doNameReturnValue(owner, desiredName, Nil, expr, expr.tpe.widen)
      }
    }
    def liftReturnValue(
      owner: Symbol,
      desiredName: TermName = TermName(macroContext.freshName("lifted"))
    ): Tree = {
      val tmpName = TermName(macroContext.freshName("unlifed"))
      nameReturnValue(owner, tmpName) match {
        case Block(statements, expr) =>
          val liftedType = appliedType(M, expr.tpe.widen)
          val Block(List(constructValDef), liftedReturn) = liftM(expr)
          doNameReturnValue(
            owner,
            desiredName,
            statements :+ constructValDef,
            liftedReturn,
            liftedType
          )
      }
    }
  }

  implicit class ValDefOps(d: universe.ValDef) {
    def ident: Ident = {
      val ident = Ident(d.name)
      internal.setSymbol(ident, d.symbol)
      internal.setType(ident, d.tpt.tpe)
      ident
    }
  }

  implicit class DefDefOps(d: universe.DefDef) {
    def ident: Ident = {
      val ident = Ident(d.name)
      internal.setSymbol(ident, d.symbol)
      internal.setType(ident, d.tpt.tpe)
      ident
    }
  }

  implicit class ListTreeOps(t: List[universe.Tree]) {
    def unify: Tree = {
      val resultType = t.last.tpe
      val result = q"..$t"
      internal.setType(result, resultType)
      result
    }
  }

  def moveUnderVal(directOwner: Symbol, valName: TermName, rhs: Tree): ValDef = {
    val valSymbol = internal.newTermSymbol(directOwner, valName, rhs.pos, Flag.SYNTHETIC)
    internal.setInfo(valSymbol, rhs.tpe)

    val typeTree = TypeTree(rhs.tpe)

    val valDef = ValDef(Modifiers(Flag.SYNTHETIC), valName, typeTree, rhs)
    internal.setSymbol(valDef, valSymbol)
    internal.setType(valDef, NoType)
    internal.changeOwner(valDef, directOwner, valSymbol)
  }

}
