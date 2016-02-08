
package org.cvogt.flow

import scala.collection.immutable.Queue
import scala.language.higherKinds
import scala.annotation.compileTimeOnly
import scala.language.implicitConversions
import scala.language.experimental.macros

class OwnerFixer[C <: reflect.macros.blackbox.Context](val c: C) {

  import c.universe._

  case class State(
    t: Tree
  )

  def fixOwners(t: c.universe.Tree): Unit = {
    val initialStates = Queue(State(t.asInstanceOf[Tree]))
    fixOwners(initialStates)
  }

  def fixOwners(remaining: Queue[State]): Unit = {
    if (remaining.isEmpty) ()
    else {
      val (s, stillLeft) = remaining.dequeue
      fixOwners(stillLeft ++ process(s))
    }
  }

  def process(s: State): List[State] = s.t match {
    case Alternative(alternatives) =>
      c.abort(s.t.pos, "pattern match alternative can't be used in a `flat` block")
    case Annotated(annot, arg) =>
      c.abort(s.t.pos, "annoations can't be used in a `flat` block")
    case AppliedTypeTree(tpt, args) =>
      c.abort(s.t.pos, "type application can't be used in a `flat` block")
    case Apply(fun, args) =>
      c.abort(s.t.pos, "function application can't be used in a `flat` block")
    case Assign(lhs, rhs) =>
      c.abort(s.t.pos, "var assignment can't be used in a `flat` block")
    case AssignOrNamedArg(lhs, rhs) =>
      c.abort(s.t.pos, "named argument can't be used in a `flat` block")
    case Bind(name, body) =>
      c.abort(s.t.pos, "variadic argument can't be used in a `flat` block")
    case Block(stats, expr) =>
      c.abort(s.t.pos, "blocks can't be defined in a `flat` block")
    case CaseDef(pat, guard, body) =>
      c.abort(s.t.pos, "pattern match cases can't be defined in a `flat` block")
    case ClassDef(mods, name, tparams, impl) =>
      c.abort(s.t.pos, "classes and traits can't be defined in a `flat` block")
    case CompoundTypeTree(templ) => List.empty
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
      c.abort(s.t.pos, "local `def`s can't be used in a `flat` block")
    case ExistentialTypeTree(tpt, whereClauses) =>
      c.abort(s.t.pos, "existential types can't be used in a `flat` block")
    case Function(vparams, body) =>
      c.abort(s.t.pos, "functions can't be used in a `flat` block")
    case Ident(name) =>
      c.abort(s.t.pos, "idents can't be used in a `flat` block")
    case If(cond, thenp, elsep) =>
      c.abort(s.t.pos, "ifs can't be used in a `flat` block")
    case Import(expr, selectors) =>
      c.abort(s.t.pos, "imports can't be used in a `flat` block")
    case Literal(value) => List.empty
    case Match(selector, cases) =>
      c.abort(s.t.pos, "singleton objects can't be defined in a `flat` block")
    case ModuleDef(mods, name, impl) =>
      c.abort(s.t.pos, "singleton objects can't be defined in a `flat` block")
    case New(tpt) =>
      c.abort(s.t.pos, "objects can't be `new`ed in a `flat` block")
    case PackageDef(pid, stats) =>
      c.abort(s.t.pos, "packages can't be defined in a `flat` block")
    case RefTree(qualifier, name) =>
      c.abort(s.t.pos, "pattern match refs aren't allowed in a `flat` block")
    case Return(expr) =>
      c.abort(s.t.pos, "explicit `return`s aren't allowed in a `flat` block")
    case Select(qual, name) =>
      c.abort(s.t.pos, "member selection isn't allowed in a `flat` block")
    case SelectFromTypeTree(qual, name) =>
      c.abort(s.t.pos, "type selections aren't allowed in a `flat` block")
    case SingletonTypeTree(ref) =>
      c.abort(s.t.pos, "singleon type references aren't allowed in a `flat` block")
    case Star(elem) =>
      c.abort(s.t.pos, "variadic types aren't allowed in a `flat` block")
    case Super(qual, mix) =>
      c.abort(s.t.pos, "super references aren't allowed in a `flat` block")
    case Template(parents, self, body) =>
      c.abort(s.t.pos, "classes and traits can't be defined in a `flat` block")
    case This(qual) =>
      c.abort(s.t.pos, "`this` references aren't allowed in a `flat` block")
    case Throw(expr) =>
      c.abort(s.t.pos, "throw isn't allowed in a `flat` block")
    case Try(block, catches, finalizer) =>
      c.abort(s.t.pos, "`try` isn't allowed in a `flat` block")
    case TypeApply(fun, args) =>
      c.abort(s.t.pos, "type applications aren't allowed in a `flat` block")
    case TypeBoundsTree(lo, hi) =>
      c.abort(s.t.pos, "type bounds aren't allowed in a `flat` block")
    case TypeDef(mods, name, tparams, rhs) =>
      c.abort(s.t.pos, "type aliases aren't allowed in a `flat` block")
    case TypeTree() =>
      c.abort(s.t.pos, "inferred types aren't allowed in a `flat` block")
    case Typed(expr, tpt) =>
      c.abort(s.t.pos, "type ascriptions aren't allowed in a `flat` block")
    case UnApply(fun, args) =>
      c.abort(s.t.pos, "UnApply trees aren't allowed in a `flat` block")
    case ValDef(mods, name, tpt, rhs) =>
      c.abort(s.t.pos, "`val` definitions aren't allowed in a `flat` block")
  }

}
