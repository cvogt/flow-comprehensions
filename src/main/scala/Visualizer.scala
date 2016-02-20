package org.cvogt.flow

import reflect.macros.blackbox

class Visualizer[C <: blackbox.Context](val macroContext: C) {

  val universe: macroContext.universe.type = macroContext.universe

  import universe._

  def visualize(trees: (String, Tree)*): Unit = {
    val global = macroContext.universe.asInstanceOf[tools.nsc.Global]
    val gTrees = trees.toMap.mapValues(_.asInstanceOf[global.Tree])
    val locks = gTrees.mapValues(_ => new scala.concurrent.Lock)
    import global.treeBrowsers._
    gTrees.map { case (name, tree) =>
      val frame = new BrowserFrame(name)
      val model = new ASTTreeModel(tree)
      frame.setTreeModel(model)
      frame.createFrame(locks(name))
    }
    locks.values.foreach(_.acquire)
  }

}
