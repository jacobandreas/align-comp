package framework.arbor.syntax

import framework.erector.graph.{LabeledGraph, RootedGraph}

/**
 * @author jda
 */

case class DependencyNode(index: Int, label: String)

case class DependencyStructure(labeledEdges: Set[(DependencyNode,String,DependencyNode)],
                               root: DependencyNode) extends RootedGraph[DependencyNode] with LabeledGraph[DependencyNode,String] {
}

object DependencyStructure {
  def linearFallback(words: IndexedSeq[String]): DependencyStructure = {
    val nodes = words.zipWithIndex map { case (w, i) => DependencyNode(i, w) }
    val edges = nodes.sliding(2).map { case IndexedSeq(n1, n2) => (n1, "null", n2) }.toSet
    DependencyStructure(edges, nodes.head)
  }
}
