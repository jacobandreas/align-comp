package framework.erector.hypergraph

/**
 * @author jda
 */

class HypergraphNode[L](val outEdges: IndexedSeq[HypergraphEdge[L]]) extends Serializable
class HypergraphEdge[L](val label: L, val tailNodes: IndexedSeq[Int]) extends Serializable

class Hypergraph[L](nodes: IndexedSeq[HypergraphNode[L]]) extends IndexedSeq[HypergraphNode[L]] with Serializable {
  override def apply(idx: Int): HypergraphNode[L] = nodes(idx)
  override def length: Int = nodes.length
  val root = tail
  def topologicalIterator = nodes.reverseIterator
  def reverseTopologicalIterator = nodes.iterator
}

// TODO(jda) not a case class
case class HypergraphPath[L](label: L, tailNodes: IndexedSeq[HypergraphPath[L]], index: Int, decision: Int)

case class HypergraphAssignment[L](path: HypergraphPath[L], decisions: IndexedSeq[IndexedSeq[Boolean]])
case class HypergraphMarginal()
