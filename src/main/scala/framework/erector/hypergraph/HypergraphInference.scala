package framework.erector.hypergraph

import breeze.linalg.argmax
import breeze.math.Semiring
import framework.erector.math.{Maxing, Log_MaxProductSemiring}
import spire.syntax.cfor._

/**
 * @author jda
 */
object HypergraphInference {
  def max[L](hg: Hypergraph[L], semiring: Semiring[Double])(scorer: (L, Int, Int) => Double): HypergraphAssignment[L] = {
    assert (semiring.isInstanceOf[Maxing])
    val scores = Array.ofDim[Double](hg.length)
    val paths = Array.ofDim[HypergraphPath[L]](hg.length)

    cforRange (0 until hg.length) { iNode =>
      val outEdges = hg(iNode).outEdges
      val outEdgeScores = Array.fill(outEdges.length)(semiring.one)
      cforRange (0 until outEdges.length) { iEdge =>
        cforRange (0 until outEdges(iEdge).tailNodes.length) { iTail =>
          outEdgeScores(iEdge) = semiring.*(outEdgeScores(iEdge), scores(outEdges(iEdge).tailNodes(iTail)))
        }
        outEdgeScores(iEdge) = semiring.*(outEdgeScores(iEdge), scorer(outEdges(iEdge).label, iNode, iEdge))
      }
      val iBestEdge = argmax(outEdgeScores)
      scores(iNode) = outEdgeScores(iBestEdge)
      val children = outEdges(iBestEdge).tailNodes.map(paths)
      assert (!children.contains(null))
      val path = new HypergraphPath(outEdges(iBestEdge).label, children, iNode, iBestEdge)
      paths(iNode) = path
    }

    val bestPath = paths.last
    val decisions = Array.ofDim[IndexedSeq[Boolean]](hg.length)
    writeDecisions(bestPath, decisions, hg)

    HypergraphAssignment(bestPath, decisions)
  }

  private def writeDecisions[L](path: HypergraphPath[L], decisions: Array[IndexedSeq[Boolean]], hg: Hypergraph[L]): Unit = {
    val here = Array.fill[Boolean](hg(path.index).outEdges.length)(false)
    here(path.decision) = true
    decisions(path.index) = here
    path.tailNodes.foreach(writeDecisions(_, decisions, hg))
  }
}
