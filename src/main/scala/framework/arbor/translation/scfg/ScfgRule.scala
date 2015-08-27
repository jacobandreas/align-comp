package framework.arbor.translation.scfg

import breeze.linalg.Vector

/**
 * @author jda
 */

case class ScfgRule[L](
    lhs: IndexedSeq[L],
    rhs: IndexedSeq[L],
    coindexing: IndexedSeq[(Int,Int)],
    features: Vector[Double]) {
  val nLhsNonterminals = lhs.length - coindexing.length
  val nRhsNonterminals = rhs.length - coindexing.length
}
