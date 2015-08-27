package framework.arbor.translation.scfg

import breeze.util.{HashIndex, Index}

import scala.collection.mutable

/**
 * @author jda
 */
class ScfgGrammar[L](rules: Index[ScfgRule[L]]) extends Index[ScfgRule[L]] {
  override def apply(t: ScfgRule[L]): Int = rules(t)
  override def unapply(i: Int): Option[ScfgRule[L]] = rules.unapply(i)
  override def pairs: Iterator[(ScfgRule[L], Int)] = rules.pairs
  override def iterator: Iterator[ScfgRule[L]] = rules.iterator
}

object ScfgGrammar {
  class Builder[L] extends mutable.Builder[ScfgRule[L],ScfgGrammar[L]] {
    private var index = new HashIndex[ScfgRule[L]]()
    override def +=(elem: ScfgRule[L]): this.type = { index.index(elem); this }
    override def clear(): Unit = index = new HashIndex[ScfgRule[L]]()
    override def result(): ScfgGrammar[L] = new ScfgGrammar(index)
    def indexOf(t: ScfgRule[L]): Int = index.indexOf(t)
    def get(i: Int): ScfgRule[L] = index.get(i)
  }
  def newBuilder[L] = new Builder[L]()
}
