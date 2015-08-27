package framework.arbor.tagging

import epic.trees.AnnotatedLabel

/**
 * @author jda
 */
object EpicTagger extends Tagger {
  val tagger = epic.models.PosTagSelector.loadTagger("en").get
  override def apply(arg: IndexedSeq[String]): IndexedSeq[AnnotatedLabel] = tagger.bestSequence(arg).label
}
