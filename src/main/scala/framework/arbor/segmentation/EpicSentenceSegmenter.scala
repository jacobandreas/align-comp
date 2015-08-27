package framework.arbor.segmentation

import epic.preprocess.MLSentenceSegmenter

/**
 * @author jda
 */
object EpicSentenceSegmenter extends SentenceSegmenter {
  val segmenter = MLSentenceSegmenter.bundled().get
  override def apply(arg: String): IndexedSeq[String] = segmenter(arg).toIndexedSeq
}
