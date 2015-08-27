package framework.arbor

import framework.arbor.segmentation.{EpicSentenceSegmenter,EpicWordSegmenter}
import framework.arbor.syntax.SpanParser
import framework.arbor.tagging.EpicTagger

/**
 * @author jda
 */
object BerkeleyAnnotators {
  implicit val sentenceSegmenter = EpicSentenceSegmenter
  implicit val wordSegmenter = EpicWordSegmenter
  implicit val parser = SpanParser
  implicit val tagger = EpicTagger
}
