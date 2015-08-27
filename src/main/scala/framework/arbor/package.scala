package framework

import framework.arbor.segmentation.{WordSegmenter, SentenceSegmenter}
import framework.arbor.syntax.Parser
import framework.arbor.tagging.Tagger

/**
 * @author jda
 */
package object arbor {

  def segmentSentences(document: String)(implicit sentenceSegmenter: SentenceSegmenter) = sentenceSegmenter(document)
  def segmentWords(sentence: String)(implicit wordSegmenter: WordSegmenter) = wordSegmenter(sentence)
  def tag(tokens: IndexedSeq[String])(implicit tagger: Tagger) = tagger(tokens)
  def parse(tokens: IndexedSeq[String])(implicit parser: Parser) = parser(tokens)

}
