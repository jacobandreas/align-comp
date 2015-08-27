package framework.arbor.syntax

import epic.trees.{AnnotatedLabel, Tree}
import framework.igor.util.DiskCache

/**
 * @author jda
 */
object SpanParser extends Parser {
  val parser = epic.parser.models.en.span.EnglishSpanParser.load()
  val cachedParser = DiskCache[IndexedSeq[String],Tree[AnnotatedLabel]]("parser", parser)
  override def apply(arg: IndexedSeq[String]): Tree[AnnotatedLabel] = cachedParser(arg)
}
