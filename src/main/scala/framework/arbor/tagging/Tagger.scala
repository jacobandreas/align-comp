package framework.arbor.tagging

import epic.trees.AnnotatedLabel

/**
 * @author jda
 */
trait Tagger extends (IndexedSeq[String] => IndexedSeq[AnnotatedLabel])
