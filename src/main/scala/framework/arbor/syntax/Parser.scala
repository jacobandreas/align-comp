package framework.arbor.syntax

import epic.trees.{AnnotatedLabel, Tree}

/**
 * @author jda
 */
trait Parser extends (IndexedSeq[String] => Tree[AnnotatedLabel])
