package framework.arbor.syntax

import framework.arbor.Conversions._
import epic.trees.{Tree, AnnotatedLabel}
import edu.stanford.nlp.trees.{Tree => StanfordTree, EnglishGrammaticalStructure, EnglishGrammaticalStructureFactory}

/**
 * @author jda
 */
object ParseCollapser {

  val stanfordDepFactory = new EnglishGrammaticalStructureFactory()

  def apply(sentence: IndexedSeq[String], tree: Tree[AnnotatedLabel]): DependencyStructure = {
    val stanfordTree: StanfordTree = epicToStanfordTree(sentence, tree)
    val stanfordDepStructure = stanfordDepFactory.newGrammaticalStructure(stanfordTree)
    stanfordDepStructure
  }

}
