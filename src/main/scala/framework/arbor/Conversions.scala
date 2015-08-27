package framework.arbor

import epic.trees.{Tree, AnnotatedLabel}
import edu.stanford.nlp.trees.{Tree => StanfordTree, LabeledScoredTreeNode => StanfordLabeledTree, EnglishGrammaticalStructure, LabeledScoredTreeFactory}
import framework.arbor.syntax.{DependencyNode, DependencyStructure}
import scala.collection.JavaConversions._
import edu.stanford.nlp.ling.LabeledWord
import java.util

/**
 * @author jda
 */
object Conversions {

  val treeFactory = new LabeledScoredTreeFactory()

  def epicToStanfordTree(sentence: IndexedSeq[String], tree: Tree[AnnotatedLabel]): StanfordTree = {
    if (tree.isLeaf) {
      val terminal = treeFactory.newLeaf(sentence(tree.begin))
      val preTerminal = treeFactory.newTreeNode(tree.label.baseLabel,List(terminal))
      preTerminal
    } else {
      val children = tree.children map (epicToStanfordTree(sentence, _))
      treeFactory.newTreeNode(tree.label.baseLabel, children)
    }
  }

  implicit def stanfordToArborDependency(input: EnglishGrammaticalStructure): DependencyStructure = {
//    val edges = input.allTypedDependencies.map { dep =>
//    val edges = input.typedDependenciesCollapsed(false).map { dep =>
    val edges = input.typedDependenciesCollapsedTree.map { dep =>
//    val edges = input.typedDependencies.map { dep =>
      val head = DependencyNode(dep.gov.index - 1, dep.gov.label.value)
      val tail = DependencyNode(dep.dep.index - 1, dep.dep.label.value)
      val label =
        if (dep.reln.getSpecific eq null) dep.reln.getShortName
        else s"${dep.reln.getShortName}_${dep.reln.getSpecific}"
      (head, label, tail)
    }.toSet
    // BACKUP use short name only
    // for some reason input.root is something other than the root
    val root = DependencyNode(-1, "ROOT")
    DependencyStructure(edges, root)
  }


}
