package model

import breeze.stats.distributions.Rand
import epic.parser.NoParseException
import framework.arbor.syntax.{DependencyNode, DependencyStructure, ParseCollapser}
import framework.fodor.graph.EventContext
import framework.fodor.{SimpleFeature, StringFeature, IndicatorFeature}
import framework.arbor._
import framework.arbor.BerkeleyAnnotators._
import framework.igor.logging.Logging
import main.Config
import task.Task
import spire.syntax.cfor._

/**
 * @author jda
 */
case class AnnotatedInstance(wordFeats: Array[Array[Set[IndicatorFeature]]],
                             depFeats: Array[Array[Array[Set[IndicatorFeature]]]],
                             nodeFeats: Array[Array[Set[IndicatorFeature]]],
                             edgeFeats: Array[Array[Array[Set[IndicatorFeature]]]],
                             altNodeFeats: Array[Array[Array[Set[IndicatorFeature]]]],
                             altEdgeFeats: Array[Array[Array[Array[Set[IndicatorFeature]]]]],
                             visitOrders: Array[Array[Int]])

case class AnnotatedWalkthrough(wordFeats: Array[Array[Set[IndicatorFeature]]],
                                depFeats: Array[Array[Array[Set[IndicatorFeature]]]],
                                visitOrders: Array[Array[Int]])

case class AnnotatedEvent(nodeFeats: Array[Set[IndicatorFeature]],
                          edgeFeats: Array[Array[Set[IndicatorFeature]]])

object Annotator extends Logging {

  final val StopWords = Set("the", "a", "an")

  def annotateInstance(task: Task)(instance: task.Instance)(implicit config: Config): AnnotatedInstance = {

    val (wordFeats, depFeats, visitOrders) = instance.instructions map buildWordAndDepFeats unzip3;
    val (nodeFeats, edgeFeats) = instance.path map (buildNodeAndEdgeFeats(task) _ tupled) unzip;

    val (altNodeFeats, altEdgeFeats) = Array.tabulate(instance.path.length) { iEvent =>
//      val startState = instance.path(iEvent)._1
//      val availableActs = task.availableActions(startState)
//      val acts =
//        if (config.sampleAlternatives.isEmpty) availableActs.toSet.toIndexedSeq
//        else (Rand.choose(availableActs).sample(config.sampleAlternatives.get).toSet + instance.path(iEvent)._2).toIndexedSeq
//      val nextStates = acts map { a => task.doAction(startState, a) }
////      logger.info(instance.path(iEvent)._2.toString)
////      logger.info(acts.toString)
//      if (!acts.contains(instance.path(iEvent)._2))
//        logger.warn(s"Gold action unavailable: ${instance.path(iEvent)._2}, $acts")
//      if (!(acts zip nextStates).contains((instance.path(iEvent)._2, instance.path(iEvent)._3)))
//        logger.warn(s"Gold outcome unavailable")
//
//      (acts zip nextStates).map { case (a, s2) => buildNodeAndEdgeFeats(task)(startState, a, s2) }.toArray.unzip

      val fromStates = if (config.globalAlternatives) instance.path.map(_._1) else Seq(instance.path(iEvent)._1)
      val availableActs = fromStates.flatMap { s => task.availableActions(s).map(s -> _) }
      val acts =
        if (config.sampleAlternatives.isEmpty) availableActs.toSet
        else Rand.choose(availableActs).sample(config.sampleAlternatives.get).toSet

      val triples = acts.map { case (s, a) => (s, a, task.doAction(s, a)) } + instance.path(iEvent)
      triples.map(buildNodeAndEdgeFeats(task) _ tupled).toArray.unzip
    }.unzip

    AnnotatedInstance(wordFeats.toArray,
                      depFeats.toArray,
                      nodeFeats.toArray,
                      edgeFeats.toArray,
                      altNodeFeats.toArray,
                      altEdgeFeats.toArray,
                      visitOrders.toArray)
  }

  def annotateWalkthrough(instructions: IndexedSeq[String]): AnnotatedWalkthrough = {
    val (wordFeats, depFeats, visitOrders) = instructions map buildWordAndDepFeats unzip3;
    AnnotatedWalkthrough(wordFeats.toArray, depFeats.toArray, visitOrders.toArray)
  }

  def annotateEvent(task: Task)(s1: task.State, a: task.Action, s2: task.State): AnnotatedEvent = {
    val (nodeFeats, edgeFeats) = buildNodeAndEdgeFeats(task)(s1, a, s2)
    AnnotatedEvent(nodeFeats.toArray, edgeFeats.toArray)
  }

  def buildWordAndDepFeats(sent: String): (Array[Set[IndicatorFeature]], Array[Array[Set[IndicatorFeature]]], Array[Int]) = {
    val words = segmentWords(sent)
    val tags = tag(words)
    val deps = try {
      val tree = parse(words)
      ParseCollapser(words, tree)
    } catch {
      case e: NoParseException => DependencyStructure.linearFallback(words)
    }

    val wordFeats = Array.tabulate[Set[IndicatorFeature]](words.length) { iWord =>
      val word = words(iWord)
      if (StopWords contains word) {
        Set()
      } else {
        Set(
          StringFeature("word", word.toLowerCase)
          // SimpleFeature(s"tag=${tags(iWord)}")
        )
      }
    }
    val depFeats = Array.ofDim[Set[IndicatorFeature]](words.length, words.length)
    cforRange2 (0 until words.length, 0 until words.length) { (iWord1, iWord2) =>
      val edge = deps.labeledEdges.find(e => e._1.index == iWord1 && e._3.index == iWord2)
      if (iWord1 == iWord2) {
        // do nothing
      } else if (StopWords contains words(iWord2)) {
        // do nothing
      } else if (edge.isDefined && iWord1 != iWord2) {
        depFeats(iWord1)(iWord2) = Set(SimpleFeature(s"rel=${edge.get._2}"))
      }
    }
    val visitOrders = deps.bfs(deps.root).map(_.index).filterNot(_ < 0).toSeq.reverse.toArray

    (wordFeats, depFeats, visitOrders)
  }

  def buildNodeAndEdgeFeats(task: Task)(s1: task.State, a: task.Action, s2: task.State): (Array[Set[IndicatorFeature]], Array[Array[Set[IndicatorFeature]]]) = {
    val repr = task.represent(s1, a, s2)
    val model = repr.model
    val nodes = model.bfs(repr.event).toSeq.reverse

    val nodeFeats = Array.tabulate[Set[IndicatorFeature]](nodes.length) { iNode =>
      nodes(iNode).features.flatMap(Featurizer.discretize)
    }
    val edgeFeats = Array.ofDim[Set[IndicatorFeature]](nodes.length, nodes.length)
    cforRange2 (0 until nodes.length, 0 until nodes.length) { (iNode1, iNode2) =>
      val node1 = nodes(iNode1)
      val node2 = nodes(iNode2)
      if (iNode1 == iNode2) {
        edgeFeats(iNode1)(iNode2) = Set(new SimpleFeature("SELF"))
      } else if (model.edges.contains((node1, node2))) {
        val feats = model.labeledEdges.find(e => e._1 == node1 && e._3 == node2).get._2.features.flatMap(Featurizer.discretize)
        edgeFeats(iNode1)(iNode2) = feats
      } else {
        // TODO skip edges?
        val path = model.path(node1, node2)
        if (path.isDefined && path.get.length == 2) {
           val labels = path.get.sliding(2).map(pair => repr.model.labeledEdges.find(e => e._1 == pair(0) && e._3 == pair(1)).get._2.features).toSeq
           assert { labels.forall(_.size == 1) }
           val joinedLabels = labels.map(_.head.asInstanceOf[IndicatorFeature].value).mkString(",")
           edgeFeats(iNode1)(iNode2) = Set[IndicatorFeature](new SimpleFeature(s"SKIP_$joinedLabels"))
        }
      }
    }
    (nodeFeats, edgeFeats)
  }

}
