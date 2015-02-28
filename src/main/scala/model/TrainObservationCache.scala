package model

import framework.fodor.IndicatorFeature
import spire.syntax.cfor._

/**
 * @author jda
 */
class TrainObservationCache(
    val pairObservations: Array[Array[Array[PairObservation]]],
    val altPairObservations: Array[Array[Array[Array[PairObservation]]]],
    val eventObservations: Array[Array[EventObservation]],
    val altEventObservations: Array[Array[Array[EventObservation]]]
  ) {
  require (pairObservations.length == eventObservations.length)
  val nExamples: Int = pairObservations.length
  def nEvents(iExample: Int): Int = eventObservations(iExample).length
  def nSentences(iExample: Int): Int = pairObservations(iExample).head.length
}

object TrainObservationCache {
  def buildFrom(instances: IndexedSeq[AnnotatedInstance], index: FeatureIndex): TrainObservationCache = {
    val pairObsBuilder = Array.newBuilder[Array[Array[PairObservation]]]
    val eventObsBuilder = Array.newBuilder[Array[EventObservation]]
    instances.foreach { inst =>
      val pairObs = Array.tabulate[PairObservation](inst.nodeFeats.length, inst.wordFeats.length) { (iEvent, iSentence) =>
        buildPairObservation(inst.nodeFeats(iEvent),
                             inst.altNodeFeats(iEvent),
                             inst.wordFeats(iSentence),
                             index)
      }
      pairObsBuilder += pairObs

      val altPairObs = Array.tabulate[Array[Array[PairObservation]]]

      val eventObs = Array.tabulate[EventObservation](inst.nodeFeats.length) { iEvent =>
        buildEventObservation(inst.nodeFeats(iEvent),
                              inst.altNodeFeats(iEvent),
                              index)
      }
      eventObsBuilder += eventObs

      val altEventObs = Array.tabulate[Array[EventObservation]]
    }
    new TrainObservationCache(pairObsBuilder.result(),
                              altPairObsBuilder.result(),
                              eventObsBuilder.result(),
                              altEventObsBuilder.result())
  }

  def buildPairObservation(nodeFeats: Array[Set[IndicatorFeature]],
                           altNodeFeats: Array[Array[Set[IndicatorFeature]]],
                           wordFeats: Array[Set[IndicatorFeature]],
                           index: FeatureIndex): PairObservation = {
    new PairObservation {
    }
  }

  def buildEventObservation(nodeFeats: Array[Set[IndicatorFeature]],
                            altNodeFeats: Array[Array[Set[IndicatorFeature]]],
                            index: FeatureIndex): PairObservation = {
    new EventObservation {
    }
  }
}
