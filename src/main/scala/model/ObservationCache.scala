package model

import breeze.util.HashIndex
import framework.fodor.{StringFeature, IndicatorFeature}
import framework.igor.logging.Logging
import spire.syntax.cfor._

/**
 * @author jda
 */
class TrainObservationCache(
    val words: Array[Set[IndicatorFeature]], // TODO kind of a hack
    val pairObservations: Array[Array[Array[PairObservation]]],
    val altPairObservations: Array[Array[Array[Array[PairObservation]]]],
    val eventObservations: Array[Array[EventObservation]],
    val altEventObservations: Array[Array[Array[EventObservation]]]
  ) extends Serializable {
  require (pairObservations.length == eventObservations.length)
  require {
    (0 until pairObservations.length).forall { iExample =>
      (0 until pairObservations(iExample).length).forall { iEvent =>
        altPairObservations(iExample)(iEvent).length == altEventObservations(iExample)(iEvent).length
      }
    }
  }
  val nExamples: Int = pairObservations.length
  def nEvents(iExample: Int): Int = eventObservations(iExample).length
  def nSentences(iExample: Int): Int = pairObservations(iExample).head.length
  def nWords(iExample: Int): Int = pairObservations(iExample).head.map(_.visitOrder.length).sum
  def nAlternatives(iExample: Int, iEvent: Int) = altEventObservations(iExample)(iEvent).length
}

object ObservationCache extends Logging {
  def buildFromTrainingData(instances: IndexedSeq[AnnotatedInstance], index: FeatureIndex): TrainObservationCache = {
    val wordBuilder = Array.ofDim[Set[IndicatorFeature]](instances.length)
    val pairObsBuilder = Array.ofDim[Array[Array[PairObservation]]](instances.length) // Array.newBuilder[Array[Array[PairObservation]]]
    val altPairObsBuilder = Array.ofDim[Array[Array[Array[PairObservation]]]](instances.length) // Array.newBuilder[Array[Array[Array[PairObservation]]]]
    val eventObsBuilder = Array.ofDim[Array[EventObservation]](instances.length) // Array.newBuilder[Array[EventObservation]]
    val altEventObsBuilder = Array.ofDim[Array[Array[EventObservation]]](instances.length) // Array.newBuilder[Array[Array[EventObservation]]]

    cforRange (0 until instances.length) { iInstance =>
      println(iInstance)
      val inst = instances(iInstance)

      wordBuilder(iInstance) = inst.wordFeats.flatten.reduce(_ | _)

      val pairObs = Array.ofDim[PairObservation](inst.nodeFeats.length, inst.wordFeats.length)
      cforRange2 (0 until inst.nodeFeats.length, 0 until inst.wordFeats.length) { (iEvent, iSentence) =>
        pairObs(iEvent)(iSentence) = buildPairObservation(inst.nodeFeats(iEvent), inst.wordFeats(iSentence), inst.visitOrders(iSentence), inst.edgeFeats(iEvent), inst.depFeats(iSentence), index)
      }
      pairObsBuilder(iInstance) = pairObs

//      val maxAlts = inst.altNodeFeats.map(_.length).max
//      val altPairObs = Array.ofDim[PairObservation](inst.nodeFeats.length, maxAlts, inst.wordFeats.length)
//      cforRange (0 until inst.altNodeFeats.length) { iEvent =>
//        cforRange2 (0 until inst.altNodeFeats(iEvent).length, 0 until inst.wordFeats.length) { (iAlt, iSentence) =>
//          altPairObs(iEvent)(iAlt)(iSentence) = buildPairObservation(inst.altNodeFeats(iEvent)(iAlt), inst.wordFeats(iSentence), inst.visitOrders(iSentence), inst.altEdgeFeats(iEvent)(iAlt), inst.depFeats(iSentence), index)
//        }
//      }
      val altPairObs = Array.ofDim[Array[Array[PairObservation]]](inst.altNodeFeats.length)
      cforRange (0 until inst.altNodeFeats.length) { iEvent =>
        altPairObs(iEvent) = Array.ofDim[Array[PairObservation]](inst.altNodeFeats(iEvent).length)
        cforRange (0 until inst.altNodeFeats(iEvent).length) { iAlt =>
          altPairObs(iEvent)(iAlt) = Array.ofDim[PairObservation](inst.altNodeFeats(iEvent)(iAlt).length)
          cforRange (0 until inst.wordFeats.length) { iSentence =>
            altPairObs(iEvent)(iAlt)(iSentence) = buildPairObservation(inst.altNodeFeats(iEvent)(iAlt), inst.wordFeats(iSentence), inst.visitOrders(iSentence), inst.altEdgeFeats(iEvent)(iAlt), inst.depFeats(iSentence), index)
          }
        }
      }


      altPairObsBuilder(iInstance) = altPairObs

      val eventObs = Array.tabulate[EventObservation](inst.nodeFeats.length) { iEvent =>
        buildEventObservation(inst.nodeFeats(iEvent), index)
      }
      eventObsBuilder(iInstance) = eventObs

      val altEventObs = Array.tabulate[Array[EventObservation]](inst.altNodeFeats.length) { iAlt =>
        Array.tabulate[EventObservation](inst.altNodeFeats(iAlt).length) { iEvent =>
          buildEventObservation(inst.altNodeFeats(iAlt)(iEvent), index)
        }
      }
      altEventObsBuilder(iInstance) = altEventObs
    }
    new TrainObservationCache(wordBuilder,
                              pairObsBuilder,
                              altPairObsBuilder,
                              eventObsBuilder,
                              altEventObsBuilder)
  }

  def build(event: AnnotatedEvent, walkthrough: AnnotatedWalkthrough, sentences: IndexedSeq[Int], index: FeatureIndex): (Array[PairObservation], EventObservation) = {
    val pairObs = Array.ofDim[PairObservation](sentences.length)
    cforRange (0 until sentences.length) { i =>
      pairObs(i) = buildPairObservation(event.nodeFeats, walkthrough.wordFeats(i), walkthrough.visitOrders(i), event.edgeFeats, walkthrough.depFeats(i), index, shortCircuit = false)
    }
    (pairObs, buildEventObservation(event.nodeFeats, index))
  }

  def buildPairObservation(nodeFeats: Array[Set[IndicatorFeature]],
                           wordFeats: Array[Set[IndicatorFeature]],
                           visitOrder: Array[Int],
                           edgeFeats: Array[Array[Set[IndicatorFeature]]],
                           depFeats: Array[Array[Set[IndicatorFeature]]],
                           index: FeatureIndex,
                           shortCircuit: Boolean = true): PairObservation = {
//    logger.info(nodeFeats.toIndexedSeq.toString)
//    logger.info(wordFeats.toIndexedSeq.toString)
    val out = Array.ofDim[Array[Int]](nodeFeats.length, wordFeats.length)
    val eventObs = buildEventObservation(nodeFeats, index)
    cforRange (0 until nodeFeats.length) { iNode =>
      val nodeFeatsHere = nodeFeats(iNode)
      cforRange (0 until wordFeats.length) { iWord =>
        val wordFeatsHere = wordFeats(iWord)
        out(iNode)(iWord) = nodeFeatsHere.flatMap { nf =>
          if (shortCircuit && !index.validNodeFeats.contains(nf.hashCode)) Set[Int]()
          else wordFeatsHere.flatMap { wf =>
            if (!shortCircuit || index.validPairs.contains(wf.hashCode, nf.hashCode)) {
              val feats = Featurizer.join(wf, nf)
              feats.flatMap(index.pair.indexOpt)
            }
            else Set()
          }
        }.toArray
      }
    }
    val deps = depFeats.map(_.map(_ != null))
    val edges = edgeFeats.map(_.map(_ != null))
    assert (0 until deps.length forall { i => !deps(i)(i) })
    PairObservationImpl(visitOrder, out, deps, edges, eventObs.eventFeatures, eventObs.iEventRoot)
  }

  def buildEventObservation(nodeFeats: Array[Set[IndicatorFeature]],
                            index: FeatureIndex): EventObservation = {
    val eventFeatures: Array[Array[Int]] = nodeFeats.map(_.flatMap(index.event.indexOpt).toArray)
    val iEventRoot: Int = nodeFeats.length - 1
    EventObservationImpl(eventFeatures, iEventRoot)
  }

}
