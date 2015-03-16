package model

import scala.collection.mutable

import breeze.linalg.Counter
import breeze.util.{HashIndex, Index}
import framework.fodor.IndicatorFeature
import main.Config
import task.{TaskInstance, Task}

/**
 * @author jda
 */
case class FeatureIndex(pair: Index[IndicatorFeature], event: Index[IndicatorFeature], validPairs: Set[(Int,Int)]) {
  val validWordFeats = validPairs.map(_._1)
  val validNodeFeats = validPairs.map(_._2)
}

object FeatureIndex {
  def buildFrom(instances: IndexedSeq[AnnotatedInstance])(implicit config: Config): FeatureIndex = {

    val pairCounter = Counter[IndicatorFeature,Int]()
    val eventCounter = Counter[IndicatorFeature,Int]()
    val associatedPairs = mutable.Map[IndicatorFeature,mutable.Set[(Int,Int)]]()
//                                 .withDefault(_ => mutable.Set[(Int,Int)]())

    instances.foreach { inst =>
      inst.nodeFeats.foreach { graphNodeFeats =>
        graphNodeFeats.foreach { nodeFeats =>
          nodeFeats.foreach { nodeFeat =>
            inst.wordFeats.foreach { sentWordFeats =>
              sentWordFeats.foreach { wordFeats =>
                wordFeats.foreach { wordFeat =>
                  val joinFeats = Featurizer.join(wordFeat, nodeFeat)
                  joinFeats.foreach(pairCounter(_) += 1)
//                  joinFeats.foreach(associatedPairs(_) += ((wordFeat.hashCode, nodeFeat.hashCode)))
                  joinFeats.foreach { jf =>
//                    associatedPairs(jf) += ((wordFeat.hashCode, nodeFeat.hashCode))
//                    associatedPairs(jf).add((wordFeat.hashCode, nodeFeat.hashCode))
                    if (!associatedPairs.contains(jf)) associatedPairs(jf) = mutable.Set[(Int,Int)]()
                    associatedPairs(jf) += ((wordFeat.hashCode, nodeFeat.hashCode))
                  }
                }
              }
            }
            eventCounter(nodeFeat) += 1
          }
        }
      }
    }

//    val pairFeatsToKeep = pairCounter.keys.filter(pairCounter(_) > config.pairFeatCountCutoff)
    val pairFeatsToKeep = pairCounter.keys.filter(_.value.contains("Match"))
//    val eventFeatsToKeep = eventCounter.keys.filter(eventCounter(_) > config.eventFeatCountCutoff)
    val eventFeatsToKeep = eventCounter.keys.filter(k => k.value.contains("dist") || k.value.contains("same"))

    val pairIndex = new HashIndex[IndicatorFeature]()
    val eventIndex = new HashIndex[IndicatorFeature]()

    pairFeatsToKeep.foreach(pairIndex.index)
    eventFeatsToKeep.foreach(eventIndex.index)

    val validPairs = pairIndex.flatMap(associatedPairs).toSet

    FeatureIndex(pairIndex, eventIndex, validPairs)
  }
}
