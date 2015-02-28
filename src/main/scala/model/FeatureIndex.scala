package model

import breeze.util.{HashIndex, Index}
import framework.fodor.IndicatorFeature
import task.{TaskInstance, Task}

/**
 * @author jda
 */
case class FeatureIndex(pair: Index[IndicatorFeature], event: Index[IndicatorFeature])

object FeatureIndex {
  def buildFrom(instances: IndexedSeq[AnnotatedInstance]): FeatureIndex = {

    val pairIndex = new HashIndex[IndicatorFeature]()
    val eventIndex = new HashIndex[IndicatorFeature]()

    instances.foreach { inst =>
      inst.nodeFeats.foreach { graphNodeFeats =>
        graphNodeFeats.foreach { nodeFeats =>
          nodeFeats.foreach { nodeFeat =>
            inst.wordFeats.foreach { sentWordFeats =>
              sentWordFeats.foreach { wordFeats =>
                wordFeats.foreach { wordFeat =>
                  val joinFeats = Featurizer.join(wordFeat, nodeFeat)
                  joinFeats.foreach(pairIndex.index)
                }
              }
            }
            eventIndex.index(nodeFeat)
          }
        }
      }
    }

    FeatureIndex(pairIndex, eventIndex)
  }
}
