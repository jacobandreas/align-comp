package model

import breeze.features.FeatureVector
import spire.syntax.cfor._

/**
 * @author jda
 */
trait BagScorer extends Scorer with LocalPairScorer {
  override def scorePair(obs: PairObservation, params: Params, grad: Params): Double = {
    var score = 0d
    cforRange (0 until obs.pairFeatures.length) { iNode =>
      cforRange (0 until obs.pairFeatures(iNode).length) { iWord =>
        score += scoreOnePair(iNode, iWord, obs, params, grad)
      }
    }
    score
  }
}
