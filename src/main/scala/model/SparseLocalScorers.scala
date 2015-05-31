package model

import breeze.features.FeatureVector
import breeze.linalg.DenseVector

/**
 * @author jda
 */

trait SparseEventParams {
  val sparseEvent: DenseVector[Double]
}

trait SparsePairParams {
  val sparsePair: DenseVector[Double]
}

trait SparseLocalEventScorer extends LocalEventScorer {
  override type Params <: SparseEventParams
  override def scoreOneNode(iNode: Int, obs: EventObservation, params: Params, grad: Params): Double = {
    0d
//    val fv = new FeatureVector(obs.eventFeatures(iNode))
//    if (grad != null) grad.sparseEvent += fv
//    params.sparseEvent dot fv
  }
}

trait SparseLocalPairScorer extends LocalPairScorer {
  override type Params <: SparsePairParams
  override def scoreOnePair(iNode: Int, iWord: Int, obs: PairObservation, params: Params, grad: Params): Double = {
    val fv = new FeatureVector(obs.pairFeatures(iNode)(iWord))
    if (grad != null) grad.sparsePair += fv
    params.sparsePair dot fv
  }
}
