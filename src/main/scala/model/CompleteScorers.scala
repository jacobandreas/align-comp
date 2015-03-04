package model

import scala.reflect.classTag

import breeze.linalg.{DenseVector,axpy}

/**
 * @author jda
 */

case class SparseParams(sparsePair: DenseVector[Double],
                        sparseEvent: DenseVector[Double]) extends SparsePairParams with SparseEventParams

trait SparseScorer extends Scorer with SparseLocalPairScorer with SparseLocalEventScorer {
  override type Params = SparseParams
  override def pct = classTag[SparseParams]
  assert (pct != null)

  override def zeroLike(params: Params): Params = SparseParams(DenseVector.zeros[Double](params.sparsePair.length),
                                                               DenseVector.zeros[Double](params.sparseEvent.length))
  override def increment(alpha: Double, update: Params, into: Params): Unit = {
    axpy(alpha, update.sparsePair, into.sparsePair)
    axpy(alpha, update.sparseEvent, into.sparseEvent)
  }
  override def pack(params: Params): DenseVector[Double] = {
    DenseVector.vertcat(params.sparsePair, params.sparseEvent)
  }
  override def unpack(vecParams: DenseVector[Double], template: Params): Params = {
    val pair = vecParams(0 until template.sparsePair.length)
    val event = vecParams(template.sparsePair.length until vecParams.length)
    SparseParams(pair, event)
  }

  override def scoreEvent(obs: EventObservation, params: Params, grad: Params): Double = {
    scoreOneNode(obs.iEventRoot, obs, params, grad)
  }

  override def initParams(index: FeatureIndex): Params = {
    val pair = DenseVector.zeros[Double](index.pair.size)
    val event = DenseVector.zeros[Double](index.event.size)
    SparseParams(pair, event)
  }
}

object CompleteSparseBagScorer extends SparseScorer with BagScorer with Serializable
object CompleteSparseTreeScorer extends SparseScorer with TreeScorer with Serializable
