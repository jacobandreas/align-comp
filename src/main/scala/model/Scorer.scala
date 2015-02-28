package model

import breeze.linalg.DenseVector

import scala.reflect.ClassTag

/**
 * @author jda
 */
trait Scorer {
  type Params <: AnyRef
  implicit val pct: ClassTag[Params]

  def zeroLike(params: Params): Params
  def ignoreLike(params: Params): Params
  def increment(alpha: Double, update: Params, into: Params)
  def pack(params: Params): DenseVector[Double]
  def unpack(vecParams: DenseVector[Double], template: Params): Params

  def initParams(index: FeatureIndex): Params

  def scorePair(obs: PairObservation, params: Params, grad: Params): Double
  def scoreEvent(obs: EventObservation, params: Params, grad: Params): Double
}

trait LocalPairScorer extends Scorer {
  def scoreOnePair(iNode: Int, iWord: Int, obs: PairObservation, params: Params, grad: Params): Double
}
trait LocalEventScorer extends Scorer {
  def scoreOneNode(iNode: Int, obs: EventObservation, params: Params, grad: Params): Double
}
