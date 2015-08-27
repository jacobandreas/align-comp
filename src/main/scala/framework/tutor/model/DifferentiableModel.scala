package framework.tutor.model

import breeze.linalg.{min, max, DenseVector}
import breeze.optimize.{GradientTester, StochasticDiffFunction, FirstOrderMinimizer, BatchDiffFunction}
import framework.tutor.neural.{Differentiable2, Differentiable}

/**
 * @author jda
 */

// colorado mtn express, summit express

trait Loader[T] extends (Int => IndexedSeq[T])

trait DifferentiableModel[In,Out,Params] extends (In => Out) {
  def scoreBatch(in: IndexedSeq[In], out: IndexedSeq[Out], lossFn: Differentiable2): (Double, Params)
}

trait DifferentiableModelTrainer {
  type In
  type Out
  type Params
  type VecParams = DenseVector[Double]
  type Config
  type Model <: DifferentiableModel[In,Out,Params]

  def build(params: Params, config: Config): Model
  def init(config: Config): Params
  def pack(params: Params, config: Config): VecParams
  def unpack(vecParams: VecParams, config: Config): Params

  def train(loader: Loader[(In,Out)],
            config: Config,
            lossFn: Differentiable2,
            nBatches: Int,
            nBatchItems: Int,
            nFragments: Int,
            minimizer: FirstOrderMinimizer[VecParams,StochasticDiffFunction[VecParams]],
            par: Boolean = true,
            test: Boolean = false): Iterator[Model] = {
    val objective = new BatchDiffFunction[VecParams] {
      override val fullRange = 0 until nBatches
      override def calculate(theta: VecParams, batch: IndexedSeq[Int]): (Double, VecParams) = {
        val model = build(unpack(theta, config), config)
        val fragmentSize = max(batch.length / nFragments, 1)
        val fragments = batch.grouped(fragmentSize).toIndexedSeq
        val coll = if (par) fragments.par else fragments
        val (aggScore, aggGrad, count) = coll.aggregate((0d, DenseVector.zeros[Double](theta.length), 0))(
        { case ((score, grad, count), fragmentIds) =>
          val (fragmentIn, fragmentOut) = fragmentIds.flatMap(loader).unzip
          val (scoreHere, gradHere) = model.scoreBatch(fragmentIn, fragmentOut, lossFn)
          grad += pack(gradHere, config)
          (score + scoreHere, grad, count + fragmentIn.length)
        },
        { case ((score1, grad1, count1), (score2, grad2, count2)) =>
          (score1 + score2, grad1 + grad2, count1 + count2)
        })
        val (finalScore, finalGrad) = (aggScore / count, aggGrad / count.toDouble)
//        (finalScore, max(min(finalGrad, 0.1), -0.1))
        (finalScore, finalGrad)
      }
    }

    val initParams = pack(init(config), config)
    if (test) {
      GradientTester.test[Int,DenseVector[Double]](objective, initParams, randFraction=1)
    }
    minimizer.iterations(objective.withRandomBatches(nBatchItems), initParams).map { state =>
      build(unpack(state.x, config), config)
    }
  }

}
