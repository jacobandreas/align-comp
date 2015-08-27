package framework.erector.util.learn

import breeze.linalg._
import breeze.numerics.{log, exp}
import breeze.optimize.{GradientTester, DiffFunction}
import breeze.optimize.FirstOrderMinimizer.OptParams
import spire.syntax.cfor._

/**
 * @author jda
 */

object Regression {
  def discardExtremes(x: DenseMatrix[Double], y: DenseVector[Double], frac: Double = 0.1): (DenseMatrix[Double], DenseVector[Double]) = {
    val indices = y.toArray.zipWithIndex.sortBy(_._1).map(_._2)
//    val insideIndices = indices.slice((indices.length * frac).toInt, (indices.length * (1 - frac)).toInt).toIndexedSeq
    val insideIndices = indices.slice(0, (indices.length * (1 - frac)).toInt).toIndexedSeq
    val newX = x(insideIndices, ::).toDenseMatrix
    val newY = y(insideIndices).toDenseVector
    (newX, newY)
  }
}

object Ridge {
  def apply(x: DenseMatrix[Double], y: DenseVector[Double], alpha: Double): RegressionResults = {
    val xC = DenseMatrix.horzcat(x, DenseMatrix.ones[Double](x.rows, 1))
    val beta = ((xC.t * xC) + (DenseMatrix.eye[Double](xC.cols) * alpha)) \ xC.t * y
    val weights = beta(0 to -2)
    val offset = beta(-1)
    val residuals = (xC * beta) - y
    val errs = residuals :* residuals
    val variance = sum(errs) / errs.length
    LinearRegressionResults(weights, offset, variance)
  }
}

object RidgeWithMemorizedVariance {
  def apply(x: IndexedSeq[Int], y: DenseVector[Double], alpha: Double): RegressionResultsWithMemorizedVariance = {
    val xVec = DenseVector[Double](x.toArray.map(_.toDouble)).toDenseMatrix.t
    val xC = DenseMatrix.horzcat(xVec, DenseMatrix.ones[Double](xVec.rows, 1))
    val beta = ((xC.t * xC) + (DenseMatrix.eye[Double](xC.cols) * alpha)) \ xC.t * y
    val weights = beta(0 to -2)
    val offset = beta(-1)
    val residuals = (xC * beta) - y
    val errs = residuals :* residuals
    val globalVariance = sum(errs) / errs.length
    val variances = (x zip errs.toArray).groupBy(_._1).map { case (i, grp) =>
      val grpErrs = grp.unzip._2
      val grpVariance = sum(grpErrs) / grpErrs.length
      i -> grpVariance
    }
    RegressionResultsWithMemorizedVariance(weights, offset, globalVariance, variances)
  }
}

object OLS {
  def apply(x: DenseMatrix[Double], y: DenseVector[Double]): RegressionResults = {
    val xC = DenseMatrix.horzcat(x, DenseMatrix.ones[Double](x.rows, 1))
    val beta = (xC.t * xC) \ xC.t * y
    val weights = beta(0 to -2)
    val offset = beta(-1)
    val residuals = (xC * beta) - y
    val errs = residuals :* residuals
    val variance = sum(errs) / errs.length
    LinearRegressionResults(weights, offset, variance)
  }
}

object LogisticRegression {
  def apply(x: DenseMatrix[Double], y: DenseVector[Int]): ClassificationResults = {

    val dim = x.cols
    val classes = max(y) + 1

    val objective = new DiffFunction[DenseVector[Double]] {
      override def calculate(theta: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val thetas = theta.asDenseMatrix.reshape(dim, classes)
        var ll = 0d
        val grad = DenseMatrix.zeros[Double](dim, classes)
        cforRange (0 until x.rows) { i =>
          val xh = x(i, ::).t
          val yh = y(i)
          val scores = Array.tabulate(classes) { cls =>
            val th = thetas(::, cls)
            th dot xh
          }
          val expScores = scores.map(exp(_))
          val sumExpScores = sum(expScores)
          val sumExpScoresInv = 1d / sumExpScores
          ll += scores(yh) - log(sumExpScores)
          axpy(1d, xh, grad(::, yh))
          cforRange (0 until classes) { cls =>
            axpy(expScores(cls) * -sumExpScoresInv, xh, grad(::, cls))
          }
        }
        val ugrad = grad.toDenseVector
        (-ll, -ugrad)
      }
    }

    val opt = OptParams(regularization = 0.1)
    val init = DenseVector.zeros[Double](dim * classes)
    val optTheta = opt.minimize(objective, init)
    val optThetas = optTheta.asDenseMatrix.reshape(dim, classes)
    val optThetasVec = Array.tabulate(classes)(optThetas(::,_))
    LogisticRegressionResults(optThetasVec)
  }
}

trait RegressionResults {
  def predict(input: DenseVector[Double]): Double
  def score(input: DenseVector[Double], prediction: Double): Double
}

case class LinearRegressionResults(weights: DenseVector[Double], offset: Double, variance: Double) extends RegressionResults {
  override def predict(input: DenseVector[Double]): Double = (weights dot input) + offset
  override def score(input: DenseVector[Double], prediction: Double): Double = {
    val modelPred = predict(input)
    val diff = modelPred - prediction
    - diff * diff / (2 * variance)
  }
}

trait ClassificationResults {
  def predict(input: DenseVector[Double]): Int
  def score(input: DenseVector[Double], cls: Int): Double

  def asRegression = new RegressionResults {
    override def predict(input: DenseVector[Double]) = ClassificationResults.this.predict(input).toDouble
    override def score(input: DenseVector[Double], prediction: Double) = ClassificationResults.this.score(input, prediction.toInt)
  }
}

case class LogisticRegressionResults(weights: IndexedSeq[DenseVector[Double]]) extends ClassificationResults {

  override def predict(input: DenseVector[Double]): Int = {
    weights.zipWithIndex.maxBy { case (weight, cls) =>
      weight dot input
    }._2
  }

  override def score(input: DenseVector[Double], cls: Int): Double = {
    val part = log(weights.map(w => exp(w dot input)).sum)
//    println("NUM", weights(cls) dot input)
//    println("PART", part)
    (weights(cls) dot input) - part
  }
}

case class RegressionResultsWithMemorizedVariance(weights: DenseVector[Double], offset: Double, globalVariance: Double, knownVariances: Map[Int,Double]) {
  def predict(input: Int): Double = (weights dot DenseVector[Double](input)) + offset
  def score(input: Int, prediction: Double): Double = {
    val modelPred = predict(input)
    val diff = modelPred - prediction
    val variance = knownVariances.getOrElse(input, globalVariance)
    - diff * diff / variance
  }
}

object TrivialRegression extends RegressionResults with Serializable {
  override def predict(input: DenseVector[Double]) = 0
  override def score(input: DenseVector[Double], prediction: Double) = 0
}

object TrivialClassification extends ClassificationResults with Serializable {
  override def predict(input: DenseVector[Double]): Int = 0
  override def score(input: DenseVector[Double], cls: Int): Double = 0d
}
