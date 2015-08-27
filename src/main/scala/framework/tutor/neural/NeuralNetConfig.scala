package framework.tutor.neural

import breeze.linalg.{convert, DenseMatrix, max, DenseVector}
import breeze.optimize.DiffFunction

/**
 * @author jda
 */

// layers

trait Layer {
  def activation: Differentiable
  def width: Int
}
case class InputLayer(width: Int) extends Layer { val activation = Identity }
case class FunctionLayer(width: Int, activation: Differentiable) extends Layer


// activations

trait Differentiable extends (DenseMatrix[Double] => DenseMatrix[Double]) {
  def value(x: DenseMatrix[Double]): DenseMatrix[Double]
  def gradient(x: DenseMatrix[Double]): DenseMatrix[Double]
  def valueAndGradient(x: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = (value(x), gradient(x))
  def apply(x: DenseMatrix[Double]) = value(x)
}

case object RectifiedLinear extends Differentiable {
  def value(x: DenseMatrix[Double]) = max(x, 0d)
  def gradient(x: DenseMatrix[Double]) = (x :> 0d).map(b => if (b) 1d else 0d)
}

case object Identity extends Differentiable {
  def value(x: DenseMatrix[Double]) = x
  def gradient(x: DenseMatrix[Double]) = DenseMatrix.ones(x.rows, x.cols)
}


// losses

trait Differentiable2 extends ((DenseMatrix[Double], DenseMatrix[Double]) => DenseMatrix[Double]) {
  def value(x: DenseMatrix[Double], a: DenseMatrix[Double]): DenseMatrix[Double]
  def gradient(x: DenseMatrix[Double], a: DenseMatrix[Double]): DenseMatrix[Double]
  def valueAndGradient(x: DenseMatrix[Double], a: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = (value(x, a), gradient(x, a))
  def apply(x: DenseMatrix[Double], a: DenseMatrix[Double]) = value(x, a)
}

case object SquaredError extends Differentiable2 {
  override def value(x: DenseMatrix[Double], a: DenseMatrix[Double]): DenseMatrix[Double] = ((x :- a) :^ 2d) * 0.5
  override def gradient(x: DenseMatrix[Double], a: DenseMatrix[Double]): DenseMatrix[Double] = x :- a
  override def valueAndGradient(x: DenseMatrix[Double], a: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val diff = x :- a
    val sqdiff = (diff :^ 2d) * 0.5
    (sqdiff, diff)
  }
}


// config

case class NeuralNetConfig(layers: IndexedSeq[Layer],
                           inputs: Range = null,
                           outputs: Range = null,
                           recurrentInputs: Range = null,
                           recurrentOutputs: Range = null) {
  val numParams = layers.tail.map(_.width).sum + layers.sliding(2).map { case Seq(l1, l2) => l1.width * l2.width }.sum
}
