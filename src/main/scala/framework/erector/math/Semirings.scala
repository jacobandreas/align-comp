package framework.erector.math

import breeze.linalg.{softmax, max}
import breeze.math.Semiring

/**
 * @author jda
 */

trait Maxing {
  def +(a: Double, b: Double) = max(a, b)
}

object MaxProductSemiring extends Semiring[Double] with Maxing {
  override def zero = 0
  override def ==(a: Double, b: Double) = a == b
  override def one = 1
  override def !=(a: Double, b: Double) = a != b
  override def *(a: Double, b: Double) = a * b
}

object SumProductSemiring extends Semiring[Double] {
  override def zero = 0
  override def ==(a: Double, b: Double) = a == b
  override def one = 1
  override def !=(a: Double, b: Double) = a != b
  override def +(a: Double, b: Double) = a + b
  override def *(a: Double, b: Double) = a * b
}

object Log_MaxProductSemiring extends Semiring[Double] with Maxing {
  override def zero = Double.NegativeInfinity
  override def ==(a: Double, b: Double) = a == b
  override def one = 0
  override def !=(a: Double, b: Double) = a != b
  override def *(a: Double, b: Double) = a + b
}

object Log_SumProductSemiring extends Semiring[Double] {
  override def zero = Double.NegativeInfinity
  override def ==(a: Double, b: Double) = a == b
  override def one = 0
  override def !=(a: Double, b: Double) = a != b
  override def +(a: Double, b: Double) = softmax(a, b)
  override def *(a: Double, b: Double) = a + b
}

