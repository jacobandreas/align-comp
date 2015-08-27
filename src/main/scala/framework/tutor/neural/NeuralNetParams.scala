package framework.tutor.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * @author jda
 */
case class NeuralNetParams(edges: IndexedSeq[DenseMatrix[Double]], biases: IndexedSeq[DenseVector[Double]]) {
}
