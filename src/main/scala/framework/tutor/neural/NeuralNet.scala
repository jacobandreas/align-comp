package framework.tutor.neural

import breeze.linalg.{sum, *, DenseMatrix, DenseVector}
import breeze.numerics.sqrt
import breeze.stats.distributions.Rand
import framework.tutor.model.{DifferentiableModelTrainer, DifferentiableModel}
import spire.syntax.cfor._

/**
 * @author jda
 */
class NeuralNet(params: NeuralNetParams, config: NeuralNetConfig) extends DifferentiableModel[DenseVector[Double], DenseVector[Double], NeuralNetParams] {

  override def scoreBatch(in: IndexedSeq[DenseVector[Double]], out: IndexedSeq[DenseVector[Double]], lossFn: Differentiable2): (Double, NeuralNetParams) = {
    val packedIn = DenseMatrix.vertcat(in.map(_.toDenseVector.asDenseMatrix): _*).t
    val packedOut = DenseMatrix.vertcat(out.map(_.asDenseMatrix): _*).t

    // forward pass
    val (forwardPreActivations, forwardActivations) = forward(packedIn)

    // compute error
    val (componentLoss, error) = lossFn.valueAndGradient(forwardActivations.last, packedOut)
    val loss = sum(componentLoss)

    // backward pass
    val edgeGrad = Array.ofDim[DenseMatrix[Double]](params.edges.length)
    val biasGrad = Array.ofDim[DenseVector[Double]](params.biases.length)
    val errors = Array.ofDim[DenseMatrix[Double]](config.layers.length)
    errors(config.layers.length - 1) = error
    cforRange (config.layers.length - 2 to 0 by -1) { iLayer =>
      errors(iLayer) =
        if (iLayer == config.layers.length - 2) errors(iLayer + 1) :* config.layers(iLayer + 1).activation.gradient(forwardPreActivations(iLayer+1))
        else (params.edges(iLayer+1).t * errors(iLayer + 1)) :* config.layers(iLayer + 1).activation.gradient(forwardPreActivations(iLayer+1))
      edgeGrad(iLayer) = errors(iLayer) * forwardActivations(iLayer).t
      biasGrad(iLayer) = sum(errors(iLayer)(*,::))
    }

    (loss, NeuralNetParams(edgeGrad, biasGrad))
  }

  override def apply(in: DenseVector[Double]): DenseVector[Double] = {
    val (_, forwardActivations) = forward(in.asDenseMatrix.t)
    forwardActivations.last.toDenseVector
  }

  private def forward(packedIn: DenseMatrix[Double]): (Array[DenseMatrix[Double]], Array[DenseMatrix[Double]]) = {
    val forwardPreActivations = Array.ofDim[DenseMatrix[Double]](config.layers.length)
    val forwardActivations = Array.ofDim[DenseMatrix[Double]](config.layers.length)

    forwardPreActivations(0) = null
    forwardActivations(0) = packedIn
    cforRange (1 until config.layers.length) { iLayer =>
      forwardPreActivations(iLayer) = (params.edges(iLayer - 1) * forwardActivations(iLayer - 1))
      forwardPreActivations(iLayer)(::,*) :+= params.biases(iLayer - 1)
      forwardActivations(iLayer) = config.layers(iLayer).activation.value(forwardPreActivations(iLayer))
    }

    (forwardPreActivations, forwardActivations)
  }

}

object NeuralNetTrainer extends DifferentiableModelTrainer {
  override type Model = NeuralNet
  override type In = DenseVector[Double]
  override type Out = DenseVector[Double]
  override type Params = NeuralNetParams
  override type Config = NeuralNetConfig

  override def init(config: Config): Params = {
    val edges = config.layers.sliding(2).toIndexedSeq.map { case Seq(l1, l2) =>
      val bd = sqrt(6d / (l1.width + l2.width))
      val rnd = Rand.uniform.map { r => (r - 0.5) * bd }
      DenseMatrix.rand(l2.width, l1.width, rnd)
    }
    val biases = config.layers.tail.map { l => DenseVector.zeros[Double](l.width) }
    NeuralNetParams(edges, biases)
  }

  override def pack(params: Params, config: Config): VecParams = {
    val edgePart = DenseVector.vertcat(params.edges.map(_.toDenseVector): _*)
    val biasPart = DenseVector.vertcat(params.biases: _*)
    DenseVector.vertcat(edgePart, biasPart)
  }

  override def unpack(vecParams: VecParams, config: Config): Params = {
    val edgeOffsets = config.layers
                            .sliding(2)
                            .map { case Seq(l1, l2) => l1.width * l2.width }
                            .foldLeft(0 :: Nil) { (soFar, size) => (size + soFar.head) :: soFar }
                            .reverse

    val biasOffsets = config.layers
                            .tail
                            .map { case l => l.width }
                            .foldLeft(edgeOffsets.last :: Nil) { (soFar, size) => (size + soFar.head) :: soFar }
                            .reverse
    assert (biasOffsets.last == vecParams.length)

    val edges = edgeOffsets.sliding(2)
                           .map { case List(start, end) => vecParams(start until end) }
                           .zipWithIndex
                           .map { case (weights, i) => weights.asDenseMatrix.reshape(config.layers(i+1).width, config.layers(i).width) }
                           .toIndexedSeq

    val biases = biasOffsets.sliding(2)
                            .map { case List(start, end) => vecParams(start until end) }
                            .toIndexedSeq

    NeuralNetParams(edges, biases)
  }

  override def build(params: Params, config: Config): Model = {
    new NeuralNet(params, config)
  }
}