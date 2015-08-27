package framework.tutor.neural

import breeze.linalg.{sum, DenseVector, *, DenseMatrix}
import framework.tutor.model.{DifferentiableModelTrainer, DifferentiableModel}
import spire.syntax.cfor._

/**
 * @author jda
 */
class RecurrentNeuralNet(params: NeuralNetParams, config: NeuralNetConfig) extends DifferentiableModel[DenseMatrix[Double],DenseMatrix[Double],NeuralNetParams] {
  val inputLayer = config.layers.head
  val outputLayer = config.layers.last
  assert (inputLayer.isInstanceOf[InputLayer])

  override def scoreBatch(in: IndexedSeq[DenseMatrix[Double]], out: IndexedSeq[DenseMatrix[Double]], lossFn: Differentiable2): (Double, NeuralNetParams) = {
    val grad = RecurrentNeuralNetTrainer.unpack(DenseVector.zeros[Double](config.numParams), config)
    var score = 0d
    cforRange (0 until in.length) { i =>
      score += scoreOne(in(i), out(i), lossFn, grad)
    }
    (score, grad)
  }

  override def apply(in: DenseMatrix[Double]): DenseMatrix[Double] = {
    val (_, forwardActivations) = forward(in)
    val outputForwardActivations = forwardActivations.last
    DenseMatrix.horzcat(outputForwardActivations: _*)
  }

  private def scoreOne(input: DenseMatrix[Double], output: DenseMatrix[Double], lossFn: Differentiable2, runningGrad: NeuralNetParams): Double = {
    val (forwardPreActivations, forwardActivations) = forward(input)
    val tMax = input.cols

    var thisLoss = 0d
    val errors = Array.ofDim[DenseMatrix[Double]](config.layers.length, tMax)
    cforRange ((tMax - 1) to 0 by -1) { t =>
      var (outputLoss, outputErr) = lossFn.valueAndGradient(forwardActivations.last(t)(config.outputs, ::), output(::,t).asDenseMatrix)
      thisLoss += sum(outputLoss)

      val errorHere = DenseMatrix.zeros[Double](outputLayer.width, outputErr.cols)
      errorHere(config.outputs, ::) := outputErr
      // backpropagate through time
      if (t < tMax - 1) {
        val recurrentError = params.edges(0).t * errors(0)(t+1)
        errorHere(config.recurrentOutputs, ::) :+= recurrentError
      }

      errors(config.layers.length - 1)(t) = errorHere
      cforRange (config.layers.length - 2 to 0 by -1) { iLayer =>
        errors(iLayer)(t) =
          if (iLayer == config.layers.length - 2) {
            errors(iLayer + 1)(t) :*
              config.layers(iLayer + 1).activation.gradient(forwardPreActivations(iLayer+1)(t))
          } else {
            (params.edges(iLayer+1).t * errors(iLayer + 1)(t)) :*
              config.layers(iLayer + 1).activation.gradient(forwardPreActivations(iLayer+1)(t))
          }
        runningGrad.edges(iLayer) += errors(iLayer)(t) * forwardActivations(iLayer)(t).t
        runningGrad.biases(iLayer) += sum(errors(iLayer)(t)(*, ::))
      }
    }

    thisLoss
  }

  private def forward(input: DenseMatrix[Double]): (Array[Array[DenseMatrix[Double]]], Array[Array[DenseMatrix[Double]]]) = {
    val tMax = input.cols
    val forwardPreActivations = Array.ofDim[DenseMatrix[Double]](config.layers.length, tMax)
    val forwardActivations = Array.ofDim[DenseMatrix[Double]](config.layers.length, tMax)

    cforRange (0 until tMax) { t =>
      forwardPreActivations(0)(t) = null
      forwardActivations(0)(t) = DenseMatrix.zeros[Double](inputLayer.width, 1)

      forwardActivations(0)(t)(config.inputs, 0) := input(::, t)
      if (t > 0) {
        forwardActivations(0)(t)(config.recurrentInputs, 0) := forwardActivations(config.layers.length - 1)(t - 1)(config.recurrentOutputs, 0)
      }

      cforRange (1 until config.layers.length) { iLayer =>
        forwardPreActivations(iLayer)(t) = (params.edges(iLayer - 1) * forwardActivations(iLayer - 1)(t))
        forwardPreActivations(iLayer)(t)(::, *) :+= params.biases(iLayer - 1)
        forwardActivations(iLayer)(t) = config.layers(iLayer).activation.value(forwardPreActivations(iLayer)(t))
      }
    }
    (forwardPreActivations, forwardActivations)
  }
}

object RecurrentNeuralNetTrainer extends DifferentiableModelTrainer {
  override type Model = RecurrentNeuralNet
  override type In = DenseMatrix[Double]
  override type Out = DenseMatrix[Double]
  override type Params = NeuralNetParams
  override type Config = NeuralNetConfig

  override def init(config: Config): Params = NeuralNetTrainer.init(config)
  override def pack(params: Params, config: Config): VecParams = NeuralNetTrainer.pack(params, config)
  override def unpack(vecParams: VecParams, config: Config): Params = NeuralNetTrainer.unpack(vecParams, config)
  override def build(params: Params, config: Config): Model = {
    new RecurrentNeuralNet(params, config)
  }

}
