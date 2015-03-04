package main

import breeze.linalg.DenseVector
import breeze.optimize._
import framework.erector.sequence.{ForwardBackward, LogHMMLattice}
import framework.erector.util.learn.{TrivialClassification, ClassificationResults, LogisticRegression}
import framework.fodor.{StringFeature, IndicatorFeature}
import framework.igor.experiment.{ResultCache, Stage}
import model._
import spire.syntax.cfor._

/**
 * @author jda
 */
object Train extends Stage[Config] {

  override def run(implicit config: Config, cache: ResultCache): Unit = {
    val scorer: Scorer = cache.get('scorer)
    val model: Model = cache.get('model)
    val index: FeatureIndex = cache.get('index)
    val obsCache: TrainObservationCache = cache.get('trainObsCache)

    var params = scorer.initParams(index)
    var alignments = initAlignments(obsCache)

    val (lengthModel, testLengthFeaturizer) = buildLengthModel(obsCache)

    cforRange (0 until config.nTrainIters) { iter =>
      print(alignments)
      params = maxParams(scorer)(params, alignments, obsCache, model)
      alignments = maxAlignments(scorer)(params, obsCache, model)
    }

    cache.put('lengthModel, lengthModel)
    cache.put('lengthFeaturizer, testLengthFeaturizer)
    cache.put('params, params)
  }

  def initAlignments(obsCache: TrainObservationCache): IndexedSeq[IndexedSeq[Int]] = {
    IndexedSeq.tabulate(obsCache.nExamples) { iExample =>
      IndexedSeq.tabulate(obsCache.nSentences(iExample)) { iEvent =>
        if (iEvent == obsCache.nSentences(iExample) - 1) obsCache.nEvents(iExample) - 1
        else iEvent * obsCache.nEvents(iExample) / obsCache.nSentences(iExample)
      }
    }
  }

  def maxParams(scorer: Scorer)
               (currentParams: scorer.Params,
                currentAlignments: IndexedSeq[IndexedSeq[Int]],
                obsCache: TrainObservationCache,
                model: Model)
               (implicit config: Config): scorer.Params = task("param step") {
    import scorer.{zeroLike,increment,pack,unpack}

    val objective = new DiffFunction[DenseVector[Double]] {
      override def calculate(vecParams: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val params = unpack(vecParams, currentParams)
        val indices = if (config.usePar) (0 until obsCache.nExamples).par else 0 until obsCache.nExamples
        val (tScore, tGrad) = indices.aggregate[(Double, scorer.Params)]((0, zeroLike(currentParams)))(
          { case ((score, grad), iExample) =>
            val eScore = model.scoreDecisions(scorer)(obsCache.pairObservations(iExample),
                                                      obsCache.altPairObservations(iExample),
                                                      obsCache.eventObservations(iExample),
                                                      obsCache.altEventObservations(iExample),
                                                      currentAlignments(iExample),
                                                      params, grad)
            (score + eScore, grad)
          },{ case ((score1, grad1), (score2, grad2)) =>
            increment(1d, grad2, grad1)
            (score1 + score2, grad1)
          }
        )
        val packed = pack(tGrad)
        (-tScore, -pack(tGrad))
      }
    }

//    GradientTester.test[Int,DenseVector[Double]](objective, pack(currentParams), randFraction = 0.1)
    unpack(minimize(objective, pack(currentParams), L1Regularization(1), MaxIterations(30)), currentParams)
  }

  def maxAlignments(scorer: Scorer)
                   (currentParams: scorer.Params,
                    obsCache: TrainObservationCache,
                    model: Model): IndexedSeq[IndexedSeq[Int]] = task("alignment step") {
    val lattice = new LogHMMLattice {
      override val numSequences: Int = obsCache.nExamples
      override def numStates(seq: Int): Int = obsCache.nEvents(seq)
      override def sequenceLength(seq: Int): Int = obsCache.nSentences(seq)

      override def nodeLogPotential(iExample: Int, iSentence: Int, iEvent: Int): Double = {
        if (iSentence == sequenceLength(iExample) - 1 && iEvent != numStates(iExample) - 1) Double.NegativeInfinity
        else model.scoreAlignment(scorer)(obsCache.pairObservations(iExample)(iEvent)(iSentence), currentParams)
      }

      override def edgeLogPotentials(iExample: Int, iEvent: Int, forward: Boolean): DenseVector[Double] = {
        val potentials = IndexedSeq.tabulate(numStates(iExample)) { iEvent2 =>
          if (forward && iEvent2 < iEvent || !forward && iEvent2 > iEvent) Double.NegativeInfinity
          else 0
        }
        DenseVector(potentials: _*)
      }
    }

    val assignments = ForwardBackward.max(lattice)
    IndexedSeq.tabulate(obsCache.nExamples)(assignments.assignment)
  }

  def buildLengthModel(obsCache: TrainObservationCache)(implicit config: Config): (ClassificationResults, (AnnotatedWalkthrough => DenseVector[Double])) = {
    val WhitelistWords = IndexedSeq("and", ",", "then", "before", "after").map(StringFeature("word", _))
    if (config.testLengthRangeStart == config.testLengthRangeEnd) {
      return (TrivialClassification, _ => DenseVector[Double]())
    }
    def discretize(value: Int, binWidth: Int, nBins: Int): DenseVector[Double] = {
      val v = DenseVector.zeros[Double](nBins)
      val bin = value / binWidth
      val truncBin = if (bin >= nBins) nBins - 1 else bin
      v(truncBin) = 1
      v
    }
    def whitelisted(words: Set[IndicatorFeature]): DenseVector[Double] = {
      val v = DenseVector.zeros[Double](WhitelistWords.length)
      WhitelistWords.zipWithIndex.foreach { case (w, i) =>
        if (words contains w) v(i) = 1
      }
      v
    }
    val lengthModel = {
      val usableExamples = (0 until obsCache.nExamples).filter(obsCache.nEvents(_) <= config.testLengthRangeEnd)
      val xLenFeatures = usableExamples.map { iExample =>
        DenseVector.vertcat(DenseVector(1d),
                            discretize(obsCache.nWords(iExample), 5, 5),
                            whitelisted(obsCache.words(iExample)),
                            DenseVector(obsCache.nSentences(iExample).toDouble))
      }
      val yLens = usableExamples.map { iExample =>
        obsCache.nEvents(iExample)
      }
      LogisticRegression(DenseVector.horzcat(xLenFeatures: _*).t, DenseVector(yLens: _*))
    }
    val testLengthFeaturizer = (wt: AnnotatedWalkthrough) => {
      val nWords = wt.visitOrders.map(_.length).sum
      val nSents = wt.visitOrders.length.toDouble
      DenseVector.vertcat(DenseVector(1d),
                           discretize(nWords, 5, 5),
                           whitelisted(wt.wordFeats.flatten.reduce(_ | _)),
                           DenseVector(nSents))
    }
    (lengthModel, testLengthFeaturizer)
  }

}
