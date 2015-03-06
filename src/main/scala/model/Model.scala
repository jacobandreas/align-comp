package model

import breeze.linalg.{DenseVector, softmax, sum}
import breeze.numerics.{log, exp}
import framework.erector.sequence.{ForwardBackward, LogHMMLattice}
import main.Config
import spire.syntax.cfor._

/**
 * @author jda
 */

trait Model {
  def scoreDecisions(scorer: Scorer)
                    (pairObservations: Array[Array[PairObservation]],
                     altPairObservations: Array[Array[Array[PairObservation]]],
                     eventObservations: Array[EventObservation],
                     altEventObservations: Array[Array[EventObservation]],
                     alignments: IndexedSeq[Int],
                     params: scorer.Params,
                     grad: scorer.Params)
                    (implicit config: Config): Double

  def scoreAlignment(scorer: Scorer)
                    (pairObservation: PairObservation,
                     params: scorer.Params): Double

  def scoreOneDecision(scorer: Scorer)
                      (pairObservations: Array[PairObservation],
                       eventObservation: EventObservation,
                       params: scorer.Params,
                       grad: scorer.Params): Double
}

trait BaseModel extends Model {
  override def scoreOneDecision(scorer: Scorer)
                               (pairObservations: Array[PairObservation],
                                eventObservation: EventObservation,
                                params: scorer.Params,
                                grad: scorer.Params): Double = {
    var score = 0d
    cforRange (0 until pairObservations.length) { iSentence =>
      score += scorer.scorePair(pairObservations(iSentence), params, grad)
    }
    score += scorer.scoreEvent(eventObservation, params, grad)
    score
  }

  override def scoreAlignment(scorer: Scorer)
                             (pairObservation: PairObservation,
                              params: scorer.Params): Double = {
    scorer.scorePair(pairObservation, params, null.asInstanceOf[scorer.Params])
  }
}

object GloballyNormalizedInconsistentModel extends BaseModel with Serializable {
  override def scoreDecisions(scorer: Scorer)
                             (pairObservations: Array[Array[PairObservation]],
                              altPairObservations: Array[Array[Array[PairObservation]]],
                              eventObservations: Array[EventObservation],
                              altEventObservations: Array[Array[EventObservation]],
                              alignments: IndexedSeq[Int],
                              params: scorer.Params,
                              grad: scorer.Params)
                             (implicit config: Config): Double = {
    import scorer.{zeroLike, increment}
    val maxStates = altPairObservations.map(_.length).max + 1
    val lattice = new LogHMMLattice {
      override def numSequences: Int = 1
      override def numStates(seq: Int): Int = maxStates
      override def sequenceLength(seq: Int): Int = pairObservations.length

      override def nodeLogPotential(seq: Int, iEvent: Int, iChoice: Int): Double = {
        if (iChoice >= altPairObservations(iEvent).length + 1) Double.NegativeInfinity
        else {
          val alignedSentences: IndexedSeq[Int] =
            if (config.multiAlign) 0 until alignments.length
            else alignments.zipWithIndex.filter(_._1 == iEvent).map(_._2).toArray
          val pairObservationsHere =
            if (iChoice == 0) alignedSentences.map(pairObservations(iEvent)).toArray
            else alignedSentences.map(altPairObservations(iEvent)(iChoice - 1)).toArray
          val eventObservationHere =
            if (iChoice == 0) eventObservations(iEvent)
            else altEventObservations(iEvent)(iChoice - 1)
          scoreOneDecision(scorer)(pairObservationsHere, eventObservationHere, params, null.asInstanceOf[scorer.Params])
        }
      }
      override def edgeLogPotentials(seq: Int, state: Int, forward: Boolean): DenseVector[Double] = DenseVector.zeros[Double](maxStates)
    }

    val marginals = ForwardBackward.marginals(lattice)

    var score = 0d
    cforRange (0 until pairObservations.length) { iEvent =>
      val alignedSentences: IndexedSeq[Int] =
        if (config.multiAlign) 0 until alignments.length
        else alignments.zipWithIndex.filter(_._1 == iEvent).map(_._2).toArray
      val nAlternatives = altEventObservations(iEvent).length
      var scoreHere = 0d
      cforRange (0 until alignedSentences.length) { i =>
        val iSentence = alignedSentences(i)
        scoreHere += scorer.scorePair(pairObservations(iEvent)(iSentence), params, grad)
      }
      scoreHere += scorer.scoreEvent(eventObservations(iEvent), params, grad)
      score += scoreHere

      val altScores = Array.tabulate(nAlternatives + 1)(marginals.expectedStateCounts(0, iEvent, _))
      cforRange (0 until altScores.length) { iChoice =>
        val gradHere = zeroLike(grad)
        val pairObservationsHere =
          if (iChoice == 0) alignedSentences.map(pairObservations(iEvent)).toArray
          else alignedSentences.map(altPairObservations(iEvent)(iChoice - 1)).toArray
        val eventObservationHere =
          if (iChoice == 0) eventObservations(iEvent)
          else altEventObservations(iEvent)(iChoice - 1)
        scoreOneDecision(scorer)(pairObservationsHere, eventObservationHere, params, gradHere)
        increment(-exp(altScores(iChoice) - marginals.seqPartition(0)), gradHere, grad)
      }
    }
    score -= marginals.seqPartition(0)
    assert (score <= 0, "Score is improperly normalized")
    score
  }
}

object LocallyNormalizedModel extends BaseModel with Serializable {
  override def scoreDecisions(scorer: Scorer)
                             (pairObservations: Array[Array[PairObservation]],
                              altPairObservations: Array[Array[Array[PairObservation]]],
                              eventObservations: Array[EventObservation],
                              altEventObservations: Array[Array[EventObservation]],
                              alignments: IndexedSeq[Int],
                              params: scorer.Params,
                              grad: scorer.Params)
                             (implicit config: Config): Double = {
    import scorer.{zeroLike,increment}
      var score = 0d
    cforRange (0 until pairObservations.length) { iEvent =>
      val alignedSentences: IndexedSeq[Int] =
        if (config.multiAlign) 0 until alignments.length
        else alignments.zipWithIndex.filter(_._1 == iEvent).map(_._2).toArray
      val nAlternatives = altEventObservations(iEvent).length
        var scoreHere = 0d
      cforRange (0 until alignedSentences.length) { i =>
        val iSentence = alignedSentences(i)
        scoreHere += scorer.scorePair(pairObservations(iEvent)(iSentence), params, grad)
      }
      scoreHere += scorer.scoreEvent(eventObservations(iEvent), params, grad)
      score += scoreHere
        val altScores = Array.fill[Double](nAlternatives)(0d)
      val altGrads = Array.fill[scorer.Params](nAlternatives)(zeroLike(params))(scorer.pct)
      cforRange (0 until nAlternatives) { iAlt =>
        cforRange (0 until alignedSentences.length) { i =>
          val iSentence = alignedSentences(i)
          altScores(iAlt) += scorer.scorePair(altPairObservations(iEvent)(iAlt)(iSentence), params, altGrads(iAlt))
        }
        altScores(iAlt) += scorer.scoreEvent(altEventObservations(iEvent)(iAlt), params, altGrads(iAlt))
      }
        val logDenom = softmax(altScores)
      assert (logDenom >= scoreHere, "Score is improperly normalized")
      score -= logDenom
      cforRange (0 until altScores.length) { iAlt =>
        increment(-exp(altScores(iAlt) - logDenom), altGrads(iAlt), grad)
      }
    }
    assert (!score.isInfinite, "Infinite score encountered")
    assert (!score.isNaN, "NaN score encountered")
    score
  }
}
