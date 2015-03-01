package model

import breeze.linalg.{softmax, sum}
import breeze.numerics.{log, exp}
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
                     grad: scorer.Params): Double

  def scoreAlignment(scorer: Scorer)
                    (pairObservation: PairObservation,
                     params: scorer.Params): Double

  def scoreOneDecision(scorer: Scorer)
                      (pairObservations: Array[PairObservation],
                       eventObservation: EventObservation,
                       params: scorer.Params,
                       grad: scorer.Params): Double
}

object GoodModel extends Model with Serializable {
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

  override def scoreDecisions(scorer: Scorer)
                             (pairObservations: Array[Array[PairObservation]],
                              altPairObservations: Array[Array[Array[PairObservation]]],
                              eventObservations: Array[EventObservation],
                              altEventObservations: Array[Array[EventObservation]],
                              alignments: IndexedSeq[Int],
                              params: scorer.Params,
                              grad: scorer.Params): Double = {
    import scorer.{zeroLike,increment}

    var score = 0d
    cforRange (0 until pairObservations.length) { iEvent =>
      val alignedSentences = alignments.zipWithIndex.filter(_._1 == iEvent).map(_._2).toArray
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
      score -= logDenom
      cforRange (0 until altScores.length) { iAlt =>
        increment(-exp(altScores(iAlt) - logDenom), altGrads(iAlt), grad)
      }
    }

    assert (!score.isInfinite, "Infinite score encountered")
    assert (!score.isNaN, "NaN score encountered")
    score
  }

  override def scoreAlignment(scorer: Scorer)
                             (pairObservation: PairObservation,
                              params: scorer.Params): Double = {
    scorer.scorePair(pairObservation, params, null.asInstanceOf[scorer.Params])
  }
}
