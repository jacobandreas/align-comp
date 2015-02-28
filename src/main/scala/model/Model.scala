package model

import breeze.linalg.sum
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
}

object GoodModel extends Model {
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
      val alignedSentences = alignments.filter(_ == iEvent).toArray

      cforRange (0 until alignedSentences.length) { i =>
        val iSentence = alignedSentences(i)
        score += scorer.scorePair(pairObservations(iEvent)(iSentence), params, grad)
      }
      score += scorer.scoreEvent(eventObservations(iEvent), params, grad)

      val altScores = Array.fill[Double](eventObservations(iEvent).nAlternatives)(0d)
      
      val altGrads = Array.fill(eventObservations(iEvent).nAlternatives)(zeroLike(params))
      cforRange (0 until eventObservations(iEvent).nAlternatives) { iAlt =>
        cforRange (0 until alignedSentences.length) { i =>
          val iSentence = alignedSentences(i)
          altScores(iAlt) += scorer.scorePair(altPairObservations(iEvent)(iAlt)(iSentence), params, altGrads(iAlt))
        }
        altScores(iAlt) += scorer.scoreEvent(altEventObservations(iEvent)(iAlt), params, altGrads(iAlt))
      }

      val altExpScores = altScores.map(exp(_))
      val altExpScoresSum = sum(altExpScores)
      val altExpScoresSumInv = 1d / altExpScoresSum
      score -= log(altExpScoresSum)
      cforRange (0 until altExpScores.length) { iAlt =>
        increment(-altExpScores(iAlt) * altExpScoresSumInv, altGrads(iAlt), grad)
      }
    }

    score
  }

  override def scoreAlignment(scorer: Scorer)
                             (pairObservation: PairObservation,
                              params: scorer.Params): Double = {
    import scorer.ignoreLike
    scorer.scorePair(pairObservation, params, ignoreLike(params))
  }
}
