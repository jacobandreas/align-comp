package model

import breeze.linalg.softmax
import breeze.numerics.exp
import spire.syntax.cfor._

/**
 * @author jda
 */
trait TreeScorer extends Scorer with LocalPairScorer {
  override def scorePair(obs: PairObservation, params: Params, grad: Params): Double = {
    val nEvents = obs.pairFeatures.length
    val nWords = obs.visitOrder.length
    val computeGrad = grad != null

    val subtreeChart = Array.fill(nWords, nEvents)(Double.NegativeInfinity)
    val subtreeGradChart: Array[Array[Params]] = if (!computeGrad) null else Array.fill(nWords, nEvents)(zeroLike(params))(pct)

    cforRange (0 until nWords) { iWord =>
      val wordSurfPos = obs.visitOrder(iWord)
      cforRange (0 until nEvents) { iNode =>
        var subtreeScore = 0d
        val subtreeGrad = (if (!computeGrad) null else zeroLike(params)).asInstanceOf[Params]

        val lexScore = scoreOnePair(iNode, wordSurfPos, obs, params, subtreeGrad)
        subtreeScore += lexScore

        cforRange (0 until nWords) { iChildWord =>
          val childWordSurfPos = obs.visitOrder(iChildWord)
          if (obs.dependencies(wordSurfPos)(childWordSurfPos)) {
            var wordSubtreeScore = Double.NegativeInfinity
            val wordSubtreeGrad = (if (!computeGrad) null else zeroLike(params)).asInstanceOf[Params]
            cforRange (0 until nEvents) { iChildNode =>
              if (obs.edges(iNode)(iChildNode)) {
                val childSubtreeScore = subtreeChart(iChildWord)(iChildNode)
                wordSubtreeScore = softmax(wordSubtreeScore, childSubtreeScore)
                if (computeGrad) {
                  val childSubtreeGrad = subtreeGradChart(iChildWord)(iChildNode)
                  increment(exp(childSubtreeScore), childSubtreeGrad, wordSubtreeGrad)
                }
              }
            }
            subtreeScore += wordSubtreeScore
            if (computeGrad) {
              val ies = exp(-wordSubtreeScore)
              val wordSubtreeInvExpScore = if (ies.isInfinite) 700d else ies
              increment(wordSubtreeInvExpScore, wordSubtreeGrad, subtreeGrad)
            }
          }
        }
        subtreeChart(iWord)(iNode) = subtreeScore
        if (computeGrad) {
          subtreeGradChart(iWord)(iNode) = subtreeGrad
        }
      }
    }
    assert (!subtreeChart.last.last.isInfinite)
    assert (!subtreeChart.last.last.isNaN)
    // TODO use root marker
    if (computeGrad) increment(1d, subtreeGradChart.last.last, grad)
    subtreeChart.last.last
  }
}
