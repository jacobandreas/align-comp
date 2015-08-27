package framework.erector.sequence

import breeze.linalg.DenseVector
import breeze.math.Semiring
import framework.erector.math._

/**
 * @author jda
 */

trait SemiHMMLattice {
  def numSequences: Int
  def numStates: Int
  def sequenceLength(seq: Int): Int
  def nodePotential(seq: Int, t: Int, duration: Int, state: Int): Double
  def edgePotentials(seq: Int, state: Int, forward: Boolean): DenseVector[Double]
  def maxDuration: Int
  require(numStates > 0)
}

trait LinSemiHMMLattice extends SemiHMMLattice
trait LogSemiHMMLattice extends SemiHMMLattice {
  def nodeLogPotential(seq: Int, t: Int, duration: Int, state: Int): Double
  def edgeLogPotentials(seq: Int, state: Int, forward: Boolean): DenseVector[Double]
  final override def nodePotential(seq: Int, t: Int, duration: Int, state: Int) = nodeLogPotential(seq, t, duration, state)
  final override def edgePotentials(seq: Int, state: Int, forward: Boolean) = edgeLogPotentials(seq, state, forward)
}

trait SemiHMMAssignments {
  def lattice: SemiHMMLattice
  def score: Double
  def seqScore(seq: Int): Double
  def assignment(seq: Int): IndexedSeq[(Int,Int)]
}

trait SemiHMMMarginals {
  def lattice: SemiHMMLattice
  def partition: Double
  def seqPartition(seq: Int): Double
  def expectedStateCounts(state: Int): Double
  def expectedTransitionCounts(state1: Int, state2: Int): Double
//  def assignment(seq: Int): IndexedSeq[Int]
}

object SemiForwardBackward {

  def max(lattice: SemiHMMLattice): SemiHMMAssignments = {
    val semiring = lattice match {
      case _:LinSemiHMMLattice => MaxProductSemiring
      case _:LogSemiHMMLattice => Log_MaxProductSemiring
      case _                   => ???
    }
    forward(lattice, semiring)
  }

  def marginals(lattice: SemiHMMLattice): SemiHMMMarginals = {
    val semiring = lattice match {
      case _:LinSemiHMMLattice => SumProductSemiring
      case _:LogSemiHMMLattice => Log_SumProductSemiring
      case _            => ???
    }
    forwardBackward(lattice, semiring)
  }

  def forward(uLattice: SemiHMMLattice, semiring: Semiring[Double] with Maxing): SemiHMMAssignments = {
    val alphas = initialize(uLattice, semiring)
    doForwardPass(alphas, uLattice, semiring)
    val viterbiPaths = extractMax(alphas, uLattice, semiring)
    new SemiHMMAssignments {
      override val lattice: SemiHMMLattice = uLattice
      override def score = (0 until lattice.numSequences).map(seqScore).reduce(semiring.*)
      override def seqScore(seq: Int): Double = maxWithEnd(alphas, lattice, seq, lattice.sequenceLength(seq))._4
      override def assignment(seq: Int): IndexedSeq[(Int,Int)] = viterbiPaths(seq)
    }
  }

  def forwardBackward(uLattice: SemiHMMLattice, semiring: Semiring[Double]): SemiHMMMarginals = {
    val alphas = initialize(uLattice, semiring)
    val betas = initialize(uLattice, semiring)
    doForwardPass(alphas, uLattice, semiring)
    doBackwardPass(betas, uLattice, semiring)
    val marginals = initialize(uLattice, semiring)
    pointwiseMult(alphas, betas, marginals, semiring)
    new SemiHMMMarginals {
      override val lattice: SemiHMMLattice = uLattice
      override lazy val partition: Double = (0 until lattice.numSequences).map(seqPartition).reduce(semiring.*)
      override def seqPartition(seq: Int): Double = sumWithEnd(marginals, lattice, seq, lattice.sequenceLength(seq), semiring)
      override def expectedStateCounts(state: Int): Double = {
        var count = semiring.zero
        var seq = 0
        while (seq < lattice.numSequences) {
          var t = 0
          while (t < lattice.sequenceLength(seq)) {
            var duration = 1
            while (duration <= lattice.maxDuration) {
              count = semiring.+(count, marginals(seq)(t)(duration)(state))
              duration += 1
            }
            t += 1
          }
          seq += 1
        }
        count
      }
      override def expectedTransitionCounts(state1: Int, state2: Int): Double = {
        var count = semiring.zero
        var seq = 0
        while (seq < lattice.numSequences) {
          var t = 0
          while (t < lattice.sequenceLength(seq) - 1) {
            var duration = 1
            while (duration <= lattice.maxDuration) {
              count = semiring.+(count, semiring.*(marginals(seq)(t)(duration)(state1),
                                                   lattice.edgePotentials(seq,state1,forward=true)(state2)))
              duration += 1
            }
            t += 1
          }
          seq += 1
        }
        count
      }
    }
  }

  def maxWithEnd(tables: Array[Array[Array[DenseVector[Double]]]], lattice: SemiHMMLattice, seq: Int, tEnd: Int, newState: Int = -1): (Int, Int, Int, Double) = {
    var bestStart: Int = -1
    var bestDuration: Int = -1
    var bestState: Int = -1
    var bestScore: Double = Double.NegativeInfinity

    var duration = 1
    while (duration <= lattice.maxDuration && tEnd - duration >= 0) {
      val t = tEnd - duration
      var state = 0
      while (state < lattice.numStates) {
        val score = if (newState == -1 || state == newState) {
          tables(seq)(t)(duration)(state)
        } else {
          Double.NegativeInfinity
        }
        if (score > bestScore) {
          bestStart = t
          bestDuration = duration
          bestState = state
          bestScore = score
        }
        state += 1
      }
      duration += 1
    }

    (bestStart, bestDuration, bestState, bestScore)
  } ensuring { t => t._1 >= 0 && t._2 >= 0 && t._3 >= 0 && !t._4.isInfinite }

  def sumWithEnd(tables: Array[Array[Array[DenseVector[Double]]]], lattice: SemiHMMLattice, seq: Int, tEnd: Int, semiring: Semiring[Double], newState: Int = -1): Double = {
    var score: Double = Double.NegativeInfinity

    var duration = 1
    while (duration <= lattice.maxDuration && tEnd - duration >= 0) {
      val t = tEnd - duration
      var state = 0
      while (state < lattice.numStates) {
        val scoreHere = tables(seq)(t)(duration)(state)
        if (newState == -1 || state == newState) {
          score = semiring.+(score, scoreHere)
        }
        state += 1
      }
      duration += 1
    }

    score
  } ensuring { !_.isInfinite }

  def initialize(lattice: SemiHMMLattice, semiring: Semiring[Double]): Array[Array[Array[DenseVector[Double]]]] = {
    Array.tabulate(lattice.numSequences) { seq =>
      Array.tabulate(lattice.sequenceLength(seq)) { t =>
        Array.tabulate(lattice.maxDuration) { duration =>
          DenseVector.fill[Double](lattice.numStates)(semiring.zero)
        }
      }
    }
  }

  def extractMax(tables: Array[Array[Array[DenseVector[Double]]]], lattice: SemiHMMLattice, semiring: Semiring[Double] with Maxing): IndexedSeq[IndexedSeq[(Int,Int)]] = {
    (0 until tables.length).map { seq =>
      val (bestStart, bestDuration, bestState, _) = maxWithEnd(tables, lattice, seq, lattice.sequenceLength(seq))
      var sol = (bestState, bestDuration, bestStart) :: Nil
      while (sol.head._3 != 0) {
        val (bStart, bDuration, bState, _) = (0 until lattice.numStates).map { state => maxWithEnd(tables, lattice, seq, sol.head._3, state) }
                                                                             .maxBy { case (strt, dur, stt, scr) => semiring.*(scr, lattice.edgePotentials(seq,stt,forward=true)(sol.head._1)) }
        sol = (bState, bDuration, bStart) :: sol
      }
      sol.map(t => (t._1, t._2)).toIndexedSeq
    }
  }

  def pointwiseMult(a: Array[Array[Array[DenseVector[Double]]]], b: Array[Array[Array[DenseVector[Double]]]], dest: Array[Array[Array[DenseVector[Double]]]], semiring: Semiring[Double]): Unit = {
    var seq = 0
    while (seq < a.length) {
      var t = 0
      while (t < a(seq).length) {
        var duration = 0
        while (duration < a(seq)(t).length) {
          var state = 0
          while ( state < a(seq)(t).length) {
            dest(seq)(t)(duration)(state) = semiring.*(a(seq)(t)(duration)(state), b(seq)(t)(duration)(state))
            state += 1
          }
          duration += 1
        }
        t += 1
      }
      seq += 1
    }
  }

  def doForwardPass(tables: Array[Array[Array[DenseVector[Double]]]], lattice: SemiHMMLattice, semiring: Semiring[Double]): Unit = {
    var seq = 0
    while (seq < lattice.numSequences) {
      val tInit = 0

      var duration = 1
      while (duration <= lattice.maxDuration) {
        var state = 0
        while (state < lattice.numStates) {
          tables(seq)(tInit)(duration)(state) = lattice.nodePotential(seq, tInit, duration, state)
          state += 1
        }
        duration += 1
      }

      var tNew = 1
      while (tNew < lattice.sequenceLength(seq)) {
        var newState = 0
        while (newState < lattice.numStates) {

          var oldState = 0
          while (oldState < lattice.numStates) {
            val scoreToHere = sumWithEnd(tables, lattice, seq, tNew, semiring, oldState)
            var duration = 1
            while (duration <= lattice.maxDuration) {
              tables(seq)(tNew)(duration)(newState) = semiring.+(tables(seq)(tNew)(duration)(newState),
                                                                 semiring.*(lattice.edgePotentials(seq, oldState, forward=true)(newState),
                                                                            semiring.*(lattice.nodePotential(seq, tNew, duration, newState),
                                                                                       scoreToHere)))
              duration += 1
            }
            oldState += 1
          }
          newState += 1
        }
        tNew += 1
      }
      seq += 1
    }
  }

  def doBackwardPass(tables: Array[Array[Array[DenseVector[Double]]]], lattice: SemiHMMLattice, semiring: Semiring[Double]): Unit = {
    // TODO(jda) this could be made slightly faster
    var seq = 0
    while (seq < lattice.numSequences) {
      val tFinal = lattice.sequenceLength(seq)

      var duration = 1
      while (duration <= lattice.maxDuration) {
        var state = 0
        while (state < lattice.numStates) {
          tables(seq)(tFinal - duration)(duration)(state) = semiring.one
          state += 1
        }
        duration += 1
      }

      var tSuccStart = tFinal - 1
      while (tSuccStart > 0) {
        var succState = 0
        while (succState < lattice.numStates) {

          var scoreAfterHere = 0d
          var succDuration = 1
          while (succDuration <= lattice.maxDuration) {
            scoreAfterHere = semiring.+(scoreAfterHere, tables(seq)(tSuccStart)(succDuration)(succState))
            succDuration += 1
          }

          var duration = 1
          while (duration <= lattice.maxDuration) {
            var state = 0
            while (state < lattice.numStates) {
              tables(seq)(tFinal - duration)(duration)(state) = semiring.+(tables(seq)(tFinal - duration)(duration)(state),
                                                                           semiring.*(lattice.edgePotentials(seq, state, forward = true)(succState),
                                                                                      semiring.*(lattice.nodePotential(seq, tFinal - duration, duration, state),
                                                                                                 scoreAfterHere)))
              state += 1
            }
            duration += 1
          }
          succState += 1
        }
        tSuccStart -= 1
      }
      seq += 1
    }
  }

}
