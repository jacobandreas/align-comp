package framework.erector.sequence

import breeze.linalg.{argmax, DenseVector}
import breeze.math.Semiring
import framework.erector.math._
import framework.erector.util.ViterbiBeam
import framework.igor.logging.Logging

/**
 * @author jda
 */

sealed trait HMMLattice {
  def numSequences: Int
  def numStates(seq: Int): Int
  def sequenceLength(seq: Int): Int
  def nodePotential(seq: Int, t: Int, state: Int): Double
  def edgePotentials(seq: Int, state: Int, forward: Boolean): DenseVector[Double]
  require ((0 until numSequences).forall(numStates(_) > 0), "Must have a nonzero state count")
}

trait LinHMMLattice extends HMMLattice
trait LogHMMLattice extends HMMLattice {
  def nodeLogPotential(seq: Int, t: Int, state: Int): Double
  def edgeLogPotentials(seq: Int, state: Int, forward: Boolean): DenseVector[Double]
  final override def nodePotential(seq: Int, t: Int, state: Int) = nodeLogPotential(seq, t, state)
  final override def edgePotentials(seq: Int, state: Int, forward: Boolean) = edgeLogPotentials(seq, state, forward)
}

trait HMMAssignments {
  def lattice: HMMLattice
  def score: Double
  def seqScore(seq: Int): Double
  def assignment(seq: Int): IndexedSeq[Int]
}

trait HMMMarginals {
  def lattice: HMMLattice
  def partition: Double
  def seqPartition(seq: Int): Double
  def expectedStateCounts(seq: Int, t: Int, state: Int): Double
  def expectedStateCounts(state: Int): Double
  def expectedTransitionCounts(seq: Int, state1: Int, state2: Int): Double
  def assignment(seq: Int): IndexedSeq[Int]

  def _alphas: Array[Array[DenseVector[Double]]]
  def _betas: Array[Array[DenseVector[Double]]]
  def rawMarginals: Array[Array[DenseVector[Double]]]
}

object ForwardBackward extends Logging {

  def max(lattice: HMMLattice): HMMAssignments = {
    val semiring = lattice match {
      case _:LinHMMLattice => MaxProductSemiring
      case _:LogHMMLattice => Log_MaxProductSemiring
      case _               => ???
    }
    forward(lattice, semiring)
  }

  def beamMax(lattice: HMMLattice, beamSize: Int): HMMAssignments = {
    val semiring = lattice match {
      case _:LinHMMLattice => MaxProductSemiring
      case _:LogHMMLattice => Log_MaxProductSemiring
      case _               => ???
    }
    beamForward(lattice, semiring, beamSize)
  }

  def marginals(lattice: HMMLattice): HMMMarginals = {
    val semiring = lattice match {
      case _:LinHMMLattice => SumProductSemiring
      case _:LogHMMLattice => Log_SumProductSemiring
      case _               => ???
    }
    forwardBackward(lattice, semiring)
  }

  private def forward(uLattice: HMMLattice, semiring: Semiring[Double] with Maxing): HMMAssignments = {
    val alphas = initialize(uLattice, semiring)
    doPass(alphas, forward = true, uLattice, semiring)
    val viterbiPaths = extractMax(alphas, uLattice, semiring)
    new HMMAssignments {
      override val lattice: HMMLattice = uLattice
      override def score = (0 until lattice.numSequences).map(seqScore).reduce(semiring.*)
      override def seqScore(seq: Int): Double = alphas(seq).last.data.reduce(semiring.+)
      override def assignment(seq: Int): IndexedSeq[Int] = viterbiPaths(seq)
    }
  }

  private def beamForward(uLattice: HMMLattice, semiring: Semiring[Double] with Maxing, beamSize: Int): HMMAssignments = {

    case class Hypothesis(state: Int, parent: Hypothesis, score: Double)

    val bestPaths = (0 until uLattice.numSequences).map { seq =>
      val lastBeam = new ViterbiBeam[Hypothesis,Double,Int](beamSize)(_.score, _.state)
      var state = 0
      while (state < uLattice.numStates(seq)) {
        lastBeam.add(Hypothesis(state, null, uLattice.nodePotential(seq, 0, state)))
        state += 1
      }
      var lastHyps = lastBeam.result()
      var t = 1
      while (t < uLattice.sequenceLength(seq)) {
//        logger.info(s"t = $t")
        val beam = new ViterbiBeam[Hypothesis,Double,Int](beamSize)(_.score, _.state)
        var h = 0
        while (h < lastHyps.length) {
          val hyp = lastHyps(h)
          val transScores = uLattice.edgePotentials(seq, hyp.state, forward = true)
          var state = 0
          while (state < uLattice.numStates(seq)) {
            if (transScores(state) != semiring.zero) {
              val nextScore = semiring.*(uLattice.nodePotential(seq, t, state), transScores(state))
              beam.add(Hypothesis(state, hyp, semiring.*(hyp.score, nextScore)))
              state += 1
            }
          }
          h += 1
        }
        lastHyps = beam.result()
        t += 1
      }
      lastHyps.maxBy(_.score)
    }

    def unwind(hyp: Hypothesis): IndexedSeq[Int] = {
      if (hyp == null) IndexedSeq()
      else unwind(hyp.parent) :+ hyp.state
    }

    val seqs = bestPaths.map(unwind)
    val scores = bestPaths.map(_.score)

    new HMMAssignments {
      override def assignment(seq: Int): IndexedSeq[Int] = seqs(seq)
      override def lattice: HMMLattice = uLattice
      override def seqScore(seq: Int): Double = scores(seq)
      override def score: Double = scores.sum
    }

  }

  private def forwardBackward(uLattice: HMMLattice, semiring: Semiring[Double]): HMMMarginals = {
    val alphas = initialize(uLattice, semiring)
    val betas = initialize(uLattice, semiring)
    doPass(alphas, forward = true, uLattice, semiring)
    doPass(betas, forward = false, uLattice, semiring)
    val partitions = alphas.map(_.last.data.reduce(semiring.+))
    val marginals = initialize(uLattice, semiring)
    pointwiseMult(alphas, betas, marginals, semiring)
    new HMMMarginals {
      override val _alphas = alphas
      override val _betas = betas
      override val rawMarginals = marginals
      override val lattice: HMMLattice = uLattice
      override lazy val partition: Double = partitions.reduce(semiring.*)
      override def seqPartition(seq: Int): Double = partitions(seq)
      override def expectedStateCounts(seq: Int, t: Int, state: Int): Double = {
        marginals(seq)(t)(state)
        // var count = semiring.zero
        // var t = 0
        // while (t < lattice.sequenceLength(seq)) {
        //   count = semiring.+(count, marginals(seq)(t)(state))
        //   t += 1
        // }
        // count
      }
      override def expectedStateCounts(state: Int): Double = {
        var count = semiring.zero
        var seq = 0
        while (seq < lattice.numSequences) {
          var t = 0
          while (t < lattice.sequenceLength(seq)) {
            count = semiring.+(count, marginals(seq)(t)(state))
            t += 1
          }
          seq += 1
        }
        count
      }
      override def expectedTransitionCounts(seq: Int, state1: Int, state2: Int): Double = {
        var count = semiring.zero
//        var seq = 0
//        while (seq < lattice.numSequences) {
//          if (seq % 100 == 0) print(seq + " ")
          var t = 0
          while (t < lattice.sequenceLength(seq) - 1) {
            count = semiring.+(count, semiring.*(alphas(seq)(t)(state1),
                                                 semiring.*(lattice.edgePotentials(seq,state1,forward=true)(state2),
                                                            betas(seq)(t+1)(state2))))
            t += 1
          }
//          seq += 1
//        }
//        println()
        count
      }
      override def assignment(seq: Int): IndexedSeq[Int] = {
        marginals(seq).map(argmax(_))
      }
    }
  }

  def initialize(lattice: HMMLattice, semiring: Semiring[Double]): Array[Array[DenseVector[Double]]] = {
    Array.tabulate(lattice.numSequences) { seq =>
      Array.tabulate(lattice.sequenceLength(seq)) { t =>
        DenseVector.fill[Double](lattice.numStates(seq))(semiring.zero)
      }
    }
  }

  def extractMax(tables: Array[Array[DenseVector[Double]]], lattice: HMMLattice, semiring: Semiring[Double] with Maxing): IndexedSeq[IndexedSeq[Int]] = {
    (0 until tables.length).map { seq =>
      val bestEnd = IndexedSeq(argmax(tables(seq).last))
      tables(seq).dropRight(1).foldRight(bestEnd) { (scoresHere, bestPrev) =>
//        argmax((0 until lattice.numStates).map { st =>
//          semiring.*(scoresHere(st), lattice.edgePotentials(seq, bestPrev.head, forward = false)(st))
//        }) +: bestPrev
        (0 until lattice.numStates(seq)).map { st =>
          semiring.*(scoresHere(st), lattice.edgePotentials(seq, bestPrev.head, forward = false)(st))
        }.zipWithIndex.maxBy(_._1)._2 +: bestPrev
      }
    }
  }

  def pointwiseMult(a: Array[Array[DenseVector[Double]]], b: Array[Array[DenseVector[Double]]], dest: Array[Array[DenseVector[Double]]], semiring: Semiring[Double]): Unit = {
    var seq = 0
    while (seq < a.length) {
      var t = 0
      while (t < a(seq).length) {
        var state = 0
        while ( state < a(seq)(t).length) {
          dest(seq)(t)(state) = semiring.*(a(seq)(t)(state), b(seq)(t)(state))
          state += 1
        }
        t += 1
      }
      seq += 1
    }
  }

  def doPass(tables: Array[Array[DenseVector[Double]]], forward: Boolean, lattice: HMMLattice, semiring: Semiring[Double]): Unit = {

    var seq = 0
    while (seq < tables.length) {
      val numStates = lattice.numStates(seq)
      val sequenceLength = lattice.sequenceLength(seq)

      val tSeq = (if (forward) 0 until sequenceLength else sequenceLength - 1 to 0 by -1).toArray
      val tInit = tSeq(0)

      var state = 0
      while (state < numStates) {
        tables(seq)(tInit)(state) = if (forward) lattice.nodePotential(seq, tInit, state) else semiring.one
        state += 1
      }

      var index = 1
      while (index < tSeq.length) {
        val t = tSeq(index)
        val tOld = tSeq(index - 1)

        var oldState = 0
        while (oldState < numStates) {
          val scores = lattice.edgePotentials(seq, oldState, forward)
          var newState = 0
          while (newState < numStates) {
            if (scores(newState) != semiring.zero) {
              val edgePotential = scores(newState)
              val nodePotential = if (forward) lattice.nodePotential(seq, t, newState) else lattice.nodePotential(seq, tOld, oldState)
              tables(seq)(t)(newState) = semiring.+(tables(seq)(t)(newState), semiring.*(tables(seq)(tOld)(oldState),
                                                                                         semiring.*(edgePotential, nodePotential)))
            }
            newState += 1
          }
          oldState += 1
        }

//        var newState = 0
//        while (newState < numStates) {
//          tables(seq)(t)(newState) = semiring.*(tables(seq)(t)(newState), lattice.nodePotential(seq, t, newState))
//          newState += 1
//        }

        index += 1
      }
      seq += 1
    }
//    (0 until tables.length).foreach { seq =>
//      val tInit = if (forward) 0 else lattice.sequenceLength(seq) - 1
//      val tSeq = if (forward) tInit + 1 until lattice.sequenceLength(seq) else tInit - 1 to 0 by -1
//      (0 until lattice.numStates).foreach { state =>
//        tables(seq)(tInit)(state) = if (forward) lattice.nodePotential(seq, tInit, state) else semiring.one
//      }
//      tSeq.foreach { t =>
//        val tOld = if (forward) t-1 else t+1
//        (0 until lattice.numStates).foreach { oldState =>
//          val scores = lattice.edgePotentials(seq, oldState, forward)
//          (0 until lattice.numStates).foreach { newState =>
//            tables(seq)(t)(newState) = semiring.+(tables(seq)(t)(newState), semiring.*(tables(seq)(tOld)(oldState), scores(newState)))
//          }
//        }
//        (0 until lattice.numStates).foreach { newState =>
//          tables(seq)(t)(newState) = semiring.*(tables(seq)(t)(newState), lattice.nodePotential(seq, t, newState))
//        }
//      }
//    }
  }

}
