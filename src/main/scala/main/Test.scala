package main

import breeze.linalg.DenseVector
import breeze.stats.distributions.Rand
import framework.erector.sequence.{ForwardBackward, LogHMMLattice}
import framework.erector.util.ViterbiBeam
import framework.fodor.StringFeature
import framework.igor.eval.EvalStats
import framework.igor.experiment.{ResultCache, Stage}
import model._
import task.{TaskInstance, Task}
import spire.syntax.cfor._

/**
 * @author jda
 */
object Test extends Stage[Config] {

  override def run(implicit config: Config, cache: ResultCache): Unit = {
    val scorer: Scorer = cache.get('scorer)
    val model: Model = cache.get('model)
    val task: Task = cache.get('task)
    val index: FeatureIndex = cache.get('index)
    val testInstances: IndexedSeq[task.Instance] = cache.get('testInstances)
    val params: scorer.Params = cache.get('params)

    val predictions = testInstances.map(predict(scorer, task)(_, params, model, index))
    val golds = testInstances.map(_.path)
    val scores = predictions zip golds map { case (pred, gold) =>
      val score = task.score(pred, gold)
      logger.info(score.toString)
      score
    }
    val finalScore = scores.reduce(_ + _)
    logger.info(finalScore.toString)
  }

  def predict(scorer: Scorer,
              task: Task)
             (instance: task.Instance,
              params: scorer.Params,
              model: Model,
              index: FeatureIndex)
             (implicit config: Config): IndexedSeq[(task.State, task.Action, task.State)] = {
    val startState = instance.path.head._1
    val annotatedWalkthrough = Annotator.annotateWalkthrough(instance.instructions)
    val (prediction, score) = (config.testLengthRangeStart to config.testLengthRangeEnd).flatMap { pathLength =>
      Seq.fill(config.nTestAlignmentRestarts)(randAlignment(instance.instructions.length, pathLength)).map { initAlignment =>
        var alignment = initAlignment
        var path = null: IndexedSeq[(task.State, task.Action, task.State)]
        var score = 0d
        cforRange (0 until config.nTestIters) { iter =>
          path = maxPath(scorer, task)(startState, pathLength, alignment, annotatedWalkthrough, params, model, index)
          val (iAlignment, iScore) = maxAlignment(scorer, task)(path, annotatedWalkthrough, params, model, index)
          alignment = iAlignment
          score = iScore
        }
        (path, score)
      }
    }.maxBy(_._2)
    logger.info(score.toString)
    prediction
  }

  def randAlignment(nSentences: Int, nEvents: Int)(implicit config: Config): IndexedSeq[Int] = {
    (Rand.randInt(nEvents).sample(nSentences - 1) :+ (nEvents - 1)).sorted
  }

  def maxPath(scorer: Scorer,
              task: Task)
             (startState: task.State,
              pathLength: Int,
              alignment: IndexedSeq[Int],
              annotatedWalkthrough: AnnotatedWalkthrough,
              params: scorer.Params,
              model: Model,
              index: FeatureIndex)
             (implicit config: Config): IndexedSeq[(task.State, task.Action, task.State)] = {
    sealed trait AHypothesis { val state: task.State; val score: Double }
    case class StartHypothesis(state: task.State) extends AHypothesis { override val score = 0d }
    case class Hypothesis(state: task.State, action: task.Action, parent: AHypothesis, score: Double) extends AHypothesis

    var lastHyps = IndexedSeq[AHypothesis](StartHypothesis(startState))
    (0 until pathLength).foreach { t =>
      val beam = new ViterbiBeam[AHypothesis, Double, task.State](config.testBeamSize)(_.score, _.state)
      val alignedSentences = alignment.zipWithIndex.filter(_._1 == t).map(_._2).toArray
      lastHyps.foreach { lastHyp =>
        task.availableActions(lastHyp.state).foreach { nextAction =>
          val nextState = task.doAction(lastHyp.state, nextAction)
          val (pairObservations, eventObservation) = buildObservations(task)(lastHyp.state, nextAction, nextState, annotatedWalkthrough, alignedSentences, index)
          val score = model.scoreOneDecision(scorer)(pairObservations, eventObservation, params, null.asInstanceOf[scorer.Params])
          val nextHyp = Hypothesis(nextState, nextAction, lastHyp, lastHyp.score + score)
          beam.add(nextHyp)
        }
      }
      lastHyps = beam.result()
    }

    def unwind(hyp: AHypothesis): IndexedSeq[(task.State,task.Action,task.State)] = {
      hyp match {
        case h:Hypothesis => unwind(h.parent) :+ (h.parent.state, h.action, h.state)
        case h:StartHypothesis => IndexedSeq()
      }
    }

    val bestLast = lastHyps.maxBy(_.score)
    unwind(bestLast)
  }

  def maxAlignment(scorer: Scorer,
                   task: Task)
                  (path: IndexedSeq[(task.State, task.Action, task.State)],
                   annotatedWalkthrough: AnnotatedWalkthrough,
                   params: scorer.Params,
                   model: Model,
                   index: FeatureIndex): (IndexedSeq[Int], Double) = {

    val annotatedEvents = path map (Annotator.annotateEvent(task) _ tupled)
    val lattice = new LogHMMLattice {
      override def numSequences: Int = 1
      override def sequenceLength(seq: Int): Int = annotatedWalkthrough.wordFeats.length
      override def numStates(seq: Int): Int = path.length
      override def edgeLogPotentials(seq: Int, iEvent: Int, forward: Boolean): DenseVector[Double] = {
        val vals = (0 until numStates(seq)).map { iEvent2 =>
          if (forward && iEvent2 < iEvent || !forward && iEvent2 > iEvent) Double.NegativeInfinity
          else 0
        }
        DenseVector[Double](vals:_*)
      }
      override def nodeLogPotential(seq: Int, iSentence: Int, iEvent: Int): Double = {
        val wordFeats = annotatedWalkthrough.wordFeats(iSentence)
        val nodeFeats = annotatedEvents(iEvent).nodeFeats
        val obs = ObservationCache.buildPairObservation(nodeFeats, wordFeats, index, shortCircuit = false)
        val score = model.scoreAlignment(scorer)(obs, params)
        score
      }
    }

    val result = ForwardBackward.max(lattice)
    (result.assignment(0), result.seqScore(0))
  }

  def buildObservations(task: Task)
                       (s1: task.State,
                        a: task.Action,
                        s2: task.State,
                        annotatedWalkthrough: AnnotatedWalkthrough,
                        alignedSentences: IndexedSeq[Int],
                        index: FeatureIndex): (Array[PairObservation], EventObservation) = {
    val annotatedEvent = Annotator.annotateEvent(task)(s1, a, s2)
    ObservationCache.build(annotatedEvent, annotatedWalkthrough, alignedSentences, index)
  }

}
