package main

import epic.util.{NotProvided, Optional}
import framework.igor.experiment.{Stage, Experiment}
import task.TaskFactory
import task.hcrc.Hcrc

/**
 * @author jda
 */

trait DefaultConfig {
  val dataDir: String = "data"
  val usePar: Boolean = false
  val task: TaskFactory
  val pairFeatCountCutoff: Int
  val eventFeatCountCutoff: Int
  val sampleAlternatives: Optional[Int]
  val nTrainIters: Int
  val nTestIters: Int
  val nTestAlignmentRestarts: Int
  val testLengthRangeStart: Int
  val testLengthRangeEnd: Int
  val testBeamSize: Int
}

trait HcrcConfig extends DefaultConfig {
  override val task = Hcrc
  override val pairFeatCountCutoff = 100
  override val eventFeatCountCutoff = 100
  override val sampleAlternatives: Optional[Int] = NotProvided // 20
  override val nTrainIters = 15
  override val nTestIters = 5
  override val nTestAlignmentRestarts = 5
  override val testLengthRangeStart: Int = 10
  override val testLengthRangeEnd: Int = 10
  override val testBeamSize: Int = 15
}

case class Config() extends HcrcConfig

object Main extends Experiment[Config] {
  override val paramManifest = manifest[Config]
  override val stages = Seq(Load, Train, Test)
}
