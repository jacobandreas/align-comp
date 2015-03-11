package main

import epic.util.{NotProvided, Optional}
import framework.igor.experiment.{Stage, Experiment}
import task.TaskFactory
import task.crossblock.CrossBlock
import task.hcrc.Hcrc
import task.sail.Sail

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
  val useL1: Boolean
  val regularizationStrength: Double
  val nTrainIters: Int
  val nTestIters: Int
  val nTestAlignmentRestarts: Int
  val testLengthRangeStart: Int
  val testLengthRangeEnd: Int
  val testKnownLength: Boolean
  val testBeamSize: Int
  val multiAlign: Boolean
  val fold: Int
}

trait HcrcConfig extends DefaultConfig {
  override val task = Hcrc
  override val pairFeatCountCutoff = 100
  override val eventFeatCountCutoff = 100
  override val sampleAlternatives: Optional[Int] = 20
  override val useL1 = false
  override val regularizationStrength = 1.0
  override val nTrainIters = 5
  override val nTestIters = 2
  override val nTestAlignmentRestarts = 10
  override val testLengthRangeStart = 10
  override val testLengthRangeEnd = 10
  override val testKnownLength = false
  override val testBeamSize = 5
  override val multiAlign = false
  override val fold = -1
}

trait SailConfig extends DefaultConfig {
  override val task = Sail
  override val pairFeatCountCutoff = 10
  override val eventFeatCountCutoff = 10
  override val sampleAlternatives = NotProvided
  override val useL1 = true
  override val regularizationStrength = 1.0
  override val nTrainIters = 1
  override val nTestIters = 5
  override val nTestAlignmentRestarts = 1
  override val testLengthRangeStart = 1
  override val testLengthRangeEnd = 2
  override val testKnownLength = false
  override val testBeamSize = 15
  override val multiAlign = true
  override val fold = -1
}

trait CrossBlockConfig extends DefaultConfig {
  override val task = CrossBlock
  override val pairFeatCountCutoff = 10
  override val eventFeatCountCutoff = 10
  override val sampleAlternatives = NotProvided
  override val useL1 = false
  override val regularizationStrength = 1d
  override val nTrainIters = 5
  override val nTestIters = 1
  override val nTestAlignmentRestarts = 10
  override val testLengthRangeStart = -1
  override val testLengthRangeEnd = -1
  override val testKnownLength = true
  override val testBeamSize = 15
  override val multiAlign = false
  override val fold = 5
}

case class Config() extends CrossBlockConfig

object Main extends Experiment[Config] {
  override val paramManifest = manifest[Config]
  override val stages = Seq(Load, Train, Test)
}
