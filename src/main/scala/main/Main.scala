package main

import epic.util.{NotProvided, Optional}
import framework.igor.experiment.{Stage, Experiment}
import task.TaskFactory
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
  val nTrainIters: Int
  val nTestIters: Int
  val nTestAlignmentRestarts: Int
  val testLengthRangeStart: Int
  val testLengthRangeEnd: Int
  val testBeamSize: Int
  val multiAlign: Boolean
}

trait HcrcConfig extends DefaultConfig {
  override val task = Hcrc
  override val pairFeatCountCutoff = 100
  override val eventFeatCountCutoff = 100
  override val sampleAlternatives: Optional[Int] = NotProvided
  override val nTrainIters = 15
  override val nTestIters = 5
  override val nTestAlignmentRestarts = 5
  override val testLengthRangeStart: Int = 15
  override val testLengthRangeEnd: Int = 15
  override val testBeamSize: Int = 15
  override val multiAlign: Boolean = false
}

trait SailConfig extends DefaultConfig {
  override val task = Sail
  override val pairFeatCountCutoff = 10
  override val eventFeatCountCutoff = 10
  override val sampleAlternatives: Optional[Int] = NotProvided
  override val nTrainIters = 1
  override val nTestIters = 5
  override val nTestAlignmentRestarts = 1
  override val testLengthRangeStart: Int = 1
  override val testLengthRangeEnd: Int = 2
  override val testBeamSize: Int = 15
  override val multiAlign: Boolean = true
}

case class Config() extends SailConfig

object Main extends Experiment[Config] {
  override val paramManifest = manifest[Config]
  override val stages = Seq(Load, Train, Test)
}
