package main

import framework.igor.experiment.{Stage, Experiment}
import task.TaskFactory
import task.hcrc.Hcrc

/**
 * @author jda
 */

trait DefaultConfig {
  val dataDir: String = "data"
  val usePar: Boolean = true
  val task: TaskFactory
  val nTrainIters: Int
}

trait HcrcConfig extends DefaultConfig {
  override val task: TaskFactory = Hcrc
  override val nTrainIters: Int = 15
}

case class Config() extends HcrcConfig

object Main extends Experiment[Config] {
  override val paramManifest = manifest[Config]
  override val stages = Seq(Load, Train, Test)
}
