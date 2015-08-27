package framework.igor.experiment

import breeze.config.CommandLineParser
import framework.igor.config.JointParams

import scala.reflect.ClassTag

/**
 * @author jda
 */
trait Experiment[P] {

  protected implicit val paramManifest: Manifest[P]
  def stages: Seq[Stage[P]]

  def main(args: Array[String]) {
    val config: JointParams[P] = CommandLineParser.readIn[JointParams[P]](args)
    new Execution(config).run(this)
  }
}
