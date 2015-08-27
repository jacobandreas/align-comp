package framework.igor.experiment

import framework.igor.logging.Logging

/**
 * @author jda
 */
trait Stage[P] extends Logging {
  def run(implicit config: P, cache: ResultCache)
}
