package main

import framework.igor.experiment.{ResultCache, Stage}

/**
 * @author jda
 */
object Test extends Stage[Config] {
  override def run(implicit config: Config, cache: ResultCache): Unit = {
    logger.info("TEST!")
  }
}
