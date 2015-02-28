package main

import java.io.File

import framework.igor.experiment.{ResultCache, Stage}
import model._
import task.Task

/**
 * @author jda
 */
object Load extends Stage[Config] {
  override def run(implicit config: Config, cache: ResultCache): Unit = {
    val scorer = CompleteSparseBagScorer
    val model = GoodModel

    val task = config.task(new File(config.dataDir))
    val instances = task.instances
    val trainInstances = task.trainIds.map(instances)
    val testInstances = task.testIds.map(instances)

    val representations = trainInstances.map(Annotator.apply(task))
    val index = FeatureIndex.buildFrom(representations)
    val obsCache = TrainObservationCache.buildFrom(representations, index)

    cache.put('scorer, scorer)
    cache.put('model, model)
    cache.put('task, task)
    cache.put('index, index)
    cache.put('trainObsCache, obsCache)
    cache.put('testInstances, testInstances)
  }
}
