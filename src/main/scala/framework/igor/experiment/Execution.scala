package framework.igor.experiment

import framework.igor.config.JointParams

import java.io._
import breeze.stats.distributions.Rand
import framework.igor.logging.Logging

/**
 * @author jda
 */
class Execution[P](val config: JointParams[P]) extends Logging {

  // TODO(jda) prompt here?
  val resume = config.execution.resume
  val env = new Environment(config.execution, resume)
  var currentStageIndex = 0
  var fromStageIndex = if (config.execution.resumeFrom != -1) config.execution.resumeFrom else getStageIndex()
  val resultCache = new ResultCache(env)

  def run(experiment: Experiment[P]) = {
    resultCache.writeFile('Config, config.toString)
    experiment.stages foreach { stage => runStage(stage) }
  }

  def runStage(stage: Stage[P]): Unit = {
    putStageIndex()
    val stageName = stage.getClass.getSimpleName
    if (currentStageIndex < fromStageIndex) {
      logger.info(s"skipping $stageName")
    } else {
      //logger.info(s"Running ${stageName.name}")
      task(s"running $stageName") {
        try {
          stage.run(config.experiment, resultCache)
        } catch {
          case e: Throwable =>
            saveState()
            e.printStackTrace()
            sys.exit(1)
        }
      }
    }
    currentStageIndex += 1
  }

  private def saveState(): Unit = {
    putStageIndex()
  }

  private def putStageIndex(): Unit = {
    val f = new File(env.workDir, "STAGE")
    val writer = new BufferedWriter(new FileWriter(f))
    try {
      writer.write(currentStageIndex.toString)
      writer.newLine()
    } finally {
      writer.close()
    }
  }

  private def getStageIndex(): Int = {
    val f = new File(env.workDir, "STAGE")
    if (f.exists) {
      val reader = new BufferedReader(new FileReader(f))
      try {
        reader.readLine().toInt
      } finally {
        reader.close()
      }
    } else {
      0
    }
  }

  // TODO as lists of symbols?
  def withResource[A](path: String)(op: FileInputStream => A): A = {
    assert(env.runDir != null)
    val f = new File(env.runDir, path)
    val stream = new FileInputStream(f)
    try {
      op(stream)
    } finally {
      stream.close()
    }
  }

  def promptForResume(): Boolean = {
    val target = Environment.latestLinkTarget(config.execution.experimentPath)
    if (target.isEmpty)
      false
    else {
      println(target)
      print("Resume previous run? ")
      def query() = readLine("[Yn] ")
      val responseMap = Map("" -> true, "y" -> true, "n" -> false)
      val inputs = Stream.continually(query())
      responseMap(inputs.find(responseMap contains _.toLowerCase).get)
    }
  }

}

object Execution {
  def makeSplit[T](instances: IndexedSeq[T], testFrac: Double): (IndexedSeq[T], IndexedSeq[T]) = {
    val nTest: Int = (testFrac * instances.length).toInt
    val testInstances = Rand.subsetsOfSize(instances, nTest).draw()
    val trainInstances = instances.filter(!testInstances.contains(_))
    (trainInstances, testInstances)
  }
}
