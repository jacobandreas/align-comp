package framework.igor.config

/**
 * @author jda
 */
case class ExecutionParams (
  resume: Boolean = false,
  resumeFrom: Int = -1,
  experimentPath: String,
  randomSeed: Int = 0
)
