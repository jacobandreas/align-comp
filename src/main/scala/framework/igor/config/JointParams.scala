package framework.igor.config

/**
 * @author jda
 */
case class JointParams[T] (
  execution: ExecutionParams,
  experiment: T
)
