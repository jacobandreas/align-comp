package framework.igor.eval

/**
 * @author jda
 */
class EvalStats(val truePositives: Double, val falseNegatives: Double, val falsePositives: Double) {

  def +(other: EvalStats) = new EvalStats(truePositives + other.truePositives,
                                          falseNegatives + other.falseNegatives,
                                          falsePositives + other.falsePositives)

  val precision =
    if (truePositives + falsePositives > 0)
      truePositives / (truePositives + falsePositives)
    else 0
  val recall =
    if (truePositives + falseNegatives > 0)
      truePositives / (truePositives + falseNegatives)
    else 0
  val f1 =
    if (precision + recall > 0)
      2 * precision * recall / (precision + recall)
    else 0

  val accuracy =
    if (truePositives + falsePositives + falseNegatives > 0)
      truePositives / (truePositives + falsePositives + falseNegatives)
    else 0

  override def toString = prfString
  def prfString = "p: %f\tr: %f\tf1: %f".format(precision, recall, f1)
  def accString = "acc: %f".format(accuracy)

}

object EvalStats {
  def apply(truePositives: Double, falseNegatives: Double, falsePositives: Double) = new EvalStats(truePositives, falseNegatives, falsePositives)
}
