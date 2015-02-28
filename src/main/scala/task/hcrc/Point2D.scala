package task.hcrc

import breeze.linalg.DenseVector
import breeze.numerics.sqrt
import task.TaskAction

/**
 * @author jda
 */
case class Point2D(x: Double, y: Double) {
  def distanceTo(other: Point2D): Double = {
    val diff = DenseVector(x, y) - DenseVector(other.x, other.y)
    val sqDist = diff dot diff
    sqrt(sqDist)
  }

  def +(d: RelativePoint2D) = Point2D(x + d.x, y + d.y)
}

case class RelativePoint2D(x: Double, y: Double) extends TaskAction
