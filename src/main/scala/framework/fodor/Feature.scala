package framework.fodor

import scala.runtime.ScalaRunTime

/**
 * @author jda
 */
sealed trait Feature

trait IndicatorFeature extends Feature {
  val value: String
}

case class SimpleFeature(value: String) extends IndicatorFeature {
  override val hashCode = value.hashCode
}

case class StringFeature(name: String, svalue: String) extends IndicatorFeature {
  val value = s"$name=$svalue"
  override val hashCode = value.hashCode
}

case class RealFeature(name: String, rvalue: Double) extends Feature {
  val value = s"$name=$rvalue"
  override val hashCode = value.hashCode
}
