package model

/**
 * @author jda
 */
trait SentenceObservation {

}

trait EventObservation {
  val eventFeatures: Array[Array[Int]]
  val iEventRoot: Int
}

trait PairObservation extends SentenceObservation with EventObservation {
  val pairFeatures: Array[Array[Array[Int]]]
}

case class EventObservationImpl(eventFeatures: Array[Array[Int]], iEventRoot: Int) extends EventObservation
case class PairObservationImpl(pairFeatures: Array[Array[Array[Int]]], eventFeatures: Array[Array[Int]], iEventRoot: Int) extends PairObservation
