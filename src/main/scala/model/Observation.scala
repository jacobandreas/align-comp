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
  val visitOrder: Array[Int]
  val pairFeatures: Array[Array[Array[Int]]]
  val dependencies: Array[Array[Boolean]]
  val edges: Array[Array[Boolean]]
}

case class EventObservationImpl(eventFeatures: Array[Array[Int]], iEventRoot: Int) extends EventObservation
case class PairObservationImpl(visitOrder: Array[Int],
                               pairFeatures: Array[Array[Array[Int]]],
                               dependencies: Array[Array[Boolean]],
                               edges: Array[Array[Boolean]] ,
                               eventFeatures: Array[Array[Int]],
                               iEventRoot: Int) extends PairObservation
