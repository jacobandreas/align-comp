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
