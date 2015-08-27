package framework.erector.util

import java.util
import collection.JavaConversions._

/**
 * @author jda
 */

class Beam[T,O](size: Int)(scorer: T => O)(implicit ordering: Ordering[O]) {

  val comparator = new util.Comparator[T] {
    override def compare(x: T, y: T): Int = ordering.compare(scorer(x), scorer(y))
  }

  val heap = new util.PriorityQueue[T](size, comparator)

  def add(t: T) {
    if (heap.size < size) {
      heap.add(t)
    } else {
      val first = heap.peek
      if (comparator.compare(t, first) > 0) {
        heap.add(t)
        heap.poll()
      }
    }
  }

  def addAll(tt: TraversableOnce[T]) {
    val iter = tt.toIterator
    while (iter.hasNext) {
      add(iter.next())
    }
  }

  def remove(t: T) {
    heap.remove(t)
  }

  def result() = heap.toIndexedSeq
}

class ViterbiBeam[T,O,P](size: Int)(scorer: T => O, projector: T => P)(implicit ordering: Ordering[O]) {
  val beam = new Beam[T,O](size)(scorer)
  val projections = new util.HashMap[P,T]

  def add(t: T) {
    val proj = projector(t)
//    assert(projections.values.forall(beam.heap.contains))
    if (!projections.contains(proj)) {
      beam.add(t)
      projections.put(proj, t)
    } else if (ordering.compare(scorer(t), scorer(projections.get(proj))) > 0) {
//      val preSize = beam.heap.size
      val old = projections.get(proj)
      beam.remove(old)
      beam.add(t)
      projections.put(proj, t)
//      val postSize = beam.heap.size
//      assert(postSize == preSize)
    }
  }
  def result() = beam.result()
}

