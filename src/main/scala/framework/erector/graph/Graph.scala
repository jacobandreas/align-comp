package framework.erector.graph

import framework.igor.util.Memoize

import scala.collection.mutable

/**
 * @author jda
 */

trait Graph[T] {
  val edges: Set[(T,T)]
  lazy val nodes: Set[T] = edges.flatMap(e => Seq(e._1, e._2))
  lazy val neighbors: Map[T,Set[T]] = edges.groupBy(_._1).map { case (n, es) => n -> es.map(_._2) }.withDefaultValue(Set())
//  require { edges.forall { e => (nodes contains e._1) && (nodes contains e._2) } }

  val path: Memoize[(T,T),Option[IndexedSeq[T]]] = Memoize { case (from, to) =>
    if (edges contains (from, to)) {
      Some(IndexedSeq(from,to))
    } else {
      val npaths: Set[IndexedSeq[T]] = neighbors(from).flatMap { neighbor =>
        val npath: Option[IndexedSeq[T]] = path((neighbor, to))
        npath
      }
      if (npaths.isEmpty) {
        None
      } else {
        val spath = npaths.minBy(_.length)
        Some(from +: spath)
      }
    }
  }
  //def path(from: T, to: T): IndexedSeq[T] = {
  //
  //}

  def bfs(from: T): Iterator[T] = {
    new Iterator[T] {
      val visited = mutable.Set[T]()
      var queue = mutable.Queue(from)
      override def hasNext: Boolean = queue.nonEmpty
      override def next(): T = {
        val nextNode = queue.dequeue()
        visited.add(nextNode)
        neighbors(nextNode).filterNot(visited.contains).foreach(queue.enqueue(_))
        nextNode
      }
    }
  }
}

trait RootedGraph[T] extends Graph[T] {
  val root: T
//  require { nodes contains root }
}

trait LabeledGraph[T,L] extends Graph[T] {
  val labeledEdges: Set[(T,L,T)]
  lazy val labeledNeighbors: Map[T,Set[(L,T)]] = labeledEdges.groupBy(_._1).map { case (n, es) => n -> es.map(e => (e._2, e._3)) }.withDefaultValue(Set())
  override lazy val edges = labeledEdges.map(e => (e._1, e._3))
}

case class GraphImpl[T](edges: Set[(T,T)]) extends Graph[T]

case class RootedGraphImpl[T](edges: Set[(T,T)], root: T) extends RootedGraph[T]

//case class Graph[T](edges: Set[(T,T)]) {
//  val nodes = edges.flatten
//}
//
//case class RootedGraph[T] e

