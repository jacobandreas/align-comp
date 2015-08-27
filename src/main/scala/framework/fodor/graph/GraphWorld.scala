package framework.fodor.graph

import framework.erector.graph.{LabeledGraph, Graph}
import framework.fodor.Feature

/**
 * @author jda
 */

sealed trait Thing { val features: Set[Feature] }
case class Event(features: Set[Feature]) extends Thing
case class Entity(features: Set[Feature]) extends Thing

case class Relation(from: Thing, to: Thing, features: Set[Feature])

case class GraphWorld(relations: Set[Relation]) extends LabeledGraph[Thing,Relation] {
  val below = relations.groupBy(_.from).withDefaultValue(Set())
  val above = relations.groupBy(_.to).withDefaultValue(Set())
  override val labeledEdges = relations.map { r => (r.from, r, r.to) }
}

case class EventContext(event: Event, model: GraphWorld)
