package framework.fodor.davidson

import framework.fodor.Feature

import scala.collection.generic.HasNewBuilder
import scala.collection.mutable.ArrayBuffer

/**
 * @author jda
 */

sealed trait Thing {
  val features: Set[Feature]
}

trait Entity extends Thing
trait Event extends Thing
trait Property extends Thing
trait Relation extends Thing {
  val from: Thing
  val to: Thing
}

case class SimpleEntity(features: Set[Feature]) extends Thing
case class SimpleEvent(features: Set[Feature]) extends Thing
case class SimpleProperty(features: Set[Feature]) extends Thing
case class SimpleRelation(from: Thing, to: Thing, features: Set[Feature]) extends Thing

trait World {
  def things: Set[Thing]
  val relations = things.collect { case r: Relation => r }
  // verify closure
  require {
    things.forall {
      case r: Relation => things.contains(r.from) && things.contains(r.to)
      case _ => true
    }
  }
  val below = relations.groupBy(_.from)
  val above = relations.groupBy(_.to)
}

case class SimpleWorld(things: Set[Thing]) extends World

case class BridgingWorld(event: Event, before: World, between: Set[Thing], after: World) extends World {
  require { between contains event }
  override val things = before.things | between | after.things
}

object World extends HasNewBuilder[Thing, World] {
  override def newBuilder = new ArrayBuffer[Thing].mapResult(result => SimpleWorld(result.toSet))
}
