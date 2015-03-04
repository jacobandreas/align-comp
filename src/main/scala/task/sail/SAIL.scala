package task.sail

import java.io.File

import breeze.linalg.max
import breeze.numerics.abs
import epic.trees.AnnotatedLabel
import framework.fodor.{Feature, SimpleFeature}
import framework.fodor.graph._
import framework.igor.eval.EvalStats
import main.Config
import task._

import scala.collection.mutable
import scala.xml.XML

/**
 * @author jda
 */
object Sail extends TaskFactory {
  override def apply(dataRoot: File)(implicit config: Config): Task = new Sail(new File(dataRoot, "sail"), "en")
}

class Sail(root: File, language: String) extends Task with Serializable {

  val transcriptDir = new File(root, "data")
  val mapDir = new File(root, "maps")
  val paragraphTranscriptFileName = if (language == "zh") "Paragraph_Mandarin.xml" else "Paragraph.xml"
  val sentenceTranscriptFileName = "SingleSentence.xml"

  final val Collapse = true

  val maps: Map[String,SailMap] = {
    mapDir.listFiles.map { mapFile =>
      val doc = XML.loadFile(mapFile)
      val mapName = (doc \@ "name").toLowerCase
      val nodeMap = (doc \ "nodes" \ "node").map { node =>
        val x = (node \@ "x").toInt
        val y = (node \@ "y").toInt
        val item = node \@ "item"
        val sailNode = SailNode(x, y, item)
        (x, y) -> sailNode
      }.toMap
      val edgeMap = (doc \ "edges" \ "edge").flatMap { edge =>
        val pos1 = (edge \@ "node1").split(",").map(_.toInt) match { case Array(x, y) => (x, y) }
        val pos2 = (edge \@ "node2").split(",").map(_.toInt) match { case Array(x, y) => (x, y) }
        val wall = edge \@ "wall"
        val floor = edge \@ "floor"
        val node1 = nodeMap(pos1)
        val node2 = nodeMap(pos2)
        val sailEdge = SailEdge(node1, node2, wall, floor)
        Seq(
          (pos1, pos2) -> sailEdge,
          (pos2, pos1) -> sailEdge
        )
      }.toMap
      mapName -> new SailMap(nodeMap, edgeMap)
    }.toMap
  }

  override def instances: IndexedSeq[Instance] = {
    val transcriptFile = new File(transcriptDir, sentenceTranscriptFileName)
    val doc = XML.loadFile(transcriptFile)
    (doc \ "example").map { example =>
//      val startStr = (example \@ "start").drop(1).dropRight(1).split(", ")
//      val start = (startStr(0).toInt, startStr(1).toInt)
//      val endStr = (example \@ "end").drop(1).dropRight(1).split(", ")
//      val end = (endStr(0).toInt, endStr(1).toInt)
      val mapName = (example \@ "map").toLowerCase
      val map = maps(mapName)
      val instruction = (example \ "instruction").text
      val sentences = instruction.split("""\.""").toSeq.map(_.trim.toLowerCase).filterNot(_.isEmpty).toIndexedSeq;
      val splitSentences = sentences.flatMap { sentence =>
        import framework.arbor.{parse,segmentWords}
        import framework.arbor.BerkeleyAnnotators.{parser,wordSegmenter}
        val words = segmentWords(sentence)
        val tree = parse(words)
        val vpCoordinators = tree.allChildren.flatMap { subtree =>
          val vpChildren = subtree.children.count(_.label.label == "VP")
          val ccChild = subtree.children.find(_.label.label == "CC")
          if (vpChildren == 2 && ccChild.isDefined && words(ccChild.get.begin) == "and") Some(ccChild)
          else None
        }.flatten
        if (vpCoordinators.nonEmpty) {
          val index = vpCoordinators.next().begin
          Seq(words.slice(0, index).mkString(" "), words.slice(index + 1, words.length).mkString(" "))
        } else {
          Some(sentence)
        }
      }
      println(splitSentences.mkString("\n"))
      println()
      val path = (example \ "path").text.trim.drop(1).dropRight(1)
      val sites = """\((\d+), (\d+), ([-\d]+)\)""".r.findAllIn(path)
      val posns = sites.matchData.map { mtch =>
        val x = mtch.group(1).toInt
        val y = mtch.group(2).toInt
        val rawOrientation = mtch.group(3).toInt
        val orientation = if (rawOrientation == -1) 0 else rawOrientation
        (x, y, orientation)
      }.toSeq
      val startState = SailState(map.nodes(posns.head._1, posns.head._2), posns.head._3, map)
      val states = posns.tail.foldLeft(startState :: Nil) { (prevStates, posn) =>
        val last = prevStates.head
        if (Collapse && prevStates.length >= 2 && prevStates.tail.head.orientation == last.orientation && last.orientation == posn._3) {
          val next = SailState(map.nodes(posn._1, posn._2), posn._3, map)
          next :: prevStates.tail
//          next :: prevStates
        } else {
          val next = SailState(map.nodes(posn._1, posn._2), posn._3, map)
          next :: prevStates
        }
      }.reverse.toIndexedSeq

      if (states.length == 1) {
        val state = states.head
        val act = VerifyAction(state)
        TaskInstance(IndexedSeq((state, act, state)), splitSentences)
//        LabeledInstanceImpl(IndexedSeq(), Walkthrough(sentences))
      } else {
        val transitions = states.sliding(2).map { case IndexedSeq(s1, s2) =>
          val action: SailAction = if (s1.node.x == s2.node.x && s1.node.y == s2.node.y) {
            val rawAngle = s2.orientation - s1.orientation
            val angle = max(rawAngle, rawAngle + 360) % 360
            RotateAction(s1, angle)
          } else {
            assert { s1.orientation == s2.orientation }
//            assert { s1.node.x == s2.node.x || s1.node.y == s2.node.y }
            if (s1.node.x != s2.node.x && s1.node.y != s2.node.y) {
              println("!!! BAD PATH")
            }
            val dist = max(abs(s1.node.x - s2.node.x), abs(s1.node.y - s2.node.y))
//            println(dist)
            MoveAction(s1, dist)
          }
//          println()
//          println(s1)
//          println(action)
//          println(doAction(s1, action))
//          println(s2)
          // some annotations are bad
          // assert { doAction(s1, action) == s2 }
          (s1, action, s2)
        }.toIndexedSeq.map {
          case (s1, a: RotateAction, s2) if a.angle == 0 => {
            assert (s1.orientation == 0)
            val ns1 = s1.copy(orientation = 90)
            (ns1, RotateAction(ns1, 270), s2)
          }
          case triple => triple
        }


        val finalTransitions = transitions
//          if (transitions.isEmpty) IndexedSeq((startState, VerifyAction(startState), startState.copy(step = startState.step+1, stayedLast = true)))
//          else transitions

//        val collapsedTransitions = mutable.Queue[(SailState,SailAction,SailState)]()
//        collapsedTransitions.enqueue(transitions.head)
//        transitions.tail.foreach { case (s1, a, s2) =>
//          val (os1, oa, os2) = collapsedTransitions.last
//          if (oa.isInstanceOf[MoveAction])
//        }

        TaskInstance(finalTransitions, splitSentences)
      }
    }.toIndexedSeq
  }

  def idsAndMaps(fileName: String): IndexedSeq[(String,Int)] = {
    val transcriptFile = new File(transcriptDir, fileName)
    val doc = XML.loadFile(transcriptFile)
    (doc \ "example").zipWithIndex.map { case (example, id) =>
      //      val startStr = (example \@ "start").drop(1).dropRight(1).split(", ")
      //      val start = (startStr(0).toInt, startStr(1).toInt)
      //      val endStr = (example \@ "end").drop(1).dropRight(1).split(", ")
      //      val end = (endStr(0).toInt, endStr(1).toInt)
      val mapName = (example \@ "map").toLowerCase
      (mapName, id)
    }.toIndexedSeq
  }

//  override val trainIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).map(_._2)
//  override val testIds = IndexedSeq[Int]()
//  override val trainIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "grid" || name == "l" }.map(_._2)
//  override val testIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "jelly" }.map(_._2)
//  override val trainIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "l" || name == "jelly" }.map(_._2)
//  override val testIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "grid" }.map(_._2)
  override val trainIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "jelly" || name == "grid" }.map(_._2)
  override val testIds: IndexedSeq[Int] = idsAndMaps(sentenceTranscriptFileName).filter { case (name, id) => name == "l" }.map(_._2)

  def stepDirection(posn: (Int, Int), orientation: Int, distance: Int): (Int, Int) = {
    orientation match {
      case 0 => (posn._1, posn._2 - distance)
      case 90 => (posn._1 + distance, posn._2)
      case 180 => (posn._1, posn._2 + distance)
      case 270 => (posn._1 - distance, posn._2)
    }
  }

  override def availableActions(state: State): Set[Action] = {
    val posn = (state.node.x, state.node.y)
    val actBuilder = Set.newBuilder[Action]

    if (Collapse) {
      var keepForward = true
      var last = posn
      var steps = 1
      while (keepForward) {
        val forward = stepDirection(last, state.orientation, 1)
        if (state.map.edges.contains(last, forward)) {
          actBuilder += MoveAction(state, steps)
          steps += 1
          last = forward
        } else {
          keepForward = false
        }
      }
    } else {
      val forward = stepDirection(posn, state.orientation, 1)
      if (state.map.edges.contains(posn, forward)) actBuilder += MoveAction(state, 1)
    }

    Set(90, 180, 270).foreach(angle => actBuilder += RotateAction(state, angle))

//    if (!state.stayedLast) actBuilder += VerifyAction(state)
    actBuilder += VerifyAction(state)

    val r = actBuilder.result()
    r
  }

  override def doAction(state: State, action: Action): SailState = {
    action match {
      case a:MoveAction =>
        val posn = (state.node.x, state.node.y)
        val forward = stepDirection(posn, state.orientation, a.distance)
//        println(posn, forward)
//        println(state.map.edges.keys)
//        assert { state.map.edges.contains(posn, forward) }
        SailState(state.map.nodes(forward), state.orientation, state.map)

      case a:RotateAction =>
        val newOrientation = (state.orientation + a.angle + 360) % 360
        SailState(state.node, newOrientation, state.map)

      case a:VerifyAction =>
        state.copy()
    }
  }

  override def visualize(pred: IndexedSeq[(State, Action, State)], gold: IndexedSeq[(State, Action, State)]): Unit = Unit

  override def score(pred: IndexedSeq[(State, Action, State)], gold: IndexedSeq[(State, Action, State)]): EvalStats = {
    // TODO FIXME
    if (gold.length == 0) return EvalStats(0, 1, 0)
    val predLast = pred.last._3.node
    val goldLast = gold.last._3.node
    val predLastDir = pred.last._3.orientation
    val goldLastDir = gold.last._3.orientation
    val correct = predLast == goldLast && predLastDir == goldLastDir
    val score = if (correct) 1 else 0
    EvalStats(score, 1-score, 0)
  }

  override type Action = SailAction
  override type State = SailState

  override def represent(s1: State, a: Action, s2: State): EventContext = {
    val eventFeats: Set[Feature] = a match {
      case MoveAction(startState, distance) => Set(SimpleFeature("action:move"), SimpleFeature("distance:" + distance))
      case RotateAction(startState, angle) => Set(SimpleFeature("action:rotate"), SimpleFeature("angle:" + angle))
      case VerifyAction(startState) => Set(SimpleFeature("action:verify"))
    }
    val relationBuilder = Set.newBuilder[Relation]
    val event = Event(eventFeats + SimpleFeature("EVENT_CONST"))
//    val fromNode = buildNode(s1.node, relationBuilder, "before", s1.map, s1.orientation)
//    val fromNode = Entity(Set(SimpleFeature("item=" + s1.node.item)))
    val toNode = buildNode(s2.node, relationBuilder, "after", s2.map, s2.orientation)
//    val toNode = Entity(Set(SimpleFeature("item=" + s2.node.item)))
//    relationBuilder += Relation(event, fromNode, Set(SimpleFeature("from")))
//    relationBuilder += Relation(event, fromNode, Set(SimpleFeature("before"))) // to")))
    relationBuilder += Relation(event, toNode, Set(SimpleFeature("after"))) // to")))
    if (s1.node != s2.node) {
      try {
        //      val edge = s1.map.edges((s1.node.x, s1.node.y),(s2.node.x, s2.node.y))
//        val xDir = signum(s2.node.x - s1.node.x).toInt
//        val yDir = signum(s2.node.y - s1.node.y).toInt
//        val startEdge = s1.map.edges((s1.node.x, s1.node.y),(s1.node.x + xDir, s1.node.y + yDir))
//        val endEdge = s1.map.edges((s2.node.x-xDir, s2.node.y-yDir),(s2.node.x, s2.node.y))
//        val via = Entity(Set(SimpleFeature("startWall=" + startEdge.wall), SimpleFeature("startFloor=" + startEdge.floor),
//                              SimpleFeature("endWall=" + endEdge.wall), SimpleFeature("endFloor=" + endEdge.floor)))
      //     SimpleFeature("startWall=" + startEdge.wall), SimpleFeature("startFloor=" + startEdge.floor)))
//        relationBuilder += Relation(event, via, Set(SimpleFeature("via")))
      } catch {
        case e: NoSuchElementException =>
      }
    }
    EventContext(event, GraphWorld(relationBuilder.result()))
  }

  def buildNode(node: SailNode, relationBuilder: mutable.Builder[Relation,Set[Relation]], position: String, map: SailMap, orientation: Int, depth: Int = 1): Entity = {

    // TODO there is a better way
    val (leftX, leftY) = orientation match {
      case 0 => (-1, 0)
      case 90 => (0, -1)
      case 180 => (1, 0)
      case 270 => (0, 1)
    }
    val (frontX, frontY) = orientation match {
      case 0 => (0, -1)
      case 90 => (1, 0)
      case 180 => (0, 1)
      case 270 => (-1, 0)
    }
    val (rightX, rightY) = orientation match {
      case 0 => (1, 0)
      case 90 => (0, 1)
      case 180 => (-1, 0)
      case 270 => (0, -1)
    }
    val (backX, backY) = orientation match {
      case 0 => (0, 1)
      case 90 => (-1, 0)
      case 180 => (0, -1)
      case 270 => (1, 0)
    }

    val left = map.nodes.get(node.x + leftX, node.y + leftY)
    val front = map.nodes.get(node.x + frontX, node.y + frontY)
    val right = map.nodes.get(node.x + rightX, node.y + rightY)
    val back = map.nodes.get(node.x + backX, node.y + backY)

//    val left = map.nodes.get(node.x - 1, node.y)
//    val front = map.nodes.get(node.x, node.y - 1)
//    val right = map.nodes.get(node.x + 1, node.y)
//    val back = map.nodes.get(node.x, node.y + 1)

    val nodes = Seq(left, front, right, back)
    val names = Seq("left", "front", "right", "back")
    val edges = nodes map {
      case None => None
      case Some(nNode) => map.edges.get((node.x, node.y), (nNode.x, nNode.y))
    }

    val itemFeatsHere =
      if (!node.item.isEmpty) Set[Feature](SimpleFeature("has-item"), SimpleFeature("item=" + node.item))
      else Set[Feature]()
    val nNeighbors = nodes.flatten.length
    val neighborFeatsHere = // Set[Feature]() // Set[Feature](SimpleFeature("neighbors=" + nNeighbors))
//      Set(if (nNeighbors >= 3) Some(SimpleFeature("intersection")) else None).flatten
//        Set(SimpleFeature("neighbors=" + nNeighbors)) ++
        Set(
          if (edges(1).isEmpty)
            Some(SimpleFeature("facing-wall"))
          else
            None).flatten
    val gHere = Entity(itemFeatsHere ++ neighborFeatsHere) // + SimpleFeature("pos=here")) // + SimpleFeature("step=" + position))

    val gNeighbors = nodes zip edges zip names map {
      case ((_, None), name) =>
        val gNeighbor = Entity(Set(SimpleFeature("wall"))) // ,  SimpleFeature("pos=here")))
//        val gNeighbor = Entity(Set(SimpleFeature("wall")))
        relationBuilder += Relation(gHere, gNeighbor, Set(SimpleFeature("edge"))) // name)))
      case ((Some(nNode), Some(nEdge)), name) =>
        val hallFeats = Set[Feature](SimpleFeature("wall=" + nEdge.wall),
                                     SimpleFeature("floor=" + nEdge.floor))
        val itemFeats =
          if (!nNode.item.isEmpty) Set[Feature](SimpleFeature("has-item"), SimpleFeature("item=" + nNode.item))
          else Set[Feature]()
        val gNeighbor = Entity(hallFeats ++ itemFeats) //+ SimpleFeature("pos=" + name))
        relationBuilder += Relation(gHere, gNeighbor, Set(SimpleFeature("edge"))) // name)))
    }

    gHere


//  def buildNode(node: SailNode, relationBuilder: mutable.Builder[Relation,Set[Relation]], map: SailMap, depth: Int = 1): Entity = {
//    val left = map.nodes.get(node.x - 1, node.y)
//    val front = map.nodes.get(node.x, node.y - 1)
//    val right = map.nodes.get(node.x + 1, node.y)
//    val back = map.nodes.get(node.x, node.y + 1)
//
//    val nodes = Seq(left, front, right, back)
//    val names = Seq("left", "front", "right", "back")
//
////    val nodes = Seq[Option[SailNode]](Some(node))
////    val names = Seq("here")
//
//    var numNeighbors = 0
//
//    val feats = nodes zip names flatMap {
//      case (None, name) => Set[Feature]()// Set(SimpleFeature(s"$name-wall"))
//      case (Some(nNode), name) =>
//        val edge = map.edges.get((node.x, node.y), (nNode.x, nNode.y))
//        if (edge.isDefined) {
//          numNeighbors += 1
////          val theseHallFeats = Set(SimpleFeature(s"$name-wall=${edge.get.wall}"),
////              SimpleFeature(s"$name-floor=${edge.get.floor}"),
////              SimpleFeature(s"floor=${edge.get.floor}"),
////              SimpleFeature(s"wall=${edge.get.wall}"))
//
//          val theseHallFeats = Set[Feature](SimpleFeature(s"wall=${edge.get.wall}"),
//                                            SimpleFeature(s"floor=${edge.get.floor}"))
//          val theseItemFeats =
//            if (!node.item.isEmpty) Set[Feature](SimpleFeature("item=" + nNode.item))
//            else Set()
//
//          theseHallFeats // ++ theseItemFeats
//        } else {
////          Set(SimpleFeature(s"$name-wall"))
//          Set[Feature]()
//        }
//    }

//    val itemFeats: Set[Feature] =
//      if (!node.item.isEmpty) Set(SimpleFeature("item="+node.item))
//        else Set()
//
//    val outNode = Entity(
//      feats.toSet ++
//      itemFeats +
//      SimpleFeature("num-neighbors=" + numNeighbors) +
//      SimpleFeature("ENTITY_CONST")
//    )

//    nodes zip names foreach {
//      case (Some(nNode), name) =>
//        val edge = map.edges.get((node.x, node.y), (nNode.x, nNode.y))
////        println(depth)
//        if (edge.isDefined && depth > 0) {
//          val oNode = buildNode(nNode, relationBuilder, map, 0)
//          relationBuilder += Relation(outNode, oNode, Set(SimpleFeature(name)))
//        }
//      case _ =>
//    }

//    outNode
  }

}

//case class SailAction(from: SailNode, edges: IndexedSeq[SailEdge]) extends GameAction {
//  val transitions = edges.foldLeft((edges.head, edges.head.nodeAfter(from)) :: Nil) { (seq, edge2) =>
//    val (edge, node) = seq.head
//    (edge2, edge2.nodeAfter(node)) :: seq
//  }.reverse
//  override val features: Set[Feature] = {
//    transitions.flatMap { case (edge, node) =>
//      Seq { BAD BAD BAD
//        SimpleFeature(s"floor=${edge.floor}")
//        SimpleFeature(s"wall=${edge.wall}")
//        SimpleFeature(s"item=${node.item}")
//      }
//    }.toSet
//  }
//}
sealed trait SailAction extends TaskAction
case class MoveAction(startState: SailState, distance: Int) extends SailAction {
//  override val features: Set[Feature] = Set {
//    SimpleFeature("action=move")
//    SimpleFeature(s"distance=$distance")
//  }
}
case class RotateAction(startState: SailState, angle: Int) extends SailAction {
//  override val features: Set[Feature] = Set {
//    SimpleFeature("action=rotate")
//    SimpleFeature(s"angle=$angle")
//  }
}

case class VerifyAction(startState: SailState) extends SailAction

case class SailState(node: SailNode, orientation: Int, map: SailMap) extends TaskState {
//  override val features: Set[Feature] = {
//    Set {
//      SimpleFeature(s"item=${node.item}")
//    }
//  }
}

case class SailNode(x: Int, y: Int, item: String)
case class SailEdge(node1: SailNode, node2: SailNode, wall: String, floor: String) {
  def nodeAfter(node: SailNode): SailNode = {
    require { node == node1 || node == node2 }
    if (node == node1) node2 else node1
  }
}

class SailMap(val nodes: Map[(Int,Int),SailNode], val edges: Map[((Int,Int),(Int,Int)),SailEdge]) extends Serializable {
  def neighbors(node: SailNode): Seq[SailNode] = {
    val positions = Seq((node.x+1,node.y),(node.x-1,node.y),(node.x,node.y+1),(node.x,node.y-1))
    positions.flatMap(nodes.get)
  }
}
