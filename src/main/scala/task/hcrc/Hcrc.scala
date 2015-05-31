package task.hcrc

import java.io.{BufferedReader, FileReader, File, FilenameFilter}
import java.util.function.Consumer

import breeze.linalg.min
import breeze.numerics.{abs, signum}
import breeze.{plot => bplot}
import framework.fodor.{IndicatorFeature, SimpleFeature, StringFeature, RealFeature}
import framework.fodor.graph._
import framework.igor.eval.EvalStats
import main.Config
import task._

import scala.io.Source

/**
 * @author jda
 */
object Hcrc extends TaskFactory {
  final val LandmarkRadius = 75
  final val LandmarkOffset = 25d
  final val InterpSteps = 15
  final val HcrcDirName = "hcrc"
  final val LandmarksDirName = "landmarks"
  final val CorrespondenceFileName = "correspondences.txt"
  override def apply(dataRoot: File)(implicit config: Config): Task = new Hcrc(new File(dataRoot, HcrcDirName))

  val whitelistWords = Set("north", "east", "south", "west", "above", "below", "left", "right", "top", "bottom", "side", "underneath")
  val pairFeatureFilter = (feature: IndicatorFeature) => feature.value.contains("Match") || (feature.value.contains("Side") && whitelistWords.exists(feature.value.contains))
  val eventFeatureFilter = (feature: IndicatorFeature) => feature.value.contains("from") || feature.value.contains("to") || feature.value.contains("dist") || feature.value.contains("same")
}

class Hcrc(hcrcRoot: File) extends Task with Serializable{

  override type State = HcrcState
  override type Action = HcrcAction

  val correspondenceFile = new File(hcrcRoot, Hcrc.CorrespondenceFileName)

  val walkthroughs: Map[String,IndexedSeq[String]] = {
    val walkthroughsDir = new File(hcrcRoot, "transcripts")
    val walkthroughFiles = walkthroughsDir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String) = name.matches("q\\d(e|n)c\\d.txt")
    })
    walkthroughFiles.map { file =>
      val transcript = Source.fromFile(file).getLines().filter(_.startsWith("g\t")).map(_.drop(2)).toIndexedSeq
      file.getName.dropRight(4) -> transcript
    }.toMap
  }

//  val startingPositions: IndexedSeq[Point2D] = {
//    val landmarksDir = new File(hcrcRoot, Hcrc.LandmarksDirName)
//    landmarksDir.listFiles(new FilenameFilter {
//      override def accept(dir: File, name: String) = { name.matches("\\d+g.xml") }
//    }).map { file => (file, HcrcMap.fromLandmarksFile(file)) }
//      .map { case (file, map) =>
//      file.getName.dropRight(5).toInt -> map.start // HcrcState(map.start, map)
//    }.sortBy(_._1).map(_._2)
//  }

  val maps: IndexedSeq[HcrcMap] = {
    val landmarksDir = new File(hcrcRoot, Hcrc.LandmarksDirName)
    landmarksDir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String) = { name.matches("\\d+g.xml") }
    }).map { file => (file, HcrcMap.fromLandmarksFile(file)) }
      .map { case (file, map) => file.getName.dropRight(5).toInt -> map }
      .sortBy(_._1)
      .map(_._2)
  }

  override val instances: IndexedSeq[Instance] = {
    val points = maps.map { map =>
      map.path.drop(1).foldLeft(IndexedSeq(map.start)) { (seq, point) =>
        val delta = RelativePoint2D(point.x - seq.last.x, point.y - seq.last.y)
        seq :+ Point2D(seq.last.x + delta.x, seq.last.y + delta.y)
      }
    }
    val paths = (points zip maps).map { case (path, map) =>
      val transitions = extractTransitionsAndSides(path, map)
//      val actions = transitions.map(_._2)
//      val initDelta = RelativePoint2D(actions.head.x - path.head.pos.x, actions.head.y - path.head.pos.y)
//      val initTriple = (path.head, initDelta, doAction(path.head, initDelta))
      val initTriple = (transitions(0), transitions(1).toAction, transitions(1))
      transitions.drop(2).foldLeft(IndexedSeq(initTriple)) { (seq, state) =>
        val lastState = seq.last._3
//        val delta = RelativePoint2D(point.x - lastState.pos.x, point.y - lastState.pos.y)
        seq :+ (lastState, state.toAction, state)
      }
    }
    println(correspondenceFile)
//    val src = Source.fromFile(correspondenceFile)
    val lines = {
      val lineBuilder = IndexedSeq.newBuilder[String]
      val reader = new BufferedReader(new FileReader(correspondenceFile))
      reader.lines().forEach(new Consumer[String] {
        override def accept(t: String): Unit = lineBuilder += t
      })
      reader.close()
      lineBuilder.result()
    }
    lines.map { line =>
      val parts = line.split(",")
      val transcriptId = parts(0)
      val mapId = parts(1).toInt
      TaskInstance(paths(mapId), walkthroughs(transcriptId))
    }.toIndexedSeq
  }

  def extractTransitionsAndSides(points: IndexedSeq[Point2D], map: HcrcMap): IndexedSeq[HcrcState] = {
    val visits = points.flatMap { point =>
      val lm = map.landmarks.sortBy(_.center.distanceTo(point)).find(_.center.distanceTo(point) <= Hcrc.LandmarkRadius)
      if (!lm.isDefined) {
        None
      } else {
        val glm = lm.get
        val xPart = point.x - glm.center.x
        val yPart = point.y - glm.center.y
//        val (xOffset, yOffset) =
//          if (abs(xPart) >= abs(yPart)) {
//            (glm.center.x + Hcrc.LandmarkOffset * signum(xPart), glm.center.y)
//          } else {
//            (glm.center.x, glm.center.y + Hcrc.LandmarkOffset * signum(yPart))
//          }
        val side = //West
          if (abs(xPart) >= abs(yPart)) {
            if (xPart > 0) East else West
          } else {
            if (yPart > 0) South else North
          }
        //Some((glm, Point2D(xOffset, yOffset)))
        Some(HcrcState(map, glm, side))
      }
    }
    filterDuplicates(visits)
  }

  def filterDuplicates[T](s: Seq[T]) = {
    s.foldLeft(IndexedSeq[T]()) { (soFar, next) =>
      if (soFar.nonEmpty && soFar.last == next) soFar
      else soFar :+ next
    }
  }

//  def extractTransitions(states: IndexedSeq[State], landmarks: IndexedSeq[Landmark]): IndexedSeq[Landmark] = {
//    filterDuplicates(extractTransitionsAndSides(states, landmarks).map(_._1))
//  }

  override def doAction(state: State, action: Action) = {
//    HcrcState(state.pos + action, state.map)
    HcrcState(state.map, action.landmark, action.side)
  }

  def trainIds: IndexedSeq[Int] = {
    val src = Source.fromFile(correspondenceFile)
    src.getLines().zipWithIndex.filter(_._1.split(",")(1).toInt < 12).map(_._2).toIndexedSeq
  }

  def testIds: IndexedSeq[Int] = {
    val src = Source.fromFile(correspondenceFile)
    src.getLines().zipWithIndex.filter(_._1.split(",")(1).toInt >= 12).map(_._2).toIndexedSeq
  }

  override def availableActions(state: State): Set[Action] = {
//    val nearestLandmarks = state.map.landmarks.sortBy(_.center.distanceTo(state.pos))
    state.map.landmarks.flatMap { landmark =>
      Seq(
        HcrcAction(landmark, North),
        HcrcAction(landmark, East),
        HcrcAction(landmark, South),
        HcrcAction(landmark, West)
      )
    }.toSet
  }

  override def visualize(pred: IndexedSeq[(State,Action,State)], gold: IndexedSeq[(State,Action,State)]): Unit = {
    val predPath = pred.head._1 +: pred.map(_._3)
    val goldPath = gold.head._1 +: gold.map(_._3)
    val fig = bplot.Figure()
    val plot = fig.subplot(0)
    val map = goldPath.head.map
    val mapXs = map.path.map(_.x)
    val mapYs = map.path.map(_.y)
    val lmXs = map.landmarks.map(_.center.x)
    val lmYs = map.landmarks.map(_.center.y)
    val goldXs = goldPath.map(_.pos.x)
    val goldYs = goldPath.map(_.pos.y)
    val predXs = predPath.map(_.pos.x)
    val predYs = predPath.map(_.pos.y)
    plot += bplot.scatter(lmYs, lmXs, i => 2 * Hcrc.LandmarkRadius)
    plot += bplot.plot(mapYs, mapXs)
    plot += bplot.plot(goldYs, goldXs)
    plot += bplot.plot(predYs, predXs)
  }

  override def score(pred: IndexedSeq[(State,Action,State)], gold: IndexedSeq[(State,Action,State)]): EvalStats = {
    val goldLms = filterDuplicates((gold.map(_._1) :+ gold.last._3).map(_.landmark))
    val predLms = filterDuplicates((pred.map(_._1) :+ pred.last._3).map(_.landmark))

    val goldTransitions = goldLms.sliding(2).toSeq
    val predTransitions = predLms.sliding(2).toSeq

    val numCorrect = predTransitions.toSet.toIndexedSeq.map { pt: IndexedSeq[Landmark] =>
      val numPred = predTransitions.count(_ == pt)
      val numGold = goldTransitions.count(_ == pt)
      min(numPred, numGold)
    }.sum
    EvalStats(numCorrect, goldTransitions.length - numCorrect, predTransitions.length - numCorrect)
  }

  override def represent(s1: State, a: Action, s2: State): EventContext = {
    val dist = s1.pos.distanceTo(s2.pos)
    val distStr = if (dist < 1e-8) "zero" else (dist / 50).toInt
    val sameLandmark = s1.landmark eq s2.landmark
    val sameLandmarkAndSide = (s1.landmark eq s2.landmark) && (s1.side == s2.side)

    val toLandmark = Entity(Set(StringFeature("name", s2.landmark.name),
                                SimpleFeature("side=" + s2.side.toString),
                                SimpleFeature("to")))
    val fromLandmark = Entity(Set(StringFeature("name", s1.landmark.name), SimpleFeature("from")))

//    val event = Event(Set(StringFeature("toLandmark", s2.landmark.name),
//                          StringFeature("fromLandmark", s1.landmark.name),
//                          SimpleFeature("dist=" + distStr),
//                          SimpleFeature("toSide=" + s2.side.toString),
//                          SimpleFeature(s"sameLandmark=$sameLandmark")))



    val event = Event(Set(SimpleFeature("dist=" + distStr), SimpleFeature(s"same=$sameLandmark")))

    val relations = Set(Relation(event, toLandmark, Set(SimpleFeature("to"))),
                        Relation(event, fromLandmark, Set(SimpleFeature("from"))))
//                          SimpleFeature(s"sameLandmarkAndSide=$sameLandmarkAndSide")))
                          //RealFeature("length", s1.pos.distanceTo(s2.pos))))
    val world = GraphWorld(relations)
    EventContext(event, world)
//    val from = Entity(Set(StringFeature("landmark", s1.landmark.name), SimpleFeature("from")))
//    val to = Entity(Set(StringFeature("landmark", s2.landmark.name), SimpleFeature("to")))
  }
}

sealed trait Side { val xOffset: Double; val yOffset: Double }
case object North extends Side { val xOffset = 0d;                   val yOffset = -Hcrc.LandmarkOffset }
case object East extends Side  { val xOffset = Hcrc.LandmarkOffset;  val yOffset = 0d }
case object South extends Side { val xOffset = 0d;                   val yOffset = Hcrc.LandmarkOffset }
case object West extends Side  { val xOffset = -Hcrc.LandmarkOffset; val yOffset = 0d }

case class HcrcState(map: HcrcMap, landmark: Landmark, side: Side) extends TaskState {
  lazy val toAction = HcrcAction(landmark, side)
  lazy val pos = Point2D(landmark.center.x + side.xOffset, landmark.center.y + side.yOffset)
  override def toString = s"HcrcState(${landmark.name}:$side)"
}

case class HcrcAction(landmark: Landmark, side: Side) extends TaskAction
