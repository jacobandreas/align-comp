package task.hcrc

import java.io.{File, FilenameFilter}

import breeze.linalg.min
import breeze.numerics.{abs, signum}
import breeze.{plot => bplot}
import framework.fodor.{StringFeature,RealFeature}
import framework.fodor.graph.{GraphWorld, Event, EventContext}
import framework.igor.eval.EvalStats
import main.Config
import task._

import scala.io.Source

/**
 * @author jda
 */
object Hcrc extends TaskFactory {
  final val LandmarkRadius = 75
  final val LandmarkOffset = 25
  final val InterpSteps = 15
  final val HcrcDirName = "hcrc"
  final val LandmarksDirName = "landmarks"
  final val CorrespondenceFileName = "correspondences.txt"
  override def apply(dataRoot: File)(implicit config: Config): Task = new Hcrc(new File(dataRoot, HcrcDirName))
}

class Hcrc(hcrcRoot: File) extends Task with Serializable{

  override type State = HcrcState
  override type Action = RelativePoint2D

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

  val starts: IndexedSeq[State] = {
    val landmarksDir = new File(hcrcRoot, Hcrc.LandmarksDirName)
    landmarksDir.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String) = { name.matches("\\d+g.xml") }
    }).map {
             file => (file, HcrcMap.fromLandmarksFile(file))
    }.map { case (file, map) =>
      file.getName.dropRight(5).toInt -> HcrcState(map.start, map)
    }.sortBy(_._1).map(_._2)
  }

  override val instances: IndexedSeq[Instance] = {
    val states = starts.map { inst =>
      inst.map.path.drop(1).foldLeft(IndexedSeq(inst)) { (seq, point) =>
        val delta = RelativePoint2D(point.x - seq.last.pos.x, point.y - seq.last.pos.y)
        seq :+ doAction(seq.last, delta)
      }
    }
    val paths = states.map { path =>
      val transitions = extractTransitionsAndSides(path, path.head.map.landmarks)
      val actions = transitions.map(_._2)
      val initDelta = RelativePoint2D(actions.head.x - path.head.pos.x, actions.head.y - path.head.pos.y)
      val initTriple = (path.head, initDelta, doAction(path.head, initDelta))
      actions.drop(2).foldLeft(IndexedSeq(initTriple)) { (seq, point) =>
        val lastState = seq.last._3
        val delta = RelativePoint2D(point.x - lastState.pos.x, point.y - lastState.pos.y)
        seq :+ (lastState, delta, doAction(lastState, delta))
      }
    }
    val src = Source.fromFile(correspondenceFile)
    src.getLines().map { line =>
      val parts = line.split(",")
      val transcriptId = parts(0)
      val mapId = parts(1).toInt
      TaskInstance(paths(mapId), walkthroughs(transcriptId))
    }.toIndexedSeq
  }

  def extractTransitionsAndSides(states: IndexedSeq[State], landmarks: IndexedSeq[Landmark]): IndexedSeq[(Landmark, Point2D)] = {
    val visits = states.flatMap { state =>
      val lm = landmarks.sortBy(_.center.distanceTo(state.pos)).find(_.center.distanceTo(state.pos) <= Hcrc.LandmarkRadius)
      if (!lm.isDefined) {
        None
      } else {
        val glm = lm.get
        val xPart = state.pos.x - glm.center.x
        val yPart = state.pos.y - glm.center.y
        val (xOffset, yOffset) =
          if (abs(xPart) >= abs(yPart)) {
            (glm.center.x + Hcrc.LandmarkOffset * signum(xPart), glm.center.y)
          } else {
            (glm.center.x, glm.center.y + Hcrc.LandmarkOffset * signum(yPart))
          }
        Some((glm, Point2D(xOffset, yOffset)))
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

  def extractTransitions(states: IndexedSeq[State], landmarks: IndexedSeq[Landmark]): IndexedSeq[Landmark] = {
    filterDuplicates(extractTransitionsAndSides(states, landmarks).map(_._1))
  }

  override def doAction(state: State, action: Action) = {
    HcrcState(state.pos + action, state.map)
  }

  def trainIds: IndexedSeq[Int] = {
    val src = Source.fromFile(correspondenceFile)
    src.getLines().zipWithIndex.filter(_._1.split(",")(1).toInt < 12).map(_._2).toIndexedSeq
  }

  def testIds: IndexedSeq[Int] = {
    val src = Source.fromFile(correspondenceFile)
    src.getLines().zipWithIndex.filter(_._1.split(",")(1).toInt >= 12).map(_._2).toIndexedSeq
  }

  override def availableActions(state: State): Set[RelativePoint2D] = {
    val nearestLandmarks = state.map.landmarks.sortBy(_.center.distanceTo(state.pos)) take 4
    nearestLandmarks.flatMap { landmark =>
      Seq(
        RelativePoint2D(landmark.center.x - Hcrc.LandmarkOffset - state.pos.x, landmark.center.y - state.pos.y),
        RelativePoint2D(landmark.center.x + Hcrc.LandmarkOffset - state.pos.x, landmark.center.y - state.pos.y),
        RelativePoint2D(landmark.center.x - state.pos.x, landmark.center.y - Hcrc.LandmarkOffset - state.pos.y),
        RelativePoint2D(landmark.center.x - state.pos.x, landmark.center.y + Hcrc.LandmarkOffset - state.pos.y)
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
    val realPred = pred.map(_._1).sliding(2).flatMap { states =>
      val from = states(0)
      val to = states(1)
      val xStep = (to.pos.x - from.pos.x) / Hcrc.InterpSteps
      val yStep = (to.pos.y - from.pos.y) / Hcrc.InterpSteps
      (1 to 15).map { i =>
        val pos = Point2D(from.pos.x + i * xStep, from.pos.y + i * yStep)
        HcrcState(pos, from.map)
      }
                                                     }.toIndexedSeq
    val realGold = gold.map(_._1) :+ gold.last._3
    val goldTransitions = extractTransitions(realGold, gold.head._1.map.landmarks)
    val predTransitions = extractTransitions(realPred, pred.head._1.map.landmarks)
    val numCorrect = predTransitions.sliding(2).toSet.toIndexedSeq.map { pt: IndexedSeq[Landmark] =>
      val numPred = predTransitions.sliding(2).count(_ == pt)
      val numGold = goldTransitions.sliding(2).count(_ == pt)
      min(numPred, numGold)
    }.sum
    EvalStats(numCorrect, goldTransitions.length - numCorrect, predTransitions.length - numCorrect)
  }

  override def represent(s1: State, a: Action, s2: State): EventContext = {
    val event = Event(Set(StringFeature("toLandmark", s2.nearestLandmark.name),
                          StringFeature("fromLandmark", s1.nearestLandmark.name),
                         RealFeature("length", s1.pos.distanceTo(s2.pos))))
    val world = GraphWorld(Set())
    EventContext(event, world)
  }
}

