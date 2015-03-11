package task.hcrc

import java.io.File

import framework.igor.util.Memoize

import scala.xml.XML

/**
 * @author jda
 */

case class HcrcMap(path: IndexedSeq[Point2D], landmarks: IndexedSeq[Landmark]) {
  val start = path.head
  val startLandmark = landmarks.find(_.name == "start").get
  val distanceCache = Memoize[(Point2D,Point2D),Double] { case (a,b) => a.distanceTo(b) }
}

object HcrcMap {
  def fromLandmarksFile(file: File): HcrcMap = {
    val doc = XML.loadFile(file)
    val path = (doc \ "path" \ "stop").map { node => Point2D(node.attribute("x").head.text.toDouble,
      node.attribute("y").head.text.toDouble) }.toIndexedSeq
    val landmarks = (doc \ "landmark").map { lm =>
      val spot = (lm \ "spot").head
      val name = lm.attribute("name").head.text.drop(3).replace("_", " ").trim
      val center = Point2D(spot.attribute("x").head.text.toDouble, spot.attribute("y").head.text.toDouble)
      val radius = spot.attribute("r").head.text.toDouble
      Landmark(center, radius, name)
    }.filterNot(_.name == "finish").toIndexedSeq

    HcrcMap(path, landmarks)
  }
}

case class Landmark(center: Point2D, radius: Double, name: String)

