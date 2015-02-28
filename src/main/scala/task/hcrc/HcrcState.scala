package task.hcrc

import task.TaskState

case class HcrcState(pos: Point2D, map: HcrcMap) extends TaskState {
  val nearestLandmark: Landmark = map.landmarks.minBy { lm => map.distanceCache(pos, lm.center)  }
  override def toString = s"HcrcState(${pos.x},${pos.y})"
}

