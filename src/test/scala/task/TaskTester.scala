package task

import main.{HcrcConfig, Config}
import org.scalatest._

import java.io.File

import task.hcrc.Hcrc


/**
 * @author jda
 */
class TaskTester extends FlatSpec with Matchers {

  implicit final val DefaultConfig = Config()
  val task = DefaultConfig.task(new File(DefaultConfig.dataDir))

  "Task" should "have as many instance IDs as instances" in {
    task.trainIds.length + task.testIds.length should equal (task.instances.length)
  }

  it should "have no gaps in instance ids" in {
    (task.trainIds.toSet ++ task.testIds.toSet) should be ((0 until task.instances.length).toSet)
  }

  it should "have no overlap between train and test ids" in {
    (task.trainIds.toSet intersect task.testIds.toSet) should be (Set())
  }

  it should "give gold maps a perfect score" in {
    val firstPath = task.instances.head.path
    val score = task.score(firstPath, firstPath)
    score.f1 should equal (1)
  }

}
