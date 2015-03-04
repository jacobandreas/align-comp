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
  val hcrc = Hcrc(new File(DefaultConfig.dataDir))(DefaultConfig)

  "Hcrc" should "have as many instance IDs as instances" in {
    hcrc.trainIds.length + hcrc.testIds.length should equal (hcrc.instances.length)
  }

  it should "have no gaps in instance ids" in {
    (hcrc.trainIds.toSet ++ hcrc.testIds.toSet) should be ((0 until hcrc.instances.length).toSet)
  }

  it should "give gold maps a perfect score" in {
    val firstPath = hcrc.instances.head.path
    val score = hcrc.score(firstPath, firstPath)
    score.f1 should equal (1)
  }

}
