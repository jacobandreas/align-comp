package framework.igor.util

import java.io._

import framework.igor.logging.Logging

/**
 * @author jda
 */
case class DiskCache[T,U](name: String, base: T => U) extends (T => U) with Logging {

  val cacheDir = new File(s".cached_$name")
  if (!cacheDir.exists) {
    logger.info(s"new cache for ${base.getClass.getName}")
    cacheDir.mkdir()
  } else {
    logger.info(s"cache exists for ${base.getClass.getName}")
  }

  override def apply(arg: T): U = {
    val cacheFile = new File(cacheDir, s"${arg.##}.ser")
    if (cacheFile.exists) {
      val reader = new ObjectInputStream(new FileInputStream(cacheFile))
      try {
        val t = reader.readObject().asInstanceOf[T]
        assert { t == arg }
        val u = reader.readObject().asInstanceOf[U]
        u
      } finally {
        reader.close()
      }
    } else {
      val u = base(arg)
      val writer = new ObjectOutputStream(new FileOutputStream(cacheFile))
      try {
        writer.writeObject(arg)
        writer.writeObject(u)
      } finally {
        writer.close()
      }
      u
    }
  }
}
