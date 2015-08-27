package framework.igor.experiment

import java.io._

import framework.igor.logging.Logging
import scala.collection.mutable
import scala.io.Source
import scala.reflect.ClassTag

/**
 * @author jda
 */
class ResultCache(env: Environment) extends Logging {

  protected val resultCache = mutable.Map[Symbol,Any]()

  def put(name: Symbol, data: Any): Unit = {
    resultCache.put(name, data)
    putDisk(name, data)
  }

  def putDisk(name: Symbol, data: Any): Unit = {
    assert(env.workDir != null)
    val f = new File(env.workDir, name.name)
    if (f.exists())
      logger.warn(s"overwriting $name")
    val writer = new ObjectOutputStream(new FileOutputStream(f))
    try {
      writer.writeObject(data)
    } catch {
      case e:NotSerializableException =>
        logger.warn(s"Could not cache $name to disk", e)
    } finally {
      writer.close()
    }
  }

  def get[A](name: Symbol)(implicit ct: ClassTag[A]): A = {
    resultCache.getOrElseUpdate(name, getDisk(name)).asInstanceOf[A]
  }

  def getDisk[A](name: Symbol)(implicit ct: ClassTag[A]): A = {
    assert(env.workDir != null)
    val f = new File(env.workDir, name.name)
    assert(f.exists())
    val reader = new ObjectInputStream(new FileInputStream(f))
    try {
      reader.readObject.asInstanceOf[A]
    } finally {
      reader.close()
    }
  }

  def writeFile(name: Symbol, data: TraversableOnce[String]): Unit = {
    assert(env.workDir != null)
    val f = new File(env.workDir, name.name)
    if (f.exists())
      logger.warn(s"overwriting $name")
    val writer = new FileWriter(f)
    try {
      data.foreach(d => writer.write(d + "\n"))
    } finally {
      writer.close()
    }
  }

  def writeFile(name: Symbol, data: String): Unit = writeFile(name, Seq(data))

  // TODO(jda) clean up this interface
  def readFile(name: Symbol): Source = {
    assert(env.workDir != null)
    val f = new File(env.workDir, name.name)
    assert(f.exists())
    Source.fromFile(f)
  }

  def withFile[T](name: Symbol)(that: Source => T): T = {
    assert(env.workDir != null)
    val f = new File(env.workDir, name.name)
    assert(f.exists())
    val src = Source.fromFile(f)
    try {
      that(src)
    } finally {
      src.close()
    }
  }

}
