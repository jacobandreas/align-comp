package framework.igor.logging

import scala.util.DynamicVariable
import com.typesafe.scalalogging
import com.typesafe.scalalogging.slf4j
import org.slf4j.LoggerFactory

/**
 * @author jda
 */
trait Logging extends scalalogging.Logging {

  final val Spacer = ".  "

  protected lazy val baseLogger: slf4j.Logger = slf4j.Logger(LoggerFactory.getLogger(getClass.getName))
  val logger: scalalogging.Logger = alterMessages(baseLogger)(s => Spacer * Logging.taskDepth.value + s)

  def task[A](name: String)(body: => A): A = {
    logger.info(name)
    time {
      Logging.taskDepth.withValue(Logging.taskDepth.value + 1) {
        body
      }
    }
  }

  def time[A](a: => A): A = {
    val now = System.nanoTime
    val result = a
    val secs = 1f * (System.nanoTime - now) / 1000000000
    logger.info(s"done in ${secs}s")
    result
  }

  def alterMessages(logger: slf4j.Logger)(rewrite: => String => String): scalalogging.Logger = {
    new scalalogging.Logger {
      override def trace(message: String, args: AnyRef*): Unit = logger.trace(rewrite(message), args)
      override def trace(message: String, cause: Throwable): Unit = logger.trace(rewrite(message), cause)
      override def trace(message: String): Unit = logger.trace(rewrite(message))
      override def debug(message: String, args: AnyRef*): Unit = logger.debug(rewrite(message), args)
      override def debug(message: String, cause: Throwable): Unit = logger.debug(rewrite(message), cause)
      override def debug(message: String): Unit = logger.debug(rewrite(message))
      override def info(message: String, args: AnyRef*): Unit = logger.info(rewrite(message), args)
      override def info(message: String, cause: Throwable): Unit = logger.info(rewrite(message), cause)
      override def info(message: String): Unit = logger.info(rewrite(message))
      override def warn(message: String, args: AnyRef*): Unit = logger.warn(rewrite(message), args)
      override def warn(message: String, cause: Throwable): Unit = logger.warn(rewrite(message), cause)
      override def warn(message: String): Unit = logger.warn(rewrite(message))
      override def error(message: String, args: AnyRef*): Unit = logger.error(rewrite(message), args)
      override def error(message: String, cause: Throwable): Unit = logger.error(rewrite(message), cause)
      override def error(message: String): Unit = logger.info(rewrite(message))
    }
  }

}

object Logging {

  val taskDepth = new DynamicVariable[Int](0)

}
