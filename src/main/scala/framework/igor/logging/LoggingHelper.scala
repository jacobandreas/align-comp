package igor.logging

import org.apache.logging.log4j
import org.apache.logging.log4j.core.appender.ConsoleAppender
import org.apache.logging.log4j.core.layout.PatternLayout

/**
 * @author jda
 */
object LoggingHelper {

  final val ConsoleLogName = "Console"

  object Level extends Enumeration {
    type Level = Value
    val All, Off, Debug, Info, Warn, Error = Value
  }

  def setLog4JLevel(l: LoggingHelper.Level.Level): Unit = {
    val loggerContext = log4j.LogManager.getContext(false).asInstanceOf[log4j.core.LoggerContext]
    val loggerConfig = loggerContext.getConfiguration
    val rootLoggerConfig = loggerConfig.getLoggerConfig(log4j.LogManager.ROOT_LOGGER_NAME)
    rootLoggerConfig.setLevel(igorLogLevelToLog4JLogLevel(l))
    loggerContext.updateLoggers()
  }

  def setLog4JLayout(layout: String): Unit = {
    val loggerContext = log4j.LogManager.getContext(false).asInstanceOf[log4j.core.LoggerContext]
    val loggerConfig = loggerContext.getConfiguration
    val rootLoggerConfig = loggerConfig.getLoggerConfig(log4j.LogManager.ROOT_LOGGER_NAME)
    val oldAppender = rootLoggerConfig.getAppenders.get(ConsoleLogName)
    //println(rootLoggerConfig.getAppenders.get(ConsoleLogName).asInstanceOf[ConsoleAppender].
  }

  def igorLogLevelToLog4JLogLevel(l: LoggingHelper.Level.Level): log4j.Level = l match {
    case Level.All => log4j.Level.ALL
    case Level.Off => log4j.Level.OFF
    case Level.Debug => log4j.Level.DEBUG
    case Level.Info => log4j.Level.INFO
    case Level.Warn => log4j.Level.WARN
    case Level.Error => log4j.Level.ERROR
  }

  implicit def stringToIgorLogLevel(l: String): LoggingHelper.Level.Level = l.toLowerCase match {
    case "all" => Level.All
    case "off" => Level.Off
    case "debug" => Level.Debug
    case "info" => Level.Info
    case "warn" => Level.Warn
    case "error" => Level.Error
  }

}
