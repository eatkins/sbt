/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util

import java.util.concurrent.atomic.AtomicReference

import sbt.util._
import org.apache.logging.log4j.{ Logger => XLogger }
import org.apache.logging.log4j.message.ObjectMessage
import sjsonnew.JsonFormat

import scala.reflect.runtime.universe.TypeTag
import sbt.internal.util.codec.JsonProtocol._

/**
 * Delegates log events to the associated LogExchange.
 */
class ManagedLogger(
    val name: String,
    getChannelName: () => Option[String],
    getExecID: () => Option[String],
    getXlogger: () => XLogger
) extends Logger {
  def this(name: String, channelName: Option[String], execID: Option[String], logger: XLogger) =
    this(name, () => channelName, () => execID, () => logger)
  def channelName: Option[String] = getChannelName()
  def execId: Option[String] = getExecID()
  override def trace(t: => Throwable): Unit =
    logEvent(Level.Error, TraceEvent("Error", t, channelName, execId))
  override def log(level: Level.Value, message: => String): Unit = {
    getXlogger().log(
      ConsoleAppender.toXLevel(level),
      new ObjectMessage(StringEvent(level.toString, message, channelName, execId))
    )
  }

  private lazy val SuccessEventTag = scala.reflect.runtime.universe.typeTag[SuccessEvent]
  // send special event for success since it's not a real log level
  override def success(message: => String): Unit = {
    infoEvent[SuccessEvent](SuccessEvent(message))(
      implicitly[JsonFormat[SuccessEvent]],
      SuccessEventTag
    )
  }

  def registerStringCodec[A: ShowLines: TypeTag]: Unit = {
    LogExchange.registerStringCodec[A]
  }

  final def debugEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Debug, event)
  final def infoEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Info, event)
  final def warnEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Warn, event)
  final def errorEvent[A: JsonFormat: TypeTag](event: => A): Unit = logEvent(Level.Error, event)
  def logEvent[A: JsonFormat: TypeTag](level: Level.Value, event: => A): Unit = {
    val v: A = event
    val tag = StringTypeTag[A]
    LogExchange.getOrElseUpdateJsonCodec(tag.key, implicitly[JsonFormat[A]])
    // println("logEvent " + tag.key)
    val entry: ObjectEvent[A] = ObjectEvent(level, v, channelName, execId, tag.key)
    getXlogger().log(
      ConsoleAppender.toXLevel(level),
      new ObjectMessage(entry)
    )
  }

  @deprecated("No longer used.", "1.0.0")
  override def ansiCodesSupported = ConsoleAppender.formatEnabledInEnv
}

private[sbt] object ManagedLogger {
  private[sbt] class ProxyLogger(
      name: String,
      channelName: AtomicReference[String],
      execID: AtomicReference[String],
      logger: AtomicReference[XLogger]
  ) extends ManagedLogger(
        name,
        () => Option(channelName.get),
        () => Option(execID.get),
        () => logger.get
      ) {
    def this(name: String) =
      this(
        name,
        new AtomicReference[String],
        new AtomicReference[String],
        new AtomicReference(LogExchange.getXLogger(name))
      )
    def setChannelName(name: String): Unit = channelName.set(name)
    def setExecID(id: String): Unit = execID.set(name)
    def setLogger(xl: XLogger): Unit = logger.set(xl)
    def withState[T](cn: String, eid: String, xlog: XLogger)(f: => T): T = {
      val (c, e, l) = (channelName.get, execID.get, logger.get)
      try {
        channelName.set(cn)
        execID.set(eid)
        logger.set(xlog)
        f
      } finally {
        channelName.set(c)
        execID.set(e)
        logger.set(l)
      }
    }
  }
}
