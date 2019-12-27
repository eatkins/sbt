/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import sbt.internal.util.{ MainAppender, ManagedLogger, Terminal }
import sbt.protocol.EventMessage
import sbt.util.{ Level, LogExchange }
import sjsonnew.JsonFormat

/**
 * A command channel represents an IO device such as network socket or human
 * that can issue command or listen for some outputs.
 * We can think of a command channel to be an abstraction of the terminal window.
 */
abstract class CommandChannel {
  private val commandQueue: ConcurrentLinkedQueue[Exec] = new ConcurrentLinkedQueue()
  private val registered: java.util.Set[java.util.Queue[CommandChannel]] = new java.util.HashSet
  private[sbt] final def register(queue: java.util.Queue[CommandChannel]): Unit =
    registered.synchronized {
      registered.add(queue)
      if (!commandQueue.isEmpty) queue.add(this)
      ()
    }
  private[sbt] final def unregister(queue: java.util.Queue[CommandChannel]): Unit =
    registered.synchronized {
      registered.remove(queue)
      ()
    }
  private[sbt] def terminal: Terminal
  def append(exec: Exec): Boolean = registered.synchronized {
    val res = commandQueue.add(exec)
    if (res) registered.forEach(q => q.synchronized { q.add(this); () })
    res
  }
  def poll: Option[Exec] = Option(commandQueue.poll)

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit
  final def publishEvent[A: JsonFormat](event: A): Unit = publishEvent(event, None)
  def publishEventMessage(event: EventMessage): Unit
  def publishBytes(bytes: Array[Byte]): Unit
  def shutdown(): Unit
  def name: String
  private[this] val level = new AtomicReference[Level.Value](Level.Info)
  private[this] val loggerHolder = new AtomicReference[Option[(ManagedLogger, Level.Value)]](None)
  private[sbt] final def setLevel(l: Level.Value): Unit = loggerHolder.get match {
    case Some((_, level)) if l == level =>
    case _ =>
      level.set(l)
      mkLogger
  }
  private[sbt] final def logLevel: Level.Value = level.get
  private[this] lazy val appender = MainAppender.defaultScreen(terminal)
  private[this] def mkLogger = loggerHolder.get match {
    case Some((l, level)) if level == logLevel => l
    case _ =>
      val log = LogExchange.logger(name, None, None)
      LogExchange.unbindLoggerAppenders(name)
      LogExchange.bindLoggerAppenders(name, List(appender -> Level.Info))
      loggerHolder.set(Some(log -> logLevel))
      log
  }
  private[sbt] final def logger: ManagedLogger = {
    loggerHolder.get match {
      case Some((l, _)) => l
      case None         => mkLogger
    }
  }
  private[sbt] def onLine: String => Unit = {
    case "error" => level.set(Level.Error); append(Exec("error", None, None))
    case "debug" => level.set(Level.Debug); append(Exec("debug", None, None))
    case "info"  => level.set(Level.Info); append(Exec("info", None, None))
    case "warn"  => level.set(Level.Warn); append(Exec("warn", None, None))
    case cmd =>
      append(Exec(cmd, Some(Exec.newExecId), Some(CommandSource(name)))); ()
  }
}

// case class Exec(commandLine: String, source: Option[CommandSource])

// case class CommandSource(channelName: String)

/*
 * This is a data passed specifically for local prompting console.
 */
case class ConsolePromptEvent(state: State) extends EventMessage

/*
 * This is a data passed specifically for unprompting local console.
 */
@deprecated("No longer used", "1.4.0")
case class ConsoleUnpromptEvent(lastSource: Option[CommandSource]) extends EventMessage

case class StartWatchEvent(state: State, index: Int) extends EventMessage
