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
  private[sbt] final def setLevel(l: Level.Value): Unit = level.set(l)
  private[sbt] final def logLevel: Level.Value = level.get
  private[sbt] final def logger: ManagedLogger = {
    val log = LogExchange.logger(name, None, None)
    LogExchange.unbindLoggerAppenders(name)
    val appender = MainAppender.defaultScreen(terminal)
    LogExchange.bindLoggerAppenders(name, List(appender -> logLevel))
    log
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
