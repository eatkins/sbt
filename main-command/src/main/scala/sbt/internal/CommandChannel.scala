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

import sbt.internal.ui.{ HasUserThread, UIThread, UserThread }
import sbt.internal.util.Prompt.AskUser
import sbt.internal.util.Terminal
import sbt.protocol.EventMessage
import sbt.util.Level
import sjsonnew.JsonFormat

import scala.collection.JavaConverters._

/**
 * A command channel represents an IO device such as network socket or human
 * that can issue command or listen for some outputs.
 * We can think of a command channel to be an abstraction of the terminal window.
 */
abstract class CommandChannel extends HasUserThread {
  private val commandQueue: ConcurrentLinkedQueue[Exec] = new ConcurrentLinkedQueue()
  private val registered: java.util.Set[java.util.Queue[Exec]] = new java.util.HashSet
  private val maintenance: java.util.Set[java.util.Queue[MaintenanceTask]] = new java.util.HashSet
  private[sbt] final def register(
      queue: java.util.Queue[Exec],
      maintenanceQueue: java.util.Queue[MaintenanceTask]
  ): Unit =
    registered.synchronized {
      registered.add(queue)
      if (!commandQueue.isEmpty) {
        queue.addAll(commandQueue)
        commandQueue.clear()
      }
      maintenance.add(maintenanceQueue)
      ()
    }
  private[sbt] final def unregister(
      queue: java.util.Queue[CommandChannel],
      maintenanceQueue: java.util.Queue[MaintenanceTask]
  ): Unit =
    registered.synchronized {
      registered.remove(queue)
      maintenance.remove(maintenanceQueue)
      ()
    }
  private[sbt] final def initiateMaintenance(task: String): Unit = {
    maintenance.forEach(q => q.synchronized { q.add(new MaintenanceTask(this, task)); () })
  }
  private[sbt] def mkUIThread: (State, CommandChannel) => UIThread
  private[sbt] def terminal: Terminal = Terminal.get
  final def append(exec: Exec): Boolean = registered.synchronized {
    exec.commandLine.nonEmpty && {
      if (registered.isEmpty) commandQueue.add(exec)
      else registered.asScala.forall(_.add(exec))
    }
  }
  def poll: Option[Exec] = Option(commandQueue.poll)

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit
  final def publishEvent[A: JsonFormat](event: A): Unit = publishEvent(event, None)
  def publishEventMessage(event: EventMessage): Unit = event match {
    case cpe: ConsolePromptEvent   => onConsolePromptEvent(cpe)
    case cue: ConsoleUnpromptEvent => onConsoleUnpromptEvent(cue)
    case _                         =>
  }
  def publishBytes(bytes: Array[Byte]): Unit
  def shutdown(logShutdown: Boolean): Unit = stopThread()
  @deprecated("Use the variant that takes the logShutdown parameter", "1.4.0")
  def shutdown(): Unit = shutdown(true)
  def name: String
  private[this] val level = new AtomicReference[Level.Value](Level.Info)
  private[sbt] final def setLevel(l: Level.Value): Unit = level.set(l)
  private[sbt] final def logLevel: Level.Value = level.get
  private[this] def setLevel(value: Level.Value, cmd: String): Boolean = {
    level.set(value)
    append(Exec(cmd, Some(Exec.newExecId), Some(CommandSource(name))))
  }
  private[sbt] def onCommand: String => Boolean = {
    case "error" => setLevel(Level.Error, "error")
    case "debug" => setLevel(Level.Debug, "debug")
    case "info"  => setLevel(Level.Info, "info")
    case "warn"  => setLevel(Level.Warn, "warn")
    case cmd =>
      if (cmd.nonEmpty) append(Exec(cmd, Some(Exec.newExecId), Some(CommandSource(name))))
      else false
  }
  private[sbt] def onMaintenance: String => Boolean = { s: String =>
    maintenance.synchronized(maintenance.forEach { q =>
      q.add(new MaintenanceTask(this, s))
      ()
    })
    true
  }

  override private[sbt] def onConsolePromptEvent(consolePromptEvent: ConsolePromptEvent): Unit = {
    val state = consolePromptEvent.state
    terminal.prompt match {
      case _: AskUser =>
      case _          => terminal.setPrompt(AskUser(() => UIThread.shellPrompt(terminal, state)))
    }
    reset(state, UserThread.Ready)
  }

  override private[sbt] def onConsoleUnpromptEvent(
      consoleUnpromptEvent: ConsoleUnpromptEvent
  ): Unit = {
    if (consoleUnpromptEvent.lastSource.fold(true)(_.channelName != name)) {
      terminal.progressState.reset()
    } else stopThread()
  }

  override private[sbt] def makeUIThread(s: State) = mkUIThread(s, this)
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
case class ConsoleUnpromptEvent(lastSource: Option[CommandSource], state: State)
    extends EventMessage

case class StartWatchEvent(state: State, index: Int) extends EventMessage

private[internal] class MaintenanceTask(val channel: CommandChannel, val task: String)
