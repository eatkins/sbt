/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.io.{ File, IOException, PrintStream }
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }

import sbt.BasicKeys._
import sbt.internal.util._
import sbt.protocol.EventMessage
import sjsonnew.JsonFormat

import scala.annotation.tailrec

private[sbt] trait UserThread extends Thread with AutoCloseable {
  def read(): Unit
  def redraw(): Unit
}

private[sbt] object UserThread {
  sealed trait UIState
  case object Ready extends UIState
  final class Blocked(val remaining: Seq[Exec]) extends UIState {
    override def toString: String = s"Blocked(${remaining mkString ","})"
  }
  object Blocked {
    def unapply(b: Blocked): Option[Seq[Exec]] = Some(b.remaining)
  }
}

private[sbt] class AskUserThread(
    name: String,
    s: State,
    terminal: Terminal,
    onLine: String => Unit
) extends Thread(s"ask-user-thread-$name")
    with UserThread {
  private[this] val writer = new PrintStream(terminal.outputStream, true)
  private[this] val prompt = s.get(newShellPrompt) match {
    case Some(pf) => pf(terminal, s)
    case None =>
      def ansi(s: String): String = if (terminal.isAnsiSupported) s"$s" else ""
      s"${ansi(ConsoleAppender.DeleteLine)}> ${ansi(ConsoleAppender.ClearScreenAfterCursor)}"
  }
  private[this] val history = s.get(historyPath).getOrElse(Some(new File(s.baseDir, ".history")))
  private[this] val reader =
    new FullReader(history, s.combinedParser, LineReader.HandleCONT, terminal)
  setDaemon(true)
  start()

  override def read(): Unit = {
    if (terminal.isAnsiSupported) {
      terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
      terminal.printStream.flush()
    }
    if (terminal.getLineHeightAndWidth._2 > 0) terminal.printStream.println()
    terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
    terminal.printStream.flush()
    terminal.withRawSystemIn {
      val line =
        try reader.readLine(prompt)
        catch { case _: ClosedChannelException | _: InterruptedException => Some("") }
      line match {
        case Some(cmd) => onLine(cmd)
        case None      =>
      }
    }
  }
  override def run(): Unit =
    try read()
    catch { case _: ClosedChannelException | _: InterruptedException | _: IOException => }
  def redraw(): Unit = {
    writer.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
    reader.redraw()
    writer.print(ConsoleAppender.clearScreen(0))
    terminal.outputStream.flush()
  }

  override def close(): Unit = interrupt()
}

private[sbt] object BlockedUIThread {
  private[sbt] sealed class CommandOption
  private[sbt] class More(val next: Map[Char, CommandOption]) extends CommandOption
  private[sbt] object More {
    def apply(o: Map[Char, CommandOption]): More = new More(o)
    def unapply(o: CommandOption): Option[Map[Char, CommandOption]] = o match {
      case m: More => Some(m.next)
      case _       => None
    }
  }
  private[sbt] class Action(val action: () => String, val show: String) extends CommandOption
  private[sbt] object Action {
    def apply(action: () => String, show: String): Action = new Action(action, show)
    def unapply(o: CommandOption): Option[() => String] = o match {
      case a: Action => Some(a.action)
      case _         => None
    }
  }
}
private[sbt] class BlockedUIThread(
    name: String,
    s: State,
    terminal: Terminal,
    options: Map[Char, BlockedUIThread.CommandOption],
    onMaintenance: String => Unit,
) extends Thread(s"blocked-ui-thread-$name")
    with UserThread {
  private[this] val isStopped = new AtomicBoolean(false)
  setDaemon(true)
  start()

  override def close(): Unit = {
    isStopped.set(true)
    interrupt()
  }

  override def run(): Unit =
    try read()
    catch {
      case _: ClosedChannelException | _: InterruptedException | _: IOException =>
        isStopped.set(true)
    }
  import BlockedUIThread._
  override def read(): Unit = if (!isStopped.get) {
    if (terminal.isAnsiSupported) {
      terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
      terminal.printStream.flush()
    }
    if (terminal.getLineHeightAndWidth._2 > 0) terminal.printStream.println()
    terminal.withRawSystemIn {
      @tailrec
      def impl(opts: Map[Char, BlockedUIThread.CommandOption]): Unit = if (!isStopped.get) {
        val res = terminal.inputStream.read
        res match {
          case -1 =>
          case k =>
            val c = k.toByte.toChar
            opts.get(c) match {
              case None             => impl(opts)
              case Some(More(next)) => impl(next)
              case Some(Action(action)) =>
                val a = action()
                onMaintenance(a)
            }
        }
      }
      try impl(options)
      catch { case _: InterruptedException => isStopped.set(true) }
    }
  }
  def redraw(): Unit = {
    terminal.outputStream.flush()
  }
}

private[sbt] trait HasUserThread {
  private[this] val askUserThread = new AtomicReference[UserThread]
  def name: String
  private[sbt] def terminal: Terminal
  private[sbt] def onLine: String => Unit
  private[sbt] def onMaintenance: String => Unit

  private[sbt] def makeAskUserThread(s: State, uiState: UserThread.UIState): UserThread =
    uiState match {
      case UserThread.Ready =>
        new AskUserThread(
          name,
          s,
          terminal,
          onLine
        )
      case UserThread.Blocked(remaining) =>
        val opts = remaining.flatMap { e: Exec =>
          e.execId.map(i => (e.commandLine, i))
        } match {
          case Seq() => Map.empty[Char, BlockedUIThread.CommandOption]
          case Seq((cmd, id)) if cmd.trim.nonEmpty =>
            Map('k' -> BlockedUIThread.Action(() => s"kill $id", s"kill $cmd $id"))
          case cmds =>
            val rest = cmds.zipWithIndex.collect {
              case ((cmd, id), index) if cmd.nonEmpty =>
                index.toString.head -> BlockedUIThread.Action(() => s"kill $id", s"kill $cmd $id")
            }.toMap
            Map('k' -> BlockedUIThread.More(rest))
        }
        new BlockedUIThread(
          name,
          s,
          terminal,
          opts,
          onMaintenance
        )
    }

  private[sbt] def reset(state: State, uiState: UserThread.UIState): Unit = {
    askUserThread.synchronized {
      askUserThread.getAndSet(null) match {
        case null =>
        case t =>
          t.close()
          t.join()
      }
      askUserThread.set(makeAskUserThread(state, uiState))
    }
  }

  private[sbt] def stopThread(): Unit = askUserThread.synchronized {
    askUserThread.get match {
      case null =>
      case t if t.isAlive =>
        t.interrupt()
        askUserThread.set(null)
      case _ => ()
    }
  }
}
private[sbt] final class ConsoleChannel(val name: String) extends CommandChannel {
  override private[sbt] def terminal = Terminal.console

  def run(s: State): State = s

  def publishBytes(bytes: Array[Byte]): Unit = ()

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()

  def publishEventMessage(event: EventMessage): Unit =
    event match {
      case e: ConsolePromptEvent => reset(e.state, UserThread.Ready)
      case _                     => //
    }
}
