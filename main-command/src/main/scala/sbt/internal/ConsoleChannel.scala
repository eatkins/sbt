/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.io.{ File, IOException, InputStream, PrintStream }
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicReference

import sbt.BasicKeys._
import sbt.internal.util._
import sbt.protocol.EventMessage
import sjsonnew.JsonFormat

private[sbt] trait UserThread extends Thread {
  def read(): Option[String]
  def redraw(): Unit
}

private[sbt] class AskUserThread(
    name: String,
    s: State,
    terminal: Terminal,
    onLine: String => Unit,
    onClose: () => Unit
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

  override def read(): Option[String] = {
    if (terminal.isAnsiSupported) {
      terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
      terminal.printStream.flush()
    }
    if (terminal.getLineHeightAndWidth._2 > 0) terminal.printStream.println()
    terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
    terminal.printStream.flush()
    terminal.withRawSystemIn(reader.readLine(prompt))
  }
  override def run(): Unit =
    try {
      read() match {
        case Some(cmd) => onLine(cmd)
        case None =>
          writer.println("") // Prevents server shutdown log lines from appearing on the prompt line
          onLine("exit")
      }
    } catch { case _: ClosedChannelException | _: InterruptedException | _: IOException => } finally onClose()
  def redraw(): Unit = {
    writer.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
    reader.redraw()
    writer.print(ConsoleAppender.clearScreen(0))
    terminal.outputStream.flush()
  }
}
private[sbt] trait HasUserThread {
  private[this] val askUserThread = new AtomicReference[UserThread]
  private[sbt] def terminal: Terminal
  private[sbt] def onLine: String => Unit

  private[sbt] def makeAskUserThread(s: State): AskUserThread =
    new AskUserThread(
      "console",
      s,
      terminal,
      onLine,
      () => askUserThread.synchronized(askUserThread.set(null))
    )

  private[sbt] def reset(state: State): Unit = {
    if (Terminal.systemInIsAttached) {
      askUserThread.synchronized {
        askUserThread.get match {
          case null => askUserThread.set(makeAskUserThread(state))
          case t    => t.redraw()
        }
      }
    }
  }

  private[sbt] def update[T](
      lastSource: Option[CommandSource],
      name: T => String,
      state: State,
      t: T
  ): Unit = {
    if (!lastSource.map(_.channelName).contains(name(t))) {
      askUserThread.getAndSet(null) match {
        case null =>
        case t =>
          t.interrupt()
          askUserThread.set(null)
      }
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
      case e: ConsolePromptEvent => reset(e.state)
      case _                     => //
    }
}
