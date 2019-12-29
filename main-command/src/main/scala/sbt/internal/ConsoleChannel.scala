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
import java.util.concurrent.atomic.AtomicReference

import sbt.BasicKeys._
import sbt.internal.util._
import sbt.protocol.EventMessage
import sjsonnew.JsonFormat

private[sbt] class AskUserThread(
    name: String,
    s: State,
    terminal: Terminal,
    onLine: String => Unit,
    onClose: () => Unit
) extends Thread(s"ask-user-thread-$name") {
  private[this] val writer = new PrintStream(terminal.outputStream, true)
  private[this] def getPrompt(s: State): String = s.get(newShellPrompt) match {
    case Some(pf) => pf(terminal, s)
    case None =>
      def ansi(s: String): String = if (terminal.isAnsiSupported) s"$s" else ""
      s"${ansi(ConsoleAppender.DeleteLine)}> ${ansi(ConsoleAppender.ClearScreenAfterCursor)}"
  }
  private val history = s.get(historyPath).getOrElse(Some(new File(s.baseDir, ".history")))
  private val prompt = getPrompt(s)
  private val reader = new FullReader(history, s.combinedParser, LineReader.HandleCONT, terminal)
  setDaemon(true)
  start()
  override def run(): Unit =
    try {
      if (terminal.isAnsiSupported) {
        terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
        terminal.printStream.flush()
      }
      if (terminal.getLineHeightAndWidth._2 > 0) terminal.printStream.println()
      terminal.printStream.print(ConsoleAppender.DeleteLine + ConsoleAppender.clearScreen(0))
      terminal.printStream.flush()
      terminal.withRawSystemIn(reader.readLine(prompt)) match {
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
private[sbt] final class ConsoleChannel(val name: String) extends CommandChannel {
  private[this] val askUserThread = new AtomicReference[AskUserThread]
  override private[sbt] def terminal = Terminal.console
  private[this] def makeAskUserThread(s: State): AskUserThread =
    new AskUserThread(
      "console",
      s,
      Terminal.console,
      onLine,
      () => askUserThread.synchronized(askUserThread.set(null))
    )

  def run(s: State): State = s

  def publishBytes(bytes: Array[Byte]): Unit = ()

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()

  def publishEventMessage(event: EventMessage): Unit =
    event match {
      case e: ConsolePromptEvent =>
        if (Terminal.systemInIsAttached) {
          askUserThread.synchronized {
            askUserThread.get match {
              case null => askUserThread.set(makeAskUserThread(e.state))
              case t    => t.redraw()
            }
          }
        }
      case _ => //
    }

  def shutdown(): Unit = askUserThread.synchronized {
    askUserThread.get match {
      case null =>
      case t if t.isAlive =>
        t.interrupt()
        askUserThread.set(null)
      case _ => ()
    }
  }
}
