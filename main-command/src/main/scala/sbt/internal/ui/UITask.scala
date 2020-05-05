/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.ui

import java.io.File
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicBoolean

import jline.console.history.PersistentHistory
import sbt.BasicKeys.{ historyPath, newShellPrompt }
import sbt.State
import sbt.internal.CommandChannel
import sbt.internal.util.ConsoleAppender.{ ClearScreenAfterCursor, CursorLeft1000, DeleteLine }
import sbt.internal.util._
import sbt.internal.util.complete.{ JLineCompletion, Parser }

import scala.annotation.tailrec

trait UITask extends Runnable with AutoCloseable {
  private[sbt] def channel: CommandChannel
  private[sbt] def reader: UITask.Reader
  private[this] final def handleInput(s: Either[String, String]): Boolean = s match {
    case Left(m)    => channel.onMaintenance(m)
    case Right(cmd) => channel.onCommand(cmd)
  }
  private[this] val isStopped = new AtomicBoolean(false)
  private[sbt] def onProgressEvent(pe: ProgressEvent, terminal: Terminal): Unit
  override def run(): Unit = {
    @tailrec def impl(): Unit = {
      val res = reader.readLine()
      if (!handleInput(res) && !isStopped.get) impl()
    }
    try impl()
    catch { case _: InterruptedException | _: ClosedChannelException => isStopped.set(true) }
  }
  override def close(): Unit = isStopped.set(true)
}

object UITask {
  trait Reader { def readLine(): Either[String, String] }
  object Reader {
    def terminalReader(prompt: Prompt, parser: Parser[_])(
        terminal: Terminal,
        state: State
    ): Reader = {
      val clearLeft = terminal.ansi(CursorLeft1000, "")
      val clearScreen = terminal.ansi(ClearScreenAfterCursor, "")
      val lineReader = LineReader.createReader(history(state), terminal, prompt)
      JLineCompletion.installCustomCompletor(lineReader, parser)
      () => {
        val clear = clearLeft + clearScreen
        try {
          terminal.setPrompt(prompt)
          val p = prompt.mkPrompt()
          @tailrec def impl(): Either[String, String] = {
            lineReader.readLine(clear + p) match {
              case null => Left("kill channel")
              case s: String =>
                lineReader.getHistory match {
                  case p: PersistentHistory =>
                    p.add(s)
                    p.flush()
                  case _ =>
                }
                s match {
                  case cmd if cmd.startsWith("kill ") => Left(cmd)
                  case ""                             => impl()
                  case cmd @ ("shutdown" | "exit")    => Left(cmd)
                  case cmd =>
                    terminal.setPrompt(Prompt.Running)
                    terminal.printStream.write(Int.MinValue)
                    Right(cmd)
                }
            }
          }
          impl()
        } catch {
          case _: InterruptedException => Right("")
        } finally lineReader.close()
      }
    }
  }
  private[this] def history(s: State): Option[File] =
    s.get(historyPath).getOrElse(Some(new File(s.baseDir, ".history")))
  private[sbt] def shellPrompt(terminal: Terminal, s: State): String =
    s.get(newShellPrompt) match {
      case Some(pf) => pf(terminal, s)
      case None =>
        def ansi(s: String): String = if (terminal.isAnsiSupported) s"$s" else ""
        s"${ansi(DeleteLine)}> ${ansi(ClearScreenAfterCursor)}"
    }
}
