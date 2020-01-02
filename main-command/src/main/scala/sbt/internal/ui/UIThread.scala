package sbt.internal.ui

import java.io.File
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicBoolean

import sbt.BasicKeys.{ historyPath, newShellPrompt }
import sbt.State
import sbt.internal.util.complete.{ JLineCompletion, Parser }
import sbt.internal.util.{ ConsoleAppender, LineReader, ProgressEvent, Terminal }

import scala.annotation.tailrec

trait UIThread extends Thread with AutoCloseable { self: Thread =>
  private[sbt] def reader: UIThread.Reader
  private[sbt] def handleInput(s: Either[String, String]): Boolean
  private[this] val isStopped = new AtomicBoolean(false)
  private[sbt] def onProgressEvent(pe: ProgressEvent, terminal: Terminal): Unit
  self.setDaemon(true)
  self.start()
  override abstract def run(): Unit = {
    @tailrec def impl(): Unit = {
      val res = reader.readLine()
      if (!handleInput(res) && !isStopped.get) impl()
    }
    try impl()
    catch { case _: InterruptedException | _: ClosedChannelException => isStopped.set(true) }
  }
  override def close(): Unit = {
    isStopped.set(true)
    interrupt()
  }
}

object UIThread {
  trait Reader { def readLine(): Either[String, String] }
  object Reader {
    def terminalReader(prompt: String, parser: Parser[_])(
        terminal: Terminal,
        state: State
    ): Reader = {
      val lineReader = LineReader.createReader(history(state), terminal)
      JLineCompletion.installCustomCompletor(lineReader, parser)
      () => {
        def clear(): Unit = if (terminal.isAnsiSupported) {
          val dl = ConsoleAppender.DeleteLine
          terminal.printStream.print(
            dl + ConsoleAppender.clearScreen(0) + dl + ConsoleAppender.CursorLeft1000
          )
          terminal.printStream.flush()
        }
        clear()
        try {
          val res = lineReader.readLine(prompt)
          res match {
            case null      => Left("kill channel")
            case s: String => Right(s)
          }
        } catch {
          case _: InterruptedException => Right("")
        } finally lineReader.close()
      }
    }
    def terminalReader(
        terminal: Terminal,
        state: State,
    ): Reader = terminalReader(shellPrompt(terminal, state), parser(state))(terminal, state)
  }
  private[this] def history(s: State): Option[File] =
    s.get(historyPath).getOrElse(Some(new File(s.baseDir, ".history")))
  private[this] def parser(s: State): Parser[_] = s.combinedParser
  private[sbt] def shellPrompt(terminal: Terminal, s: State): String =
    s.get(newShellPrompt) match {
      case Some(pf) => pf(terminal, s)
      case None =>
        def ansi(s: String): String = if (terminal.isAnsiSupported) s"$s" else ""
        s"${ansi(ConsoleAppender.DeleteLine)}> ${ansi(ConsoleAppender.clearScreen(0))}"
    }
}
