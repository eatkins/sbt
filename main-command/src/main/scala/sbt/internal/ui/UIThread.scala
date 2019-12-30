package sbt.internal.ui

import java.io.File
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicBoolean

import sbt.BasicKeys.{ historyPath, newShellPrompt }
import sbt.State
import sbt.internal.util.complete.{ JLineCompletion, Parser }
import sbt.internal.util.{ ConsoleAppender, LineReader, Terminal }

import scala.annotation.tailrec

trait UIThread extends AutoCloseable { self: Thread =>
  private[sbt] def reader: UIThread.Reader
  private[sbt] def handleInput(s: Option[String]): Boolean
  private[this] val isStopped = new AtomicBoolean(false)
  self.setDaemon(true)
  self.start()
  override abstract def run(): Unit = {
    @tailrec def impl(): Unit = if (!handleInput(reader.readLine()) && !isStopped.get) impl()
    try impl()
    catch { case _: InterruptedException | _: ClosedChannelException => isStopped.set(true) }
  }
  override def close(): Unit = {
    isStopped.set(true)
    interrupt()
  }
}

object UIThread {
  trait Reader { def readLine(): Option[String] }
  object Reader {
    def terminalReader(prompt: (Terminal, State) => String, parser: State => Parser[_])(
        terminal: Terminal,
        state: State
    ): Reader = {
      val lineReader = LineReader.createReader(history(state), terminal)
      JLineCompletion.installCustomCompletor(lineReader, parser(state))
      () => {
        if (terminal.isAnsiSupported) {
          val dl = ConsoleAppender.DeleteLine
          terminal.printStream.print(dl + ConsoleAppender.clearScreen(0) + dl)
          terminal.printStream.flush()
        }
        if (terminal.getLineHeightAndWidth._2 > 0) terminal.printStream.println()
        try terminal.withRawSystemIn(lineReader.readLine(prompt(terminal, state))) match {
          case null      => None
          case s: String => Some(s)
        } catch {
          case _: InterruptedException => None
        } finally lineReader.close()
      }
    }
    def terminalReader(
        terminal: Terminal,
        state: State,
    ): Reader = terminalReader(shellPrompt _, parser _)(terminal, state)
  }
  private[this] def history(s: State): Option[File] =
    s.get(historyPath).getOrElse(Some(new File(s.baseDir, ".history")))
  private[this] def parser(s: State): Parser[_] = s.combinedParser
  private[this] def shellPrompt(terminal: Terminal, s: State): String =
    s.get(newShellPrompt) match {
      case Some(pf) => pf(terminal, s)
      case None =>
        def ansi(s: String): String = if (terminal.isAnsiSupported) s"$s" else ""
        s"${ansi(ConsoleAppender.DeleteLine)}> ${ansi(ConsoleAppender.clearScreen(0))}"
    }
}
