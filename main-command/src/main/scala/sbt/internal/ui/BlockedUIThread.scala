package sbt.internal.ui

import sbt.{ Exec, State }
import sbt.internal.util.{ ConsoleAppender, EscHelpers, ProgressEvent, ProgressState, Terminal }
import sbt.internal.util.complete.Parser
import sbt.internal.util.complete.Parser._

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
    onLine: String => Boolean,
    onMaintenance: String => Boolean,
) extends Thread(s"blocked-ui-thread-$name")
    with UIThread {
  private def differentSource(e: Exec): Boolean = e.source match {
    case None    => true
    case Some(s) => s.channelName != name
  }
  override private[sbt] lazy val reader: UIThread.Reader = {
    val remaining = (s.currentCommand.toList ::: s.remainingCommands).collect {
      case e if e.commandLine.nonEmpty && e.execId.isDefined && differentSource(e) =>
        (e.commandLine, e.execId.get)
    }
    val killParser = remaining match {
      case Seq() => Parser.failure("no commands")
      case Seq((cl, id), t @ _*) =>
        val kill = token("kill") ~> token(' ').+ ~> t.foldLeft(token(cl) ^^^ s"kill $id") {
          case (p, (c, i)) if c.nonEmpty => p | (token(c) ^^^ s"kill $i")
          case (p, _)                    => p
        }
        kill.map(Left(_))
    }
    val prefix =
      if (remaining.isEmpty) ""
      else {
        "sbt server is running other commands: " +
          remaining.map(_._1).mkString("\n", "\n", "\n") +
          s"stop running commands with the kill command, for example: `kill ${remaining.head._1}`."
      }
    val parser = killParser | matched(s.combinedParser).map(Right(_))
    val sp = UIThread.shellPrompt(terminal, s)
    val spLen = EscHelpers.removeEscapeSequences(sp).length
    val prompt = if (terminal.isAnsiSupported) {
      sp + "\n" + prefix + ConsoleAppender.cursorUp(terminal.lineCount(prefix)) +
        ConsoleAppender.CursorLeft1000 + ConsoleAppender.cursorRight(spLen)
    } else {
      prefix + "\n" + sp
    }
    val reader = UIThread.Reader.terminalReader(prompt, parser)(terminal, s)
    () =>
      reader.readLine match {
        case Right(cmd) =>
          Parser.parse(cmd, parser) match {
            case Right(e) => e
            case Left(_)  => Left("")
          }
        case l => l
      }
  }
  override private[sbt] def handleInput(s: Either[String, String]): Boolean = s match {
    case Left(c)  => onMaintenance(c)
    case Right(c) => onLine(c)
  }

  override private[sbt] def onProgressEvent(pe: ProgressEvent, terminal: Terminal): Unit = {
    ProgressState.updateProgressState(pe, terminal)
  }
}
