package sbt.internal.util

import java.io.PrintStream

private[sbt] sealed trait Prompt {
  def mkPrompt: () => String
}

private[sbt] object Prompt {
  private[sbt] case class AskUser(override val mkPrompt: () => String) extends Prompt
  private[sbt] case object Running extends Prompt {
    override val mkPrompt: () => String = () => ""
  }
  private[sbt] def render(
      prompt: Prompt,
      progressState: ProgressState,
      terminal: Terminal,
      printStream: PrintStream
  ): String = {
    prompt match {
      case null =>
        printStream.println()
        ""
      case AskUser(mkPrompt) =>
        val p = mkPrompt()
        //System.err.println(s"WTF $mkPrompt")
        terminal.write(p.getBytes.map(_ & 0xFF): _*)
        terminal.withPrintStream(progressState.reprint)
        p
      case Running =>
        terminal.withPrintStream(progressState.reprint)
        ""
    }
  }
}
