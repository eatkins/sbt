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
      getPrompt: String,
      progressState: ProgressState,
      terminal: Terminal,
      printStream: PrintStream
  ): String = prompt match {
    case AskUser(_) =>
      printStream.print(getPrompt)
      printStream.flush()
      if (progressState.progressLines.get.nonEmpty) progressState.reprint(printStream)
      getPrompt
    case _ => ""
  }
}
