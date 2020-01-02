package sbt.internal.util

private[sbt] sealed trait Prompt

private[sbt] object Prompt {
  private[sbt] case object AskUser extends Prompt
  private[sbt] case object Running extends Prompt
  private[sbt] def render(
      prompt: Prompt,
      progressState: ProgressState,
      terminal: Terminal
  ): Unit = {
    prompt match {
      case AskUser =>
        terminal.withPrintStream(progressState.reprint)
      case Running =>
        terminal.withPrintStream(progressState.reprint)
    }
  }
}
