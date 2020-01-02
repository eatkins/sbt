/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

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
