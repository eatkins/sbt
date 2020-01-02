/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util

import java.io.{ OutputStream, PrintStream }
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.JavaConverters._

private[sbt] sealed trait Prompt {
  def mkPrompt: () => String
  def render(): String
}

private[sbt] object Prompt {
  private[sbt] case class AskUser(override val mkPrompt: () => String) extends Prompt {
    private[this] val bytes = new LinkedBlockingQueue[Int]
    def wrappedOutputStream(os: OutputStream): OutputStream = b => {
      bytes.put(b)
      os.write(b)
    }

    override def render(): String = {
      s"${mkPrompt()}${new String(bytes.asScala.toArray.map(_.toByte))}"
    }
  }
  private[sbt] case object Running extends Prompt {
    override val mkPrompt: () => String = () => ""
    override def render(): String = ""
  }
  private[sbt] def render(
      prompt: Prompt,
      progressState: ProgressState,
      terminal: Terminal,
      printStream: PrintStream
  ): String = prompt match {
    case AskUser(_) =>
      val res = prompt.render()
      printStream.print(res)
      printStream.flush()
      if (progressState.progressLines.get.nonEmpty) progressState.reprint(printStream)
      res
    case _ =>
      if (progressState.progressLines.get.nonEmpty) progressState.reprint(printStream)
      ""
  }
}
