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
    def wrappedOutputStream(os: OutputStream): OutputStream = new OutputStream {
      override def write(b: Int): Unit = {
        if (b == 10) {
          bytes.clear()
        } else bytes.put(b)
        os.write(b)
      }
      override def flush(): Unit = os.flush()
    }

    override def render(): String = {
      s"${ConsoleAppender.DeleteLine}${new String(bytes.asScala.toArray.map(_.toByte))}"
    }
  }
  private[sbt] case object Running extends Prompt {
    override val mkPrompt: () => String = () => ""
    override def render(): String = ""
  }
  private[sbt] def render(
      line: String,
      prompt: Prompt,
      progressState: ProgressState,
      terminal: Terminal,
      printStream: PrintStream
  ): String = prompt match {
    case AskUser(_) =>
      val res = prompt.render()
      if (res != line) {
        printStream.print(ConsoleAppender.DeleteLine + res)
        printStream.flush()
        if (progressState.progressLines.get.nonEmpty) progressState.reprint(printStream)
      }
      res
    case _ =>
      if (progressState.progressLines.get.nonEmpty) progressState.reprint(printStream)
      ""
  }
}
