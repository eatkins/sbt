package sbt.internal.ui

import sbt.State
import sbt.internal.util.Terminal

private[sbt] class AskUserThread(
    name: String,
    state: State,
    terminal: Terminal,
    onLine: String => Boolean,
    onMaintenance: String => Boolean
) extends Thread(s"ask-user-thread-$name")
    with UIThread {
  override private[sbt] def reader: UIThread.Reader =
    UIThread.Reader.terminalReader(terminal, state)
  override private[sbt] def handleInput(s: Option[String]): Boolean = s.map(onLine).getOrElse {
    onMaintenance("kill channel")
  }
}
