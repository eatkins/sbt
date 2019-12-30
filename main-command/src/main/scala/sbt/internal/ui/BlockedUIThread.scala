package sbt.internal.ui

import sbt.State
import sbt.internal.util.Terminal

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
    options: Map[Char, BlockedUIThread.CommandOption],
    onMaintenance: String => Boolean,
) extends Thread(s"blocked-ui-thread-$name")
    with UIThread {
  override private[sbt] def reader: UIThread.Reader = () => None
  override private[sbt] def handleInput(s: Option[String]): Boolean = true
}
