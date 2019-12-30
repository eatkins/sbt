package sbt.internal.ui

import java.util.concurrent.atomic.AtomicReference

import sbt.{ Exec, State }
import sbt.internal.util.Terminal

private[sbt] object UserThread {
  sealed trait UIState
  case object Ready extends UIState
  final class Blocked(val remaining: Seq[Exec]) extends UIState {
    override def toString: String = s"Blocked(${remaining mkString ","})"
  }
  object Blocked {
    def unapply(b: Blocked): Option[Seq[Exec]] = Some(b.remaining)
  }
}

private[sbt] trait HasUserThread {
  private[this] val askUserThread = new AtomicReference[UIThread]
  def name: String
  private[sbt] def terminal: Terminal
  private[sbt] def onLine: String => Boolean
  private[sbt] def onMaintenance: String => Boolean

  private[sbt] def makeAskUserThread(s: State, uiState: UserThread.UIState): UIThread =
    uiState match {
      case UserThread.Ready =>
        new AskUserThread(
          name,
          s,
          terminal,
          onLine,
          onMaintenance
        )
      case UserThread.Blocked(remaining) =>
        val opts = remaining.flatMap { e: Exec =>
          e.execId.map(i => (e.commandLine, i))
        } match {
          case Seq() => Map.empty[Char, BlockedUIThread.CommandOption]
          case Seq((cmd, id)) if cmd.trim.nonEmpty =>
            Map('k' -> BlockedUIThread.Action(() => s"kill $id", s"kill $cmd $id"))
          case cmds =>
            val rest = cmds.zipWithIndex.collect {
              case ((cmd, id), index) if cmd.nonEmpty =>
                index.toString.head -> BlockedUIThread.Action(() => s"kill $id", s"kill $cmd $id")
            }.toMap
            Map('k' -> BlockedUIThread.More(rest))
        }
        new BlockedUIThread(
          name,
          s,
          terminal,
          opts,
          onMaintenance
        )
    }

  private[sbt] def reset(state: State, uiState: UserThread.UIState): Unit = {
    askUserThread.synchronized {
      askUserThread.getAndSet(null) match {
        case null =>
        case t    => t.close()
      }
      askUserThread.set(makeAskUserThread(state, uiState))
    }
  }

  private[sbt] def stopThread(): Unit = askUserThread.synchronized {
    askUserThread.getAndSet(null) match {
      case null =>
      case t    => t.close()
    }
  }
}
