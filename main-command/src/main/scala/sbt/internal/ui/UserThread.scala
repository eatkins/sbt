package sbt.internal.ui

import java.util.concurrent.atomic.AtomicReference

import sbt.State
import sbt.internal.util.{ ProgressEvent, Terminal }

private[sbt] object UserThread {
  sealed trait UIState
  case object Ready extends UIState
  case object Blocked extends UIState
}

private[sbt] trait HasUserThread {
  private[this] val askUserThread = new AtomicReference[UIThread]
  def name: String
  private[sbt] def terminal: Terminal
  private[sbt] def onLine: String => Boolean
  private[sbt] def onMaintenance: String => Boolean
  private[sbt] final def onProgressEvent(pe: ProgressEvent): Unit = askUserThread.get match {
    case null => terminal.progressState.reset()
    case t    => t.onProgressEvent(pe, terminal)
  }

  private[sbt] def makeAskUserThread(s: State, uiState: UserThread.UIState): UIThread =
    uiState match {
      case UserThread.Ready | UserThread.Blocked =>
        new AskUserThread(
          name,
          s,
          terminal,
          onLine,
          onMaintenance
        )
//      case UserThread.Blocked =>
//        new BlockedUIThread(
//          name,
//          s,
//          terminal,
//          onLine,
//          onMaintenance
//        )
    }

  private[sbt] def reset(state: State, uiState: UserThread.UIState): Unit = {
    askUserThread.synchronized {
      askUserThread.get match {
        case a: AskUserThread if a.isAlive && uiState == UserThread.Ready =>
        case _ =>
          askUserThread.getAndSet(null) match {
            case null =>
            case t    => t.close()
          }
          askUserThread.set(makeAskUserThread(state, uiState))
      }
    }
  }

  private[sbt] def stopThread(): Unit = askUserThread.synchronized {
    askUserThread.getAndSet(null) match {
      case null =>
      case t    => t.close()
    }
  }
}
