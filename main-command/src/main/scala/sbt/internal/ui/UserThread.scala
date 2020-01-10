/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.ui

import java.util.concurrent.atomic.AtomicReference

import sbt.State
import sbt.internal.{ ConsolePromptEvent, ConsoleUnpromptEvent }
import sbt.internal.util.{ ProgressEvent, Terminal }

private[sbt] object UserThread {
  sealed trait UIState
  case object Ready extends UIState
  case object Watch extends UIState
}

private[sbt] trait HasUserThread {
  private[this] val askUserThread = new AtomicReference[UIThread]
  def name: String
  private[sbt] def terminal: Terminal
  private[sbt] def onCommand: String => Boolean
  private[sbt] def onMaintenance: String => Boolean
  private[sbt] final def onProgressEvent(pe: ProgressEvent): Unit = askUserThread.get match {
    case null => terminal.progressState.reset()
    case t    => t.onProgressEvent(pe, terminal)
  }
  private[sbt] def makeUIThread(s: State): UIThread
  private[sbt] def onConsolePromptEvent(consolePromptEvent: ConsolePromptEvent): Unit
  private[sbt] def onConsoleUnpromptEvent(consoleunPromptEvent: ConsoleUnpromptEvent): Unit
  private[sbt] def reset(state: State, uiState: UserThread.UIState): Unit = {
    askUserThread.synchronized {
      val newThread = makeUIThread(state)
      askUserThread.getAndSet(newThread) match {
        case null                                  => newThread.start()
        case t if t.getClass == newThread.getClass => askUserThread.set(t)
        case _                                     => newThread.start()
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
