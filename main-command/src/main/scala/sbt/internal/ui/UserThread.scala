/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.ui

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ Executors, Future }

import sbt.State
import sbt.internal.util.{ ProgressEvent, Terminal }
import sbt.internal.{ ConsolePromptEvent, ConsoleUnpromptEvent }

private[sbt] object UserThread {
  sealed trait UIState
  case object Ready extends UIState
  case object Watch extends UIState
}

private[sbt] trait HasUserThread {
  private[this] val askUserThread = new AtomicReference[(UITask, Future[_])]
  def name: String
  private[sbt] def terminal: Terminal
  private[sbt] def onCommand: String => Boolean
  private[sbt] def onMaintenance: String => Boolean
  private[sbt] final def onProgressEvent(pe: ProgressEvent): Unit = askUserThread.get match {
    case null   => terminal.progressState.reset()
    case (t, _) => t.onProgressEvent(pe, terminal)
  }
  private[sbt] def makeUIThread(s: State): UITask
  private[sbt] def onConsolePromptEvent(consolePromptEvent: ConsolePromptEvent): Unit
  private[sbt] def onConsoleUnpromptEvent(consoleunPromptEvent: ConsoleUnpromptEvent): Unit
  private[this] val executor =
    Executors.newSingleThreadExecutor(r => new Thread(r, s"sbt-$name-ui-thread"))
  private[sbt] def reset(state: State, uiState: UserThread.UIState): Unit = {
    askUserThread.synchronized {
      val task = makeUIThread(state)
      askUserThread.get match {
        case null                                  => askUserThread.set((task, executor.submit(task)))
        case (t, _) if t.getClass == task.getClass =>
        case (_, f) =>
          f.cancel(true)
          askUserThread.set((task, executor.submit(task)))
      }
    }
  }

  private[sbt] def stopThread(): Unit = askUserThread.synchronized {
    askUserThread.getAndSet(null) match {
      case null =>
      case (t, f) =>
        t.close()
        f.cancel(true)
        executor.shutdown()
    }
  }
}
