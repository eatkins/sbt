/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.ui

import sbt.State
import sbt.internal.util.{ ProgressEvent, ProgressState, Terminal }

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
  override private[sbt] def handleInput(s: Either[String, String]): Boolean = s match {
    case Left(c)  => onMaintenance(c)
    case Right(c) => onLine(c)
  }

  override private[sbt] def onProgressEvent(pe: ProgressEvent, terminal: Terminal): Unit = {
    ProgressState.updateProgressState(pe, terminal)
  }
}
