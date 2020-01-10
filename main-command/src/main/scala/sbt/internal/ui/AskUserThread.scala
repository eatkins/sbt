/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.ui

import sbt.State
import sbt.internal.CommandChannel
import sbt.internal.util.{ ProgressEvent, ProgressState, Terminal }

private[sbt] class AskUserThread(
    state: State,
    override val channel: CommandChannel,
) extends Thread(s"ask-user-thread-${channel.name}")
    with UIThread {
  private[this] val terminal = channel.terminal
  override private[sbt] def reader: UIThread.Reader =
    UIThread.Reader.terminalReader(terminal.prompt, state.combinedParser)(terminal, state)

  override private[sbt] def onProgressEvent(pe: ProgressEvent, terminal: Terminal): Unit = {
    ProgressState.updateProgressState(pe, terminal)
  }
}
