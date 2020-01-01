/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import sbt.internal.ui.UserThread
import sbt.internal.util._
import sbt.protocol.EventMessage
import sjsonnew.JsonFormat

private[sbt] final class ConsoleChannel(val name: String) extends CommandChannel {
  override private[sbt] def terminal = Terminal.console

  def run(s: State): State = s

  def publishBytes(bytes: Array[Byte]): Unit = ()

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()

  def publishEventMessage(event: EventMessage): Unit =
    event match {
      case e: ConsolePromptEvent => reset(e.state, UserThread.Ready)
      case ConsoleUnpromptEvent(lastSource, state) =>
        terminal.progressState.reset()
        if (lastSource.fold(true)(_.channelName != name))
          reset(state, UserThread.Blocked)
        else stopThread()
      case _ => //
    }
}
