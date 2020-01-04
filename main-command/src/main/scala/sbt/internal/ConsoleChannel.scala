/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import sbt.internal.ui.{ UIThread, UserThread }
import sbt.internal.util.Prompt.AskUser
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
      case ConsolePromptEvent(state) =>
        // Need to stop thread because the ConsoleChannel logs remote commands which screw up
        // the prompt.
        terminal.prompt match {
          case _: AskUser =>
          case p          => terminal.setPrompt(AskUser(() => UIThread.shellPrompt(terminal, state)))
        }
        reset(state, UserThread.Ready)
      case ConsoleUnpromptEvent(lastSource, state) =>
        if (lastSource.fold(true)(_.channelName != name)) {
          terminal.progressState.reset()
        } else stopThread()
//        terminal.progressState.reset()
//        if (lastSource.fold(true)(_.channelName != name))
//          reset(state, UserThread.Blocked)
//        else stopThread()
      case _ => //
    }
}
