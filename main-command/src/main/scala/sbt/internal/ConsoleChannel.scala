/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import sbt.internal.ui.UIThread
import sbt.internal.util._
import sjsonnew.JsonFormat

private[sbt] final class ConsoleChannel(
    val name: String,
    override private[sbt] val mkUIThread: (
        State,
        Terminal,
        String => Boolean,
        String => Boolean
    ) => UIThread
) extends CommandChannel {
  override private[sbt] def terminal = Terminal.console

  def run(s: State): State = s

  def publishBytes(bytes: Array[Byte]): Unit = ()

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()

}
