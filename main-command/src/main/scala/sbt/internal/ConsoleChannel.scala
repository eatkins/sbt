/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import sbt.internal.ui.UITask
import sbt.internal.util._
import sjsonnew.JsonFormat

private[sbt] final class ConsoleChannel(
    val name: String,
    override private[sbt] val mkUIThread: (State, CommandChannel) => UITask
) extends CommandChannel {
  override private[sbt] def terminal = Terminal.console

  def run(s: State): State = s

  def publishBytes(bytes: Array[Byte]): Unit = ()

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()

}

private[sbt] final class ScriptedCommandChannel(
    override private[sbt] val mkUIThread: (State, CommandChannel) => UITask
) extends CommandChannel {
  override val name: String = "anonymous"

  override private[sbt] def terminal = new Terminal {
    val delegate = Terminal.console
    override def close(): Unit = {}
    def getBooleanCapability(capability: String): Boolean =
      delegate.getBooleanCapability(capability)
    def getHeight: Int = delegate.getHeight
    def getLastLine: Option[String] = delegate.getLastLine
    def getLineHeightAndWidth(line: String): (Int, Int) = delegate.getLineHeightAndWidth(line)
    def getNumericCapability(capability: String): Int = delegate.getNumericCapability(capability)
    def getStringCapability(capability: String): String = delegate.getStringCapability(capability)
    def getWidth: Int = Terminal.console.getWidth
    def inputStream: java.io.InputStream = new java.io.InputStream {
      override def read = {
        this.synchronized(this.wait())
        -1
      }
    }
    def isAnsiSupported: Boolean = delegate.isAnsiSupported
    def isColorEnabled: Boolean = delegate.isColorEnabled
    def isEchoEnabled: Boolean = delegate.isEchoEnabled
    def isSupershellEnabled: Boolean = false
    def outputStream: java.io.OutputStream = System.out
    private[sbt] def printStream: java.io.PrintStream = System.out
    private[sbt] def withPrintStream[T](f: java.io.PrintStream => T): T = f(System.out)
    private[sbt] def write(bytes: Int*): Unit = ()
  }

  def run(s: State): State = s

  def publishBytes(bytes: Array[Byte]): Unit = ()

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()
}
