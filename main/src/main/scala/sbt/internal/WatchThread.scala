package sbt.internal
import sbt.internal.util.{ ProgressEvent, Terminal }

private[sbt] final class WatchThread(terminal: Terminal) extends ui.UIThread {
  override private[sbt] def reader: ui.UIThread.Reader = () => Right("")
  override private[sbt] def handleInput(s: Either[String, String]) = false
  override private[sbt] def onProgressEvent(pe: ProgressEvent, terminal: Terminal): Unit = {}
}
