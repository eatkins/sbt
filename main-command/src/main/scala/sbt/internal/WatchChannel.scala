package sbt.internal
import sbt.{ Exec, State }
import sbt.protocol.EventMessage
import sjsonnew.JsonFormat

private[sbt] final class WatchChannel(val exec: Exec) extends CommandChannel {

  override def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = ()

  override def publishEventMessage(event: EventMessage): Unit = event match {
    case StartWatchEvent(state, index) =>
      println(s"$state $index")
      ()
    case _ =>
  }

  override def publishBytes(bytes: Array[Byte]): Unit = ()

  override def shutdown(): Unit = ()

  override def name: String = s"watch: ${exec.commandLine}"
}

private[sbt] object WatchChannel {
  final class Config(
      val inputReader: Option[() => Option[Exec]],
      val fileEventReader: Option[() => Option[Exec]],
      val preRunStateUpdate: Option[State => State],
      val postRunStateUpdate: Option[State => State]
  )
}
