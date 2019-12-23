/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalPropertiesResponseFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalPropertiesResponseFormat: JsonFormat[sbt.protocol.TerminalPropertiesResponse] = new JsonFormat[sbt.protocol.TerminalPropertiesResponse] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalPropertiesResponse = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      val width = unbuilder.readField[Int]("width")
      val height = unbuilder.readField[Int]("height")
      val isAnsiSupported = unbuilder.readField[Boolean]("isAnsiSupported")
      val isEchoEnabled = unbuilder.readField[Boolean]("isEchoEnabled")
      unbuilder.endObject()
      sbt.protocol.TerminalPropertiesResponse(width, height, isAnsiSupported, isEchoEnabled)
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalPropertiesResponse, builder: Builder[J]): Unit = {
    builder.beginObject()
    builder.addField("width", obj.width)
    builder.addField("height", obj.height)
    builder.addField("isAnsiSupported", obj.isAnsiSupported)
    builder.addField("isEchoEnabled", obj.isEchoEnabled)
    builder.endObject()
  }
}
}
