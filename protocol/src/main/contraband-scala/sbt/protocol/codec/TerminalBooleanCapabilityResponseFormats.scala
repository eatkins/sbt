/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalBooleanCapabilityResponseFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalBooleanCapabilityResponseFormat: JsonFormat[sbt.protocol.TerminalBooleanCapabilityResponse] = new JsonFormat[sbt.protocol.TerminalBooleanCapabilityResponse] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalBooleanCapabilityResponse = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      val id = unbuilder.readField[String]("id")
      val capability = unbuilder.readField[Boolean]("capability")
      unbuilder.endObject()
      sbt.protocol.TerminalBooleanCapabilityResponse(id, capability)
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalBooleanCapabilityResponse, builder: Builder[J]): Unit = {
    builder.beginObject()
    builder.addField("id", obj.id)
    builder.addField("capability", obj.capability)
    builder.endObject()
  }
}
}
