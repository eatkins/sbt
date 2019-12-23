/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalStringCapabilityResponseFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalStringCapabilityResponseFormat: JsonFormat[sbt.protocol.TerminalStringCapabilityResponse] = new JsonFormat[sbt.protocol.TerminalStringCapabilityResponse] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalStringCapabilityResponse = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      val id = unbuilder.readField[String]("id")
      val capability = unbuilder.readField[String]("capability")
      unbuilder.endObject()
      sbt.protocol.TerminalStringCapabilityResponse(id, capability)
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalStringCapabilityResponse, builder: Builder[J]): Unit = {
    builder.beginObject()
    builder.addField("id", obj.id)
    builder.addField("capability", obj.capability)
    builder.endObject()
  }
}
}
