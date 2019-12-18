/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalNumericCapabilityResponseFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalNumericCapabilityResponseFormat: JsonFormat[sbt.protocol.TerminalNumericCapabilityResponse] = new JsonFormat[sbt.protocol.TerminalNumericCapabilityResponse] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalNumericCapabilityResponse = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      val capability = unbuilder.readField[String]("capability")
      unbuilder.endObject()
      sbt.protocol.TerminalNumericCapabilityResponse(capability)
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalNumericCapabilityResponse, builder: Builder[J]): Unit = {
    builder.beginObject()
    builder.addField("capability", obj.capability)
    builder.endObject()
  }
}
}
