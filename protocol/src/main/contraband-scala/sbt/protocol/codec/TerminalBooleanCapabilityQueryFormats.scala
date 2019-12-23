/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalBooleanCapabilityQueryFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalBooleanCapabilityQueryFormat: JsonFormat[sbt.protocol.TerminalBooleanCapabilityQuery] = new JsonFormat[sbt.protocol.TerminalBooleanCapabilityQuery] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalBooleanCapabilityQuery = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      val id = unbuilder.readField[String]("id")
      unbuilder.endObject()
      sbt.protocol.TerminalBooleanCapabilityQuery(id)
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalBooleanCapabilityQuery, builder: Builder[J]): Unit = {
    builder.beginObject()
    builder.addField("id", obj.id)
    builder.endObject()
  }
}
}
