/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalNumericCapabilityQueryFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalNumericCapabilityQueryFormat: JsonFormat[sbt.protocol.TerminalNumericCapabilityQuery] = new JsonFormat[sbt.protocol.TerminalNumericCapabilityQuery] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalNumericCapabilityQuery = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      
      unbuilder.endObject()
      sbt.protocol.TerminalNumericCapabilityQuery()
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalNumericCapabilityQuery, builder: Builder[J]): Unit = {
    builder.beginObject()
    
    builder.endObject()
  }
}
}
