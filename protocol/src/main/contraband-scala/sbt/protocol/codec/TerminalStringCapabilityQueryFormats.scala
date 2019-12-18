/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol.codec
import _root_.sjsonnew.{ Unbuilder, Builder, JsonFormat, deserializationError }
trait TerminalStringCapabilityQueryFormats { self: sjsonnew.BasicJsonProtocol =>
implicit lazy val TerminalStringCapabilityQueryFormat: JsonFormat[sbt.protocol.TerminalStringCapabilityQuery] = new JsonFormat[sbt.protocol.TerminalStringCapabilityQuery] {
  override def read[J](__jsOpt: Option[J], unbuilder: Unbuilder[J]): sbt.protocol.TerminalStringCapabilityQuery = {
    __jsOpt match {
      case Some(__js) =>
      unbuilder.beginObject(__js)
      
      unbuilder.endObject()
      sbt.protocol.TerminalStringCapabilityQuery()
      case None =>
      deserializationError("Expected JsObject but found None")
    }
  }
  override def write[J](obj: sbt.protocol.TerminalStringCapabilityQuery, builder: Builder[J]): Unit = {
    builder.beginObject()
    
    builder.endObject()
  }
}
}
