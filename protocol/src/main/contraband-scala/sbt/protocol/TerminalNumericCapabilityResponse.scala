/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalNumericCapabilityResponse private (
  val capability: String) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalNumericCapabilityResponse => (this.capability == x.capability)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalNumericCapabilityResponse".##) + capability.##)
  }
  override def toString: String = {
    "TerminalNumericCapabilityResponse(" + capability + ")"
  }
  private[this] def copy(capability: String = capability): TerminalNumericCapabilityResponse = {
    new TerminalNumericCapabilityResponse(capability)
  }
  def withCapability(capability: String): TerminalNumericCapabilityResponse = {
    copy(capability = capability)
  }
}
object TerminalNumericCapabilityResponse {
  
  def apply(capability: String): TerminalNumericCapabilityResponse = new TerminalNumericCapabilityResponse(capability)
}
