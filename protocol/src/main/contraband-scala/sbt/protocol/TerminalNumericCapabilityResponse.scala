/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalNumericCapabilityResponse private (
  val id: String,
  val capability: String) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalNumericCapabilityResponse => (this.id == x.id) && (this.capability == x.capability)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (17 + "sbt.protocol.TerminalNumericCapabilityResponse".##) + id.##) + capability.##)
  }
  override def toString: String = {
    "TerminalNumericCapabilityResponse(" + id + ", " + capability + ")"
  }
  private[this] def copy(id: String = id, capability: String = capability): TerminalNumericCapabilityResponse = {
    new TerminalNumericCapabilityResponse(id, capability)
  }
  def withId(id: String): TerminalNumericCapabilityResponse = {
    copy(id = id)
  }
  def withCapability(capability: String): TerminalNumericCapabilityResponse = {
    copy(capability = capability)
  }
}
object TerminalNumericCapabilityResponse {
  
  def apply(id: String, capability: String): TerminalNumericCapabilityResponse = new TerminalNumericCapabilityResponse(id, capability)
}
