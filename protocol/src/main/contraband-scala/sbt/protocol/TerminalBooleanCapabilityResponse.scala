/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalBooleanCapabilityResponse private (
  val id: String,
  val capability: Boolean) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalBooleanCapabilityResponse => (this.id == x.id) && (this.capability == x.capability)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (17 + "sbt.protocol.TerminalBooleanCapabilityResponse".##) + id.##) + capability.##)
  }
  override def toString: String = {
    "TerminalBooleanCapabilityResponse(" + id + ", " + capability + ")"
  }
  private[this] def copy(id: String = id, capability: Boolean = capability): TerminalBooleanCapabilityResponse = {
    new TerminalBooleanCapabilityResponse(id, capability)
  }
  def withId(id: String): TerminalBooleanCapabilityResponse = {
    copy(id = id)
  }
  def withCapability(capability: Boolean): TerminalBooleanCapabilityResponse = {
    copy(capability = capability)
  }
}
object TerminalBooleanCapabilityResponse {
  
  def apply(id: String, capability: Boolean): TerminalBooleanCapabilityResponse = new TerminalBooleanCapabilityResponse(id, capability)
}
