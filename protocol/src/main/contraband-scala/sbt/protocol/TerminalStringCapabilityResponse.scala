/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalStringCapabilityResponse private (
  val id: String,
  val capability: String) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalStringCapabilityResponse => (this.id == x.id) && (this.capability == x.capability)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (17 + "sbt.protocol.TerminalStringCapabilityResponse".##) + id.##) + capability.##)
  }
  override def toString: String = {
    "TerminalStringCapabilityResponse(" + id + ", " + capability + ")"
  }
  private[this] def copy(id: String = id, capability: String = capability): TerminalStringCapabilityResponse = {
    new TerminalStringCapabilityResponse(id, capability)
  }
  def withId(id: String): TerminalStringCapabilityResponse = {
    copy(id = id)
  }
  def withCapability(capability: String): TerminalStringCapabilityResponse = {
    copy(capability = capability)
  }
}
object TerminalStringCapabilityResponse {
  
  def apply(id: String, capability: String): TerminalStringCapabilityResponse = new TerminalStringCapabilityResponse(id, capability)
}
