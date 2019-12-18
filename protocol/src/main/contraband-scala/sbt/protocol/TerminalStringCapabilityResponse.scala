/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalStringCapabilityResponse private (
  val capability: String) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalStringCapabilityResponse => (this.capability == x.capability)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalStringCapabilityResponse".##) + capability.##)
  }
  override def toString: String = {
    "TerminalStringCapabilityResponse(" + capability + ")"
  }
  private[this] def copy(capability: String = capability): TerminalStringCapabilityResponse = {
    new TerminalStringCapabilityResponse(capability)
  }
  def withCapability(capability: String): TerminalStringCapabilityResponse = {
    copy(capability = capability)
  }
}
object TerminalStringCapabilityResponse {
  
  def apply(capability: String): TerminalStringCapabilityResponse = new TerminalStringCapabilityResponse(capability)
}
