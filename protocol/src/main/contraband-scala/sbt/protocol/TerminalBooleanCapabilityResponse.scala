/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalBooleanCapabilityResponse private (
  val capability: Boolean) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalBooleanCapabilityResponse => (this.capability == x.capability)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalBooleanCapabilityResponse".##) + capability.##)
  }
  override def toString: String = {
    "TerminalBooleanCapabilityResponse(" + capability + ")"
  }
  private[this] def copy(capability: Boolean = capability): TerminalBooleanCapabilityResponse = {
    new TerminalBooleanCapabilityResponse(capability)
  }
  def withCapability(capability: Boolean): TerminalBooleanCapabilityResponse = {
    copy(capability = capability)
  }
}
object TerminalBooleanCapabilityResponse {
  
  def apply(capability: Boolean): TerminalBooleanCapabilityResponse = new TerminalBooleanCapabilityResponse(capability)
}
