/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalBooleanCapabilityQuery private (
  val id: String) extends sbt.protocol.CommandMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalBooleanCapabilityQuery => (this.id == x.id)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalBooleanCapabilityQuery".##) + id.##)
  }
  override def toString: String = {
    "TerminalBooleanCapabilityQuery(" + id + ")"
  }
  private[this] def copy(id: String = id): TerminalBooleanCapabilityQuery = {
    new TerminalBooleanCapabilityQuery(id)
  }
  def withId(id: String): TerminalBooleanCapabilityQuery = {
    copy(id = id)
  }
}
object TerminalBooleanCapabilityQuery {
  
  def apply(id: String): TerminalBooleanCapabilityQuery = new TerminalBooleanCapabilityQuery(id)
}
