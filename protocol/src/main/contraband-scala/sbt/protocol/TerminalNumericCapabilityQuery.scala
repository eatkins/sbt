/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalNumericCapabilityQuery private (
  val id: String) extends sbt.protocol.CommandMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalNumericCapabilityQuery => (this.id == x.id)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalNumericCapabilityQuery".##) + id.##)
  }
  override def toString: String = {
    "TerminalNumericCapabilityQuery(" + id + ")"
  }
  private[this] def copy(id: String = id): TerminalNumericCapabilityQuery = {
    new TerminalNumericCapabilityQuery(id)
  }
  def withId(id: String): TerminalNumericCapabilityQuery = {
    copy(id = id)
  }
}
object TerminalNumericCapabilityQuery {
  
  def apply(id: String): TerminalNumericCapabilityQuery = new TerminalNumericCapabilityQuery(id)
}
