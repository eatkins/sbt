/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalStringCapabilityQuery private (
  val id: String) extends sbt.protocol.CommandMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalStringCapabilityQuery => (this.id == x.id)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalStringCapabilityQuery".##) + id.##)
  }
  override def toString: String = {
    "TerminalStringCapabilityQuery(" + id + ")"
  }
  private[this] def copy(id: String = id): TerminalStringCapabilityQuery = {
    new TerminalStringCapabilityQuery(id)
  }
  def withId(id: String): TerminalStringCapabilityQuery = {
    copy(id = id)
  }
}
object TerminalStringCapabilityQuery {
  
  def apply(id: String): TerminalStringCapabilityQuery = new TerminalStringCapabilityQuery(id)
}
