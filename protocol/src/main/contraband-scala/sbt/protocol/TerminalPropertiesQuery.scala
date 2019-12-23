/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalPropertiesQuery private (
  val id: String) extends sbt.protocol.CommandMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalPropertiesQuery => (this.id == x.id)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (17 + "sbt.protocol.TerminalPropertiesQuery".##) + id.##)
  }
  override def toString: String = {
    "TerminalPropertiesQuery(" + id + ")"
  }
  private[this] def copy(id: String = id): TerminalPropertiesQuery = {
    new TerminalPropertiesQuery(id)
  }
  def withId(id: String): TerminalPropertiesQuery = {
    copy(id = id)
  }
}
object TerminalPropertiesQuery {
  
  def apply(id: String): TerminalPropertiesQuery = new TerminalPropertiesQuery(id)
}
