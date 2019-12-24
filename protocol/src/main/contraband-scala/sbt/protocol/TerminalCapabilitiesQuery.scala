/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalCapabilitiesQuery private (
  val id: String,
  val boolean: Option[String],
  val numeric: Option[String],
  val string: Option[String]) extends sbt.protocol.CommandMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalCapabilitiesQuery => (this.id == x.id) && (this.boolean == x.boolean) && (this.numeric == x.numeric) && (this.string == x.string)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (37 * (17 + "sbt.protocol.TerminalCapabilitiesQuery".##) + id.##) + boolean.##) + numeric.##) + string.##)
  }
  override def toString: String = {
    "TerminalCapabilitiesQuery(" + id + ", " + boolean + ", " + numeric + ", " + string + ")"
  }
  private[this] def copy(id: String = id, boolean: Option[String] = boolean, numeric: Option[String] = numeric, string: Option[String] = string): TerminalCapabilitiesQuery = {
    new TerminalCapabilitiesQuery(id, boolean, numeric, string)
  }
  def withId(id: String): TerminalCapabilitiesQuery = {
    copy(id = id)
  }
  def withBoolean(boolean: Option[String]): TerminalCapabilitiesQuery = {
    copy(boolean = boolean)
  }
  def withBoolean(boolean: String): TerminalCapabilitiesQuery = {
    copy(boolean = Option(boolean))
  }
  def withNumeric(numeric: Option[String]): TerminalCapabilitiesQuery = {
    copy(numeric = numeric)
  }
  def withNumeric(numeric: String): TerminalCapabilitiesQuery = {
    copy(numeric = Option(numeric))
  }
  def withString(string: Option[String]): TerminalCapabilitiesQuery = {
    copy(string = string)
  }
  def withString(string: String): TerminalCapabilitiesQuery = {
    copy(string = Option(string))
  }
}
object TerminalCapabilitiesQuery {
  
  def apply(id: String, boolean: Option[String], numeric: Option[String], string: Option[String]): TerminalCapabilitiesQuery = new TerminalCapabilitiesQuery(id, boolean, numeric, string)
  def apply(id: String, boolean: String, numeric: String, string: String): TerminalCapabilitiesQuery = new TerminalCapabilitiesQuery(id, Option(boolean), Option(numeric), Option(string))
}
