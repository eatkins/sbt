/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalCapabilitiesResponse private (
  val id: String,
  val boolean: Option[Boolean],
  val numeric: Option[Int],
  val string: Option[String]) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalCapabilitiesResponse => (this.id == x.id) && (this.boolean == x.boolean) && (this.numeric == x.numeric) && (this.string == x.string)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (37 * (17 + "sbt.protocol.TerminalCapabilitiesResponse".##) + id.##) + boolean.##) + numeric.##) + string.##)
  }
  override def toString: String = {
    "TerminalCapabilitiesResponse(" + id + ", " + boolean + ", " + numeric + ", " + string + ")"
  }
  private[this] def copy(id: String = id, boolean: Option[Boolean] = boolean, numeric: Option[Int] = numeric, string: Option[String] = string): TerminalCapabilitiesResponse = {
    new TerminalCapabilitiesResponse(id, boolean, numeric, string)
  }
  def withId(id: String): TerminalCapabilitiesResponse = {
    copy(id = id)
  }
  def withBoolean(boolean: Option[Boolean]): TerminalCapabilitiesResponse = {
    copy(boolean = boolean)
  }
  def withBoolean(boolean: Boolean): TerminalCapabilitiesResponse = {
    copy(boolean = Option(boolean))
  }
  def withNumeric(numeric: Option[Int]): TerminalCapabilitiesResponse = {
    copy(numeric = numeric)
  }
  def withNumeric(numeric: Int): TerminalCapabilitiesResponse = {
    copy(numeric = Option(numeric))
  }
  def withString(string: Option[String]): TerminalCapabilitiesResponse = {
    copy(string = string)
  }
  def withString(string: String): TerminalCapabilitiesResponse = {
    copy(string = Option(string))
  }
}
object TerminalCapabilitiesResponse {
  
  def apply(id: String, boolean: Option[Boolean], numeric: Option[Int], string: Option[String]): TerminalCapabilitiesResponse = new TerminalCapabilitiesResponse(id, boolean, numeric, string)
  def apply(id: String, boolean: Boolean, numeric: Int, string: String): TerminalCapabilitiesResponse = new TerminalCapabilitiesResponse(id, Option(boolean), Option(numeric), Option(string))
}
