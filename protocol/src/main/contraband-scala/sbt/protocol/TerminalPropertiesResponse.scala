/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalPropertiesResponse private (
  val id: String,
  val length: Int,
  val height: Int,
  val isAnsiSupported: Boolean,
  val isEchoEnabled: Boolean) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalPropertiesResponse => (this.id == x.id) && (this.length == x.length) && (this.height == x.height) && (this.isAnsiSupported == x.isAnsiSupported) && (this.isEchoEnabled == x.isEchoEnabled)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (37 * (37 * (17 + "sbt.protocol.TerminalPropertiesResponse".##) + id.##) + length.##) + height.##) + isAnsiSupported.##) + isEchoEnabled.##)
  }
  override def toString: String = {
    "TerminalPropertiesResponse(" + id + ", " + length + ", " + height + ", " + isAnsiSupported + ", " + isEchoEnabled + ")"
  }
  private[this] def copy(id: String = id, length: Int = length, height: Int = height, isAnsiSupported: Boolean = isAnsiSupported, isEchoEnabled: Boolean = isEchoEnabled): TerminalPropertiesResponse = {
    new TerminalPropertiesResponse(id, length, height, isAnsiSupported, isEchoEnabled)
  }
  def withId(id: String): TerminalPropertiesResponse = {
    copy(id = id)
  }
  def withLength(length: Int): TerminalPropertiesResponse = {
    copy(length = length)
  }
  def withHeight(height: Int): TerminalPropertiesResponse = {
    copy(height = height)
  }
  def withIsAnsiSupported(isAnsiSupported: Boolean): TerminalPropertiesResponse = {
    copy(isAnsiSupported = isAnsiSupported)
  }
  def withIsEchoEnabled(isEchoEnabled: Boolean): TerminalPropertiesResponse = {
    copy(isEchoEnabled = isEchoEnabled)
  }
}
object TerminalPropertiesResponse {
  
  def apply(id: String, length: Int, height: Int, isAnsiSupported: Boolean, isEchoEnabled: Boolean): TerminalPropertiesResponse = new TerminalPropertiesResponse(id, length, height, isAnsiSupported, isEchoEnabled)
}
