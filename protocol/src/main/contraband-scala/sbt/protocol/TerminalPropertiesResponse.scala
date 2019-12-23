/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalPropertiesResponse private (
  val width: Int,
  val height: Int,
  val isAnsiSupported: Boolean,
  val isEchoEnabled: Boolean) extends sbt.protocol.EventMessage() with Serializable {
  
  
  
  override def equals(o: Any): Boolean = o match {
    case x: TerminalPropertiesResponse => (this.width == x.width) && (this.height == x.height) && (this.isAnsiSupported == x.isAnsiSupported) && (this.isEchoEnabled == x.isEchoEnabled)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (37 * (17 + "sbt.protocol.TerminalPropertiesResponse".##) + width.##) + height.##) + isAnsiSupported.##) + isEchoEnabled.##)
  }
  override def toString: String = {
    "TerminalPropertiesResponse(" + width + ", " + height + ", " + isAnsiSupported + ", " + isEchoEnabled + ")"
  }
  private[this] def copy(width: Int = width, height: Int = height, isAnsiSupported: Boolean = isAnsiSupported, isEchoEnabled: Boolean = isEchoEnabled): TerminalPropertiesResponse = {
    new TerminalPropertiesResponse(width, height, isAnsiSupported, isEchoEnabled)
  }
  def withWidth(width: Int): TerminalPropertiesResponse = {
    copy(width = width)
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
  
  def apply(width: Int, height: Int, isAnsiSupported: Boolean, isEchoEnabled: Boolean): TerminalPropertiesResponse = new TerminalPropertiesResponse(width, height, isAnsiSupported, isEchoEnabled)
}
