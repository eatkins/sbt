/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalPropertiesQuery private () extends sbt.protocol.CommandMessage() with Serializable {



override def equals(o: Any): Boolean = o match {
  case _: TerminalPropertiesQuery => true
  case _ => false
}
override def hashCode: Int = {
  37 * (17 + "sbt.protocol.TerminalPropertiesQuery".##)
}
override def toString: String = {
  "TerminalPropertiesQuery()"
}
private[this] def copy(): TerminalPropertiesQuery = {
  new TerminalPropertiesQuery()
}

}
object TerminalPropertiesQuery {
  
  def apply(): TerminalPropertiesQuery = new TerminalPropertiesQuery()
}
