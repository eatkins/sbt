/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalNumericCapabilityQuery private () extends sbt.protocol.CommandMessage() with Serializable {



override def equals(o: Any): Boolean = o match {
  case _: TerminalNumericCapabilityQuery => true
  case _ => false
}
override def hashCode: Int = {
  37 * (17 + "sbt.protocol.TerminalNumericCapabilityQuery".##)
}
override def toString: String = {
  "TerminalNumericCapabilityQuery()"
}
private[this] def copy(): TerminalNumericCapabilityQuery = {
  new TerminalNumericCapabilityQuery()
}

}
object TerminalNumericCapabilityQuery {
  
  def apply(): TerminalNumericCapabilityQuery = new TerminalNumericCapabilityQuery()
}
