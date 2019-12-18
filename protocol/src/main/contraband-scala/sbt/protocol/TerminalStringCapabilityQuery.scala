/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalStringCapabilityQuery private () extends sbt.protocol.CommandMessage() with Serializable {



override def equals(o: Any): Boolean = o match {
  case _: TerminalStringCapabilityQuery => true
  case _ => false
}
override def hashCode: Int = {
  37 * (17 + "sbt.protocol.TerminalStringCapabilityQuery".##)
}
override def toString: String = {
  "TerminalStringCapabilityQuery()"
}
private[this] def copy(): TerminalStringCapabilityQuery = {
  new TerminalStringCapabilityQuery()
}

}
object TerminalStringCapabilityQuery {
  
  def apply(): TerminalStringCapabilityQuery = new TerminalStringCapabilityQuery()
}
