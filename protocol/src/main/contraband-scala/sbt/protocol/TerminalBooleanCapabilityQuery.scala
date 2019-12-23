/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class TerminalBooleanCapabilityQuery private () extends sbt.protocol.CommandMessage() with Serializable {



override def equals(o: Any): Boolean = o match {
  case _: TerminalBooleanCapabilityQuery => true
  case _ => false
}
override def hashCode: Int = {
  37 * (17 + "sbt.protocol.TerminalBooleanCapabilityQuery".##)
}
override def toString: String = {
  "TerminalBooleanCapabilityQuery()"
}
private[this] def copy(): TerminalBooleanCapabilityQuery = {
  new TerminalBooleanCapabilityQuery()
}

}
object TerminalBooleanCapabilityQuery {
  
  def apply(): TerminalBooleanCapabilityQuery = new TerminalBooleanCapabilityQuery()
}
