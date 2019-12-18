/**
 * This code is generated using [[https://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class Attach private () extends sbt.protocol.CommandMessage() with Serializable {



override def equals(o: Any): Boolean = o match {
  case _: Attach => true
  case _ => false
}
override def hashCode: Int = {
  37 * (17 + "sbt.protocol.Attach".##)
}
override def toString: String = {
  "Attach()"
}
private[this] def copy(): Attach = {
  new Attach()
}

}
object Attach {
  
  def apply(): Attach = new Attach()
}
