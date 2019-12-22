/**
 * This code is generated using [[http://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package sbt.protocol
final class InitCommand private (
  val token: Option[String],
  val execId: Option[String],
  val collectAnalysis: Option[Boolean]) extends sbt.protocol.CommandMessage() with Serializable {
  
  private def this(token: Option[String], execId: Option[String]) = this(token, execId, None)
  
  override def equals(o: Any): Boolean = o match {
    case x: InitCommand => (this.token == x.token) && (this.execId == x.execId) && (this.collectAnalysis == x.collectAnalysis)
    case _ => false
  }
  override def hashCode: Int = {
    37 * (37 * (37 * (37 * (17 + "sbt.protocol.InitCommand".##) + token.##) + execId.##) + collectAnalysis.##)
  }
  override def toString: String = {
    "InitCommand(" + token + ", " + execId + ", " + collectAnalysis + ")"
  }
  private[this] def copy(token: Option[String] = token, execId: Option[String] = execId, collectAnalysis: Option[Boolean] = collectAnalysis): InitCommand = {
    new InitCommand(token, execId, collectAnalysis)
  }
  def withToken(token: Option[String]): InitCommand = {
    copy(token = token)
  }
  def withToken(token: String): InitCommand = {
    copy(token = Option(token))
  }
  def withExecId(execId: Option[String]): InitCommand = {
    copy(execId = execId)
  }
  def withExecId(execId: String): InitCommand = {
    copy(execId = Option(execId))
  }
  def withCollectAnalysis(collectAnalysis: Option[Boolean]): InitCommand = {
    copy(collectAnalysis = collectAnalysis)
  }
  def withCollectAnalysis(collectAnalysis: Boolean): InitCommand = {
    copy(collectAnalysis = Option(collectAnalysis))
  }
}
object InitCommand {
  
  def apply(token: Option[String], execId: Option[String]): InitCommand = new InitCommand(token, execId)
  def apply(token: String, execId: String): InitCommand = new InitCommand(Option(token), Option(execId))
  def apply(token: Option[String], execId: Option[String], collectAnalysis: Option[Boolean]): InitCommand = new InitCommand(token, execId, collectAnalysis)
  def apply(token: String, execId: String, collectAnalysis: Boolean): InitCommand = new InitCommand(Option(token), Option(execId), Option(collectAnalysis))
}
