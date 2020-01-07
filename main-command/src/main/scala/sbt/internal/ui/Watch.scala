package sbt.internal.ui

import sbt.internal.util.AttributeKey

private[sbt] object Watch {
  val key = AttributeKey[Map[String, State]]("watch-state", "", Int.MaxValue)
  trait State {
    def iteration: Int
    def prompt: Int => String
    def onLine: String => String
  }
}
