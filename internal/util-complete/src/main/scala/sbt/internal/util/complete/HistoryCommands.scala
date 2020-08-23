/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util
package complete

import sbt.internal.util.Util.{ AnyOps, nil }
import sbt.io.IO

object HistoryCommands {
  val Start = "!"
  // second characters
  val Contains = "?"
  val Last = "!"
  val ListCommands = ":"

  def ContainsFull: String = h(Contains)
  def LastFull: String = h(Last)
  def ListFull: String = h(ListCommands)

  def ListN: String = ListFull + "n"
  def ContainsString: String = ContainsFull + "string"
  def StartsWithString: String = Start + "string"
  def Previous: String = Start + "-n"
  def Nth: String = Start + "n"

  private def h(s: String) = Start + s
  def plainCommands: Seq[String] = Seq(ListFull, Start, LastFull, ContainsFull)

  def descriptions: Seq[(String, String)] = Seq(
    LastFull -> "Execute the last command again",
    ListFull -> "Show all previous commands",
    ListN -> "Show the last n commands",
    Nth -> ("Execute the command with index n, as shown by the " + ListFull + " command"),
    Previous -> "Execute the nth command before this one",
    StartsWithString -> "Execute the most recent command starting with 'string'",
    ContainsString -> "Execute the most recent command containing 'string'"
  )

  def helpString: String =
    "History commands:\n   " + (descriptions
      .map { case (c, d) => c + "    " + d })
      .mkString("\n   ")

  def printHelp(): Unit = println(helpString)

  def printHistory(history: complete.History, historySize: Int, show: Int): Unit =
    history.list(historySize, show).foreach(println)

  import DefaultParsers._

  val MaxLines = 500
  lazy val num: Parser[Int] = token(NatBasic, "<integer>")
  lazy val last: Parser[History => Option[List[String]]] = Last ^^^ { execute(_.!!) }

  lazy val list
      : Parser[History => Option[List[String]]] = ListCommands ~> (num ?? Int.MaxValue) map {
    show => (h: History) =>
      { printHistory(h, MaxLines, show); nil[String].some }
  }

  lazy val execStr: Parser[History => Option[List[String]]] = flag('?') ~ token(
    any.+.string,
    "<string>"
  ) map {
    case (contains, str) =>
      execute(h => if (contains) h !? str else h ! str)
  }

  lazy val execInt: Parser[History => Option[List[String]]] = flag('-') ~ num map {
    case (neg, value) =>
      execute(h => if (neg) h !- value else h ! value)
  }

  lazy val help: Parser[History => Option[List[String]]] = success((h: History) => {
    printHelp(); nil[String].some
  })

  def execute(f: History => Option[String]): History => Option[List[String]] = (h: History) => {
    val command = f(h).filterNot(_.startsWith(Start))
    val lines = h.lines.toArray
    command.foreach(lines(lines.length - 1) = _)
    h.path foreach { h =>
      IO.writeLines(h, lines)
    }
    command.toList.some
  }

  val actionParser: Parser[complete.History => Option[List[String]]] =
    Start ~> (help | last | execInt | list | execStr) // execStr must come last
}
