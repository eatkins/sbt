/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util

import org.scalatest.FlatSpec

class CleanStringSpec extends FlatSpec {
  "EscHelpers" should "not modify normal strings" in {
    val cleanString = s"1234"
    assert(EscHelpers.stripColorsAndMoves(cleanString) == cleanString)
  }
  it should "remove delete lines" in {
    val clean = "1234"
    val string = s"${ConsoleAppender.DeleteLine}$clean"
    assert(EscHelpers.stripColorsAndMoves(string) == clean)
  }
  it should "remove backspaces" in {
    val clean = "1234"
    val backspaced = s"1235${ConsoleAppender.cursorLeft(1)}${ConsoleAppender.clearLine(0)}4"
    assert(EscHelpers.stripColorsAndMoves(backspaced) == clean)
  }
  it should "remove colors" in {
    val clean = "1234"
    val colored = s"${scala.Console.RED}$clean${scala.Console.RESET}"
    assert(EscHelpers.stripColorsAndMoves(colored) == clean)
  }
}
