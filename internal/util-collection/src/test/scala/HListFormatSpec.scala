/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal
package util

import sbt.internal.util.HListFormats._
import sbt.internal.util.Types.:+:

import org.scalatest.Assertion
import sjsonnew.BasicJsonProtocol._
import sjsonnew._
import sjsonnew.shaded.scalajson.ast.unsafe._
import sjsonnew.support.scalajson.unsafe._

class HListFormatSpec extends UnitSpec {
  val quux: Int :+: (String :+: (Boolean :+: HNil)) = 23 :+: "quux" :+: true :+: HNil

  it should "round trip quux" in assertRoundTrip(quux)
  it should "round trip hnil" in assertRoundTrip(HNil)

  it should "have a flat structure for quux" in assertJsonString(quux, """[23,"quux",true]""")
  it should "have a flat structure for hnil" in assertJsonString(HNil, "[]")

  def assertRoundTrip[A: JsonWriter: JsonReader](x: A): Assertion = {
    val jsonString: String = toJsonString(x)
    val jValue: JValue = Parser.parseUnsafe(jsonString)
    val y: A = Converter.fromJson[A](jValue).get
    assert(x === y)
  }

  def assertJsonString[A: JsonWriter](x: A, s: String): Assertion = assert(toJsonString(x) === s)

  def toJsonString[A: JsonWriter](x: A): String = CompactPrinter(Converter.toJson(x).get)
}
