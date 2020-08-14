/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

import sbt.internal.util.AutoJson
import hedgehog._
import hedgehog.runner._
import scala.util.Success

import AutoJsonHelpers._

object AutoJsonPrimitiveSpec extends Properties {
  val sc: AutoJson[Seq[java.io.File]] = implicitly[AutoJson[Seq[java.io.File]]]
  def tests: List[Test] =
    primitiveTests ::: primitiveOptionTests ::: primitiveArrayTests

  // primitives
  private val primitiveTests =
    Test("Boolean serialization/deserialization", testBoolean) ::
      Test("Double serialization/deserialization", testDouble) ::
      Test("Int serialization/deserialization", testInt) ::
      Test("Long serialization/deserialization", testLong) ::
      Test("String serialization/deserialization", testString) ::
      Nil

  private val primitiveOptionTests =
    Test("Option[Boolean] serialization/deserialization", testBooleanOption) ::
      Test("Option[Double] serialization/deserialization", testDoubleOption) ::
      Test("Option[Int] serialization/deserialization", testIntOption) ::
      Test("Option[Long] serialization/deserialization", testLongOption) ::
      Test("Option[String] serialization/deserialization", testStringOption) ::
      Nil

  private val primitiveArrayTests =
    Test("Seq[Boolean] serialization/deserialization", testBooleanArray) ::
      Test("Seq[Double] serialization/deserialization", testDoubleArray) ::
      Test("Seq[Int] serialization/deserialization", testIntArray) ::
      Test("Seq[Long] serialization/deserialization", testLongArray) ::
      Test("Seq[String] serialization/deserialization", testStringArray) ::
      Test("Seq[File] serialization/deserialization", testFileArray) ::
      Nil

  private def randomBoolean = Gen.boolean
  private def randomDouble = Gen.double(Range.linearFrac(Double.MinValue, Double.MaxValue))
  private def randomInt = Gen.int(Range.linear(Int.MinValue, Int.MaxValue))
  private def randomLong = Gen.long(Range.linear(Long.MinValue, Long.MaxValue))
  private def randomString = Gen.string(Gen.alphaNum, Range.linear(0, 256))
  private def randomFile =
    Gen.list(Gen.string(Gen.alphaNum, Range.linear(0, 64)), Range.linear(0, 12)).map {
      case h :: tail => tail.foldLeft(new java.io.File(h)) { case (f, p) => new java.io.File(f, p) }
      case Nil       => new java.io.File("")
    }

  private def testBoolean: Property = randomBoolean.forAll.map(check[Boolean])
  private def testDouble: Property = randomDouble.forAll.map(check[Double])
  private def testInt: Property = randomInt.forAll.map(check[Int])
  private def testLong: Property = randomLong.forAll.map(check[Long])
  private def testString: Property = randomString.forAll.map(check[String])

  private def testBooleanOption: Property = randomBoolean.option.forAll.map(check[Option[Boolean]])
  private def testDoubleOption: Property = randomDouble.option.forAll.map(check[Option[Double]])
  private def testIntOption: Property = randomInt.option.forAll.map(check[Option[Int]])
  private def testLongOption: Property = randomLong.option.forAll.map(check[Option[Long]])
  private def testStringOption: Property = randomString.option.forAll.map(check[Option[String]])

  private def listLen = Range.linear(0, 256)
  def testSeq[T: AutoJson: scala.reflect.ClassTag](gen: core.GenT[T]) =
    Gen.list(gen, listLen).forAll.map { l =>
      check(l) and check(Seq(l: _*)) and check(l.toVector)
    }
  private def testBooleanArray: Property = testSeq(randomBoolean)
  private def testDoubleArray: Property = testSeq(randomDouble)
  private def testIntArray: Property = testSeq(randomInt)
  private def testLongArray: Property = testSeq(randomLong)
  private def testStringArray: Property = testSeq(randomString)
  private def testFileArray: Property = testSeq(randomFile)

  private def checkArray[A](a: A)(implicit aj: AutoJson[A]) = roundTrip(a) ==== a
  private def checkSjson[A](a: A)(implicit aj: AutoJson[A]) = roundTripSjson(a) ==== Success(a)
  def foo[A](a: A)(implicit aj: AutoJson[A]) = checkSjson(a) and checkArray(a)
  private def check[A](a: A)(implicit aj: AutoJson[A]) = {
    checkArray(a) and checkSjson(a)
  }
}
