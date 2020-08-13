/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

import AutoJsonHelpers._
import sbt.internal.util.AutoJson
import scala.util.Success

class AutoJsonTupleSpec extends org.scalatest.FlatSpec {
  def check[A: AutoJson](a: A) = {
    assert(roundTrip(a) == a)
    assert(roundTripSjson(a) == Success(a))
  }
  "single tuples" should "be serializable" in {
    check(Tuple1(1))
    check(Tuple1("foo"))
    check(Tuple1(3.14159))
    check(Tuple1(Long.MaxValue))
  }
  "pairs" should "be serializable" in {
    check((1, "foo"))
    check((1, 2))
    check(("foo", 2))
    check((3.14159, 3))
    check((3.14159, 2.718))
    check((Long.MaxValue, "foo"))
  }
  "tuple10" should "be serializable" in {
    check((1, 2.0, "3", 4L, 5.0, "6", 7L, 8, "nine", 10))
  }
  "recursive tuples" should "be serializable" in {
    check((1, ("foo", (3.14, 5L))))
  }
  class Foo[T](val x: T) {
    override def equals(o: Any): Boolean = o match {
      case f: Foo[_] => f.x == x
      case _         => false
    }
  }
  "generic classes" should "be serializable" in {
    check(new Foo(1))
    check(Some(new Foo(1)))
    check(Vector(new Foo(1), new Foo(2)))
    check(Vector(Some(new Foo(1)), Some(new Foo(2))))
    check(Seq(new java.io.File("foo")))
    println(roundTrip(Vector(Some(new Foo(1)))))
  }
}
/*
 *class Foo private (val x: Int) {
 *  override def equals(o: Any): Boolean = o match {
 *    case f: Foo => f.x == x
 *    case _      => false
 *  }
 *}
 *object Foo {
 *  def apply(y: Int): Foo = new Foo(y)
 *}
 */
