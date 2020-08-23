/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util
package logic

import sbt.internal.util.logic.Logic.{ LogicException, Matched }

import org.scalacheck.Prop.secure
import org.scalacheck._

object LogicTest extends Properties("Logic") {
  import TestClauses._

  property("Handles trivial resolution.") = secure(expect(trivial, Set(A)))
  property("Handles less trivial resolution.") = secure(expect(lessTrivial, Set(B, A, D)))
  property("Handles cycles without negation") = secure(expect(cycles, Set(F, A, B)))
  property("Handles basic exclusion.") = secure(expect(excludedPos, Set()))
  property("Handles exclusion of head proved by negation.") = secure(expect(excludedNeg, Set()))
  // TODO: actually check ordering, probably as part of a check that dependencies are satisfied
  property("Properly orders results.") = secure(expect(ordering, Set(B, A, C, E, F)))
  property("Detects cyclic negation") = secure(
    Logic.reduceAll(badClauses, Set()) match {
      case Right(_)                      => false
      case Left(_: Logic.CyclicNegation) => true
      case Left(err)                     => sys.error(s"Expected cyclic error, got: $err")
    }
  )

  def expect(result: Either[LogicException, Matched], expected: Set[Atom]): Boolean = result match {
    case Left(_) => false
    case Right(res) =>
      val actual = res.provenSet
      if (actual != expected)
        sys.error(s"Expected to prove $expected, but actually proved $actual")
      else
        true
  }
}

object TestClauses {

  val A: Atom = Atom("A")
  val B: Atom = Atom("B")
  val C: Atom = Atom("C")
  val D: Atom = Atom("D")
  val E: Atom = Atom("E")
  val F: Atom = Atom("F")
  val G: Atom = Atom("G")

  val clauses: List[Clause] =
    A.proves(B) ::
      A.proves(F) ::
      B.proves(F) ::
      F.proves(A) ::
      (!C).proves(F) ::
      D.proves(C) ::
      C.proves(D) ::
      Nil

  val cycles: Either[LogicException, Matched] = Logic.reduceAll(clauses, Set())

  val badClauses: List[Clause] =
    A.proves(D) ::
      clauses

  val excludedNeg: Either[LogicException, Matched] = {
    val cs =
      (!A).proves(B) ::
        Nil
    val init =
      (!A) ::
        (!B) ::
        Nil
    Logic.reduceAll(cs, init.toSet)
  }

  val excludedPos: Either[LogicException, Matched] = {
    val cs =
      A.proves(B) ::
        Nil
    val init =
      A ::
        (!B) ::
        Nil
    Logic.reduceAll(cs, init.toSet)
  }

  val trivial: Either[LogicException, Matched] = {
    val cs =
      Formula.True.proves(A) ::
        Nil
    Logic.reduceAll(cs, Set.empty)
  }

  val lessTrivial: Either[LogicException, Matched] = {
    val cs =
      Formula.True.proves(A) ::
        Formula.True.proves(B) ::
        (A && B && (!C)).proves(D) ::
        Nil
    Logic.reduceAll(cs, Set())
  }

  val ordering: Either[LogicException, Matched] = {
    val cs =
      E.proves(F) ::
        (C && !D).proves(E) ::
        (A && B).proves(C) ::
        Nil
    Logic.reduceAll(cs, Set(A, B))
  }

  def all(): Unit = {
    println(s"Cycles: $cycles")
    println(s"xNeg: $excludedNeg")
    println(s"xPos: $excludedPos")
    println(s"trivial: $trivial")
    println(s"lessTrivial: $lessTrivial")
    println(s"ordering: $ordering")
  }
}
