/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal

import sbt.librarymanagement._
import java.io.File
import UpdateReportCodecs.jf
import sjsonnew.support.scalajson.unsafe.Converter
import scala.util.Success

class UpdateReportFormatSpec extends org.scalatest.FlatSpec {
  "UpdateReport" should "serialize" in {
    val us = UpdateStats(0, 0, 0, true)
    val ur = UpdateReport(new File("foo"), Vector.empty, us, Map.empty)
    val json = Converter.toJson(ur).get
    assert(Converter.fromJson(json) == Success(ur))
  }
}
