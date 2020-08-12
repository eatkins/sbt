/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal

import java.util.{ Calendar, TimeZone }
import sbt.internal.util.JsonUnbuilder
import sbt.internal.util.JsonBuilder
import sbt.internal.util.AutoJson
import sbt.librarymanagement._
//import sjsonnew.JsonFormat
import AutoJson._
import sjsonnew.JsonFormat

object UpdateReportCodecs {

  implicit val af: AutoJson[Artifact] = AutoJson.macroDefault
  implicit val cs: AutoJson[Checksum] = AutoJson.macroDefault
  implicit val cv: AutoJson[CrossVersion] = new AutoJson[CrossVersion] {
    override def read(unbuilder: JsonUnbuilder): CrossVersion = CrossVersion.Full()
    override def write(obj: CrossVersion, builder: JsonBuilder): Unit = {}
  }
  implicit val cal: AutoJson[Calendar] = new AutoJson[Calendar] {
    override def read(unbuilder: JsonUnbuilder): Calendar = {
      val builder = new Calendar.Builder()
      builder.setTimeZone(TimeZone.getTimeZone(unbuilder.readString))
      builder.build()
    }
    override def write(obj: Calendar, builder: JsonBuilder): Unit = {
      builder.writeString(obj.getTimeZone.getID)
    }

  }
  implicit val ui: AutoJson[sbt.internal.LibraryManagement.UpdateInputs] = implicitly
  implicit val jfui: JsonFormat[sbt.internal.LibraryManagement.UpdateInputs] = AutoJson.jsonFormat
  implicit val cr: AutoJson[ConfigRef] = AutoJson.macroDefault
  implicit val mi: AutoJson[ModuleID] = AutoJson.macroDefault
  implicit val jfmi: JsonFormat[ModuleID] = AutoJson.jsonFormat
  implicit val iel: AutoJson[InclExclRule] = AutoJson.macroDefault
  implicit val cfj: AutoJson[ConfigurationReport] = implicitly[AutoJson[ConfigurationReport]]
  implicit val jf: JsonFormat[UpdateReport] = AutoJson.jsonFormat
  println(s"FUCK $jf")
  /*
 *implicit val ir: AutoJson[InclExclRule] = AutoJson.macroDefault[InclExclRule]
 *implicit val oar: AutoJson[OrganizationArtifactReport] =
 *  AutoJson.macroDefault[OrganizationArtifactReport]
 *implicit val us: AutoJson[UpdateStats] = AutoJson.macroDefault[UpdateStats]
 *implicit val aj: AutoJson[UpdateReport] = AutoJson.macroDefault[UpdateReport]
 *implicit val sj: JsonFormat[UpdateReport] = AutoJson.jsonFormat[UpdateReport]
 */
}
