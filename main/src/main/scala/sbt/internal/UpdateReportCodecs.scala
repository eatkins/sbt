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
import com.github.ghik.silencer.silent

@silent
object UpdateReportCodecs {

  implicit val updateLogging: AutoJson[UpdateLogging] = new AutoJson[UpdateLogging] {
    override def read(unbuilder: JsonUnbuilder): UpdateLogging = UpdateLogging.Full
    override def write(obj: UpdateLogging, builder: JsonBuilder): Unit = {}
  }
  implicit val artifact: AutoJson[Artifact] = AutoJson.macroDefault
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
  implicit val logicalClock: AutoJson[LogicalClock] = new AutoJson[LogicalClock] {
    override def read(unbuilder: JsonUnbuilder): LogicalClock = LogicalClock.unknown
    override def write(obj: LogicalClock, builder: JsonBuilder): Unit = {}
  }
  val smi = AutoJson.macroDefault[ScalaModuleInfo]
  implicit val moduleSettings: AutoJson[ModuleSettings] = new AutoJson[ModuleSettings] {
    class ModuleSettings0(
        override val validate: Boolean,
        override val scalaModuleInfo: Option[sbt.librarymanagement.ScalaModuleInfo]
    ) extends ModuleSettings
    implicit val smiFormat = implicitly[AutoJson[Option[ScalaModuleInfo]]]
    override def read(unbuilder: JsonUnbuilder): ModuleSettings = {
      new ModuleSettings0(unbuilder.readBoolean, smiFormat.read(unbuilder))
    }
    override def write(obj: ModuleSettings, builder: JsonBuilder): Unit = {
      builder.writeBoolean(obj.validate)
      smiFormat.write(obj.scalaModuleInfo, builder)
    }
  }
  implicit val ui: AutoJson[sbt.internal.LibraryManagement.UpdateInputs] = AutoJson.macroDefault
  implicit val uc: AutoJson[UpdateConfiguration] = AutoJson.macroDefault
  implicit val configuration: AutoJson[Configuration] = AutoJson.macroDefault
  implicit val jfui: JsonFormat[sbt.internal.LibraryManagement.UpdateInputs] = AutoJson.jsonFormat
  implicit val cr: AutoJson[ConfigRef] = AutoJson.macroDefault
  implicit val moduleID: AutoJson[ModuleID] = AutoJson.macroDefault
  implicit val jfmi: JsonFormat[ModuleID] = AutoJson.jsonFormat
  implicit val iel: AutoJson[InclExclRule] = AutoJson.macroDefault
  implicit val moduleReport: AutoJson[ModuleReport] = AutoJson.macroDefault
  implicit val oar: AutoJson[OrganizationArtifactReport] = AutoJson.macroDefault
  implicit val cfj: AutoJson[ConfigurationReport] = AutoJson.macroDefault
  implicit val jf: JsonFormat[UpdateReport] = AutoJson.jsonFormat
}
