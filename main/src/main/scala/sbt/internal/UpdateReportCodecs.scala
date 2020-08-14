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
import lmcoursier.CoursierConfiguration

@silent
object UpdateReportCodecs {
  private[sbt] type UpdateKey = (Long, ModuleSettings, UpdateConfiguration, CoursierConfiguration)

  implicit val inclExclRule: AutoJson[InclExclRule] = AutoJson.macroDefault
  implicit val updateLogging: AutoJson[UpdateLogging] = new AutoJson[UpdateLogging] {
    override def read(unbuilder: JsonUnbuilder): UpdateLogging = UpdateLogging.Full
    override def write(obj: UpdateLogging, builder: JsonBuilder): Unit = {}
  }
  implicit val configRef: AutoJson[ConfigRef] = AutoJson.macroDefault
  implicit val checkSum: AutoJson[Checksum] = AutoJson.macroDefault
  implicit val artifact: AutoJson[Artifact] = AutoJson.macroDefault
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
  implicit val configuration: AutoJson[Configuration] = AutoJson.macroDefault
  implicit val scalaModuleInfo = AutoJson.macroDefault[ScalaModuleInfo]
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
  implicit val retrieveConfiguration: AutoJson[RetrieveConfiguration] = AutoJson.macroDefault
  implicit val artifactTypeFilter: AutoJson[ArtifactTypeFilter] = AutoJson.macroDefault
  implicit val updateConfiguration: AutoJson[UpdateConfiguration] = AutoJson.macroDefault
  implicit val updateKeyFormat: AutoJson[UpdateKey] = {
    import LibraryManagementCodec._
    implicit val _: AutoJson[CoursierConfiguration] = new AutoJson[CoursierConfiguration] {
      override def read(unbuilder: JsonUnbuilder): CoursierConfiguration = ???
      override def write(obj: CoursierConfiguration, builder: JsonBuilder): Unit = {}
    }
    implicitly[AutoJson[UpdateKey]]
  }
  implicit val updateKeyJsonFormat: JsonFormat[UpdateKey] = AutoJson.jsonFormat
  implicit val moduleID: AutoJson[ModuleID] = AutoJson.macroDefault
  implicit val moduleIDJsonFormat: JsonFormat[ModuleID] = AutoJson.jsonFormat
  implicit val caller: AutoJson[Caller] = AutoJson.macroDefault
  implicit val moduleReport: AutoJson[ModuleReport] = AutoJson.macroDefault
  implicit val oar: AutoJson[OrganizationArtifactReport] = AutoJson.macroDefault
  implicit val cfj: AutoJson[ConfigurationReport] = AutoJson.macroDefault
  implicit val us: AutoJson[UpdateStats] = AutoJson.macroDefault
  implicit val updateReportFormat: AutoJson[UpdateReport] = AutoJson.macroDefault
  implicit val jf: JsonFormat[UpdateReport] = AutoJson.jsonFormat

}
