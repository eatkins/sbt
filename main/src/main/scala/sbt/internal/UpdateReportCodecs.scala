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
import xsbti.Logger
import lmcoursier.definitions.{ Configuration => DConfiguration, Developer => DDeveloper, _ }
import lmcoursier.FallbackDependency
import lmcoursier.credentials.Credentials
import lmcoursier.credentials.FileCredentials
import lmcoursier.credentials.DirectCredentials

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
    implicit val moduleNameFormat: AutoJson[ModuleName] = AutoJson.macroDefault
    implicit val organizationFormat: AutoJson[Organization] = AutoJson.macroDefault
    implicit val moduleFormat: AutoJson[Module] = AutoJson.macroDefault
    implicit val fallbackDependencyFormat: AutoJson[FallbackDependency] = AutoJson.macroDefault
    implicit val moduleMatchersFormat: AutoJson[ModuleMatchers] = AutoJson.macroDefault
    implicit val dateTime: AutoJson[DateTime] = AutoJson.macroDefault
    implicit val cachePolicy: AutoJson[CachePolicy] = new AutoJson[CachePolicy] {
      override def read(unbuilder: JsonUnbuilder): CachePolicy = {
        unbuilder.readString match {
          case "LocalOnly"           => CachePolicy.LocalOnly
          case "LocalOnlyIfValid"    => CachePolicy.LocalOnlyIfValid
          case "LocalUpdate"         => CachePolicy.LocalUpdate
          case "LocalUpdateChanging" => CachePolicy.LocalUpdateChanging
          case "UpdateChanging"      => CachePolicy.UpdateChanging
          case "Update"              => CachePolicy.Update
          case "FetchMissing"        => CachePolicy.FetchMissing
          case "ForceDownload"       => CachePolicy.ForceDownload
          case i                     => throw new IllegalStateException(s"Got invalid cache policy $i")
        }
      }
      override def write(obj: CachePolicy, builder: JsonBuilder): Unit = {
        builder.writeString(obj.toString)
      }
    }

    implicit val reconciliationFormat: AutoJson[Reconciliation] =
      new AutoJson[Reconciliation] {
        override def read(unbuilder: JsonUnbuilder): Reconciliation = {
          unbuilder.readString match {
            case "Relaxed" => Reconciliation.Relaxed
            case "Strict"  => Reconciliation.Strict
            case "SemVer"  => Reconciliation.SemVer
            case _         => Reconciliation.Default
          }
        }
        override def write(obj: Reconciliation, builder: JsonBuilder): Unit = {
          builder.writeString(obj.toString)
        }
      }

    implicit val strictFormat: AutoJson[Strict] = AutoJson.macroDefault
    implicit val cacheLoggerFormat: AutoJson[Option[CacheLogger]] =
      new AutoJson[Option[CacheLogger]] {
        override def read(unbuilder: JsonUnbuilder): Option[CacheLogger] = None
        override def write(obj: Option[CacheLogger], builder: JsonBuilder): Unit = {}
      }
    implicit val fileFormat: AutoJson[FileCredentials] = AutoJson.macroDefault
    implicit val directFormat: AutoJson[DirectCredentials] = AutoJson.macroDefault
    implicit val credentialsFormat: AutoJson[Credentials] = new AutoJson[Credentials] {
      override def read(unbuilder: JsonUnbuilder): Credentials = {
        val isFile = unbuilder.readInt == 0
        if (isFile) fileFormat.read(unbuilder) else directFormat.read(unbuilder)
      }
      override def write(obj: Credentials, builder: JsonBuilder): Unit = {
        obj match {
          case f: FileCredentials =>
            builder.writeInt(0)
            fileFormat.write(f, builder)
          case d: DirectCredentials =>
            builder.writeInt(1)
            directFormat.write(d, builder)
        }
      }
    }

    implicit val authenticationFormat: AutoJson[Authentication] = AutoJson.macroDefault
    implicit val developerFormat: AutoJson[DDeveloper] = AutoJson.macroDefault
    implicit val infoFormat: AutoJson[Info] = AutoJson.macroDefault
    implicit val classifierFormat: AutoJson[Classifier] = AutoJson.macroDefault
    implicit val extensionFormat: AutoJson[Extension] = AutoJson.macroDefault
    implicit val typeFormat: AutoJson[Type] = AutoJson.macroDefault
    implicit val publicationFormat: AutoJson[Publication] = AutoJson.macroDefault
    implicit val dconfigurationFormat: AutoJson[DConfiguration] = AutoJson.macroDefault
    implicit val dependencyFormat: AutoJson[Dependency] = AutoJson.macroDefault
    implicit val projectFormat: AutoJson[Project] = AutoJson.macroDefault
    implicit val resolverFormat: AutoJson[Resolver] = new AutoJson[Resolver] {
      class ResolverImpl(name: String) extends Resolver(name)
      override def read(unbuilder: JsonUnbuilder): Resolver = new ResolverImpl(unbuilder.readString)
      override def write(obj: Resolver, builder: JsonBuilder): Unit = builder.writeString(obj.name)
    }
    implicit val loggerFormat: AutoJson[Option[xsbti.Logger]] = new AutoJson[Option[xsbti.Logger]] {
      override def read(unbuilder: JsonUnbuilder): Option[Logger] = None
      override def write(obj: Option[Logger], builder: JsonBuilder): Unit = {}
    }
    implicit val coursierConfigurationFormat: AutoJson[CoursierConfiguration] =
      AutoJson.macroDefault
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
