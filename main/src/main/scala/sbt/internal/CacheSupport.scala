/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import sjsonnew.{ Builder, JsonFormat, Unbuilder, deserializationError, serializationError }
import java.io.File
import sbt.librarymanagement.Configuration
import sbt.internal.util.AttributeMap
import sbt.internal.util.Attributed

private[sbt] object CacheSupport {
  private[sbt] class ScalaInstanceParams(
      val version: String,
      val allJars: Seq[File],
      val libraryJars: Seq[File],
      val compilerJar: File,
      val scalaHome: Option[File]
  ) {
    override def equals(o: Any): Boolean = o match {
      case that: ScalaInstanceParams =>
        this.hashCode == that.hashCode &&
          (this.version == that.version) &&
          (this.allJars == that.allJars) &&
          (this.libraryJars == that.libraryJars) &&
          (this.compilerJar == that.compilerJar) &&
          (this.scalaHome == that.scalaHome)
      case _ => false
    }
    override lazy val hashCode: Int =
      (37 * (37 * (37 * (37 * (17 + version.##) ^ allJars.##) ^ libraryJars.##) ^ compilerJar.##) ^ scalaHome.##)
  }
  private[sbt] object ScalaInstanceParams {
    implicit val format: JsonFormat[ScalaInstanceParams] =
      new JsonFormat[ScalaInstanceParams] {
        override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): ScalaInstanceParams =
          jsOpt match {
            case Some(j) =>
              unbuilder.beginArray(j)
              val version = unbuilder.readString(j)
              val allJarsLength = unbuilder.readInt(j)
              val allJars = (0 until allJarsLength).map(_ => new File(unbuilder.readString(j)))
              val libraryJarsLength = unbuilder.readInt(j)
              val libraryJars =
                (0 until libraryJarsLength).map(_ => new File(unbuilder.readString(j)))
              val compilerJar = new File(unbuilder.readString(j))
              val scalaHome =
                if (unbuilder.readBoolean(j)) Some(new File(unbuilder.readString(j))) else None
              unbuilder.endArray()
              new ScalaInstanceParams(version, allJars, libraryJars, compilerJar, scalaHome)
            case _ => deserializationError("Couldn't deserialize ScalaInstanceParams")

          }
        override def write[J](obj: ScalaInstanceParams, builder: Builder[J]): Unit = {
          builder.beginArray
          builder.writeString(obj.version)
          builder.writeInt(obj.allJars.length)
          obj.allJars.foreach(f => builder.writeString(f.toString))
          builder.writeInt(obj.libraryJars.length)
          obj.libraryJars.foreach(f => builder.writeString(f.toString))
          builder.writeString(obj.compilerJar.toString)
          obj.scalaHome match {
            case Some(h) =>
              builder.writeBoolean(true)
              builder.writeString(h.toString)
            case _ => builder.writeBoolean(false)
          }
        }
      }
  }
  object Formats {
    implicit val configurationFormat: JsonFormat[Configuration] = new JsonFormat[Configuration] {
      override def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): Configuration =
        jsOpt match {
          case Some(j) =>
            def impl(): Configuration = {
              unbuilder.beginArray(j)
              val id = unbuilder.readString(j)
              val name = unbuilder.readString(j)
              val description = unbuilder.readString(j)
              val isPublic = unbuilder.readBoolean(j)
              val transitive = unbuilder.readBoolean(j)
              val config =
                Configuration.of(id, name, description, isPublic, Vector.empty, transitive)
              val extendsConfigsLength = unbuilder.readInt(j)
              val extendsConfigs = (0 until extendsConfigsLength).map(_ => impl()).toVector
              unbuilder.endArray()
              config.withExtendsConfigs(extendsConfigs)
            }
            impl()
          case _ => deserializationError("couldn't deserialize ScalaInstanceParams")

        }
      override def write[J](obj: Configuration, builder: Builder[J]): Unit = {
        def impl(conf: Configuration): Unit = {
          builder.beginArray()
          builder.writeString(conf.id)
          builder.writeString(conf.name)
          builder.writeString(conf.description)
          builder.writeBoolean(conf.isPublic)
          builder.writeBoolean(conf.transitive)
          builder.writeInt(conf.extendsConfigs.length)
          conf.extendsConfigs.foreach(impl)
          builder.endArray()
        }
        impl(obj)
      }
    }
    private implicit val mapFormat: JsonFormat[AttributeMap] = new JsonFormat[AttributeMap] {
      import sbt.librarymanagement.LibraryManagementCodec.{
        ArtifactFormat,
        ConfigurationFormat,
        ModuleIDFormat
      }
      def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): AttributeMap = jsOpt match {
        case o @ Some(j) =>
          unbuilder.beginArray(j)
          val art = ArtifactFormat.read(Some(unbuilder.nextElement), unbuilder)
          val module = ModuleIDFormat.read(Some(unbuilder.nextElement), unbuilder)
          val config = ConfigurationFormat.read(Some(unbuilder.nextElement), unbuilder)
          val result = AttributeMap.empty
            .put(Keys.artifact.key, art)
            .put(Keys.moduleID.key, module)
            .put(Keys.configuration.key, config)
          unbuilder.endArray()
          result
        case None => deserializationError("couldn't deserialize AttributeMap")
      }
      def write[J](obj: AttributeMap, builder: Builder[J]): Unit = {
        builder.beginArray()
        obj.get(Keys.artifact.key) match {
          case Some(a) => ArtifactFormat.write(a, builder)
          case _       => serializationError("no artifact in AttributeMap")
        }
        obj.get(Keys.moduleID.key) match {
          case Some(m) => ModuleIDFormat.write(m, builder)
          case _       => serializationError("no module id in AttributeMap")
        }
        obj.get(Keys.configuration.key) match {
          case Some(c) => ConfigurationFormat.write(c, builder)
          case _       => serializationError("no configuration id in AttributeMap")
        }
        builder.endArray()
      }
    }
    implicit val classpathFormat: JsonFormat[Def.Classpath] = new JsonFormat[Def.Classpath] {
      def read[J](jsOpt: Option[J], unbuilder: Unbuilder[J]): Def.Classpath = jsOpt match {
        case o @ Some(j) =>
          unbuilder.beginArray(j)
          val size = unbuilder.readInt(unbuilder.nextElement)
          val result = (0 until size).map { _ =>
            val file = new File(unbuilder.readString(unbuilder.nextElement))
            Attributed(file)(mapFormat.read(Some(unbuilder.nextElement), unbuilder))
          }
          unbuilder.endArray()
          result
        case None => deserializationError("couldn't deserialize classpath")
      }
      def write[J](obj: Def.Classpath, builder: Builder[J]): Unit = {
        builder.beginArray()
        builder.writeInt(obj.size)
        obj.foreach { af =>
          builder.writeString(af.data.toString)
          mapFormat.write(af.metadata, builder)
        }
        builder.endArray()
      }
    }
  }
}
