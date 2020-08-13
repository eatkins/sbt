/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.io.File
import sbt.internal.util.AutoJson
import sjsonnew.JsonFormat
import sbt.internal.util.AttributeMap
import sbt.internal.util.JsonUnbuilder
import sbt.internal.util.JsonBuilder
import sbt.internal.util.Attributed

object UpdateCache {
  class ScalaInstanceParams(
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
  object ScalaInstanceParams {
    private implicit val format: AutoJson[ScalaInstanceParams] = AutoJson.macroDefault
    implicit val jf: JsonFormat[ScalaInstanceParams] = AutoJson.jsonFormat[ScalaInstanceParams]
  }
  object AttributedFormats {
    import UpdateReportCodecs._
    implicit val mapFormat: AutoJson[AttributeMap] = new AutoJson[AttributeMap] {
      def read(unbuilder: JsonUnbuilder): AttributeMap = {
        val result = AttributeMap.empty
        val art = artifact.read(unbuilder)
        val module = moduleID.read(unbuilder)
        val config = configuration.read(unbuilder)
        result
          .put(Keys.artifact.key, art)
          .put(Keys.moduleID.key, module)
          .put(Keys.configuration.key, config)
      }
      def write(obj: AttributeMap, builder: JsonBuilder): Unit = {
        obj.get(Keys.artifact.key) match {
          case Some(a) => artifact.write(a, builder)
          case _       =>
        }
        obj.get(Keys.moduleID.key) match {
          case Some(m) => moduleID.write(m, builder)
          case _       =>
        }
        obj.get(Keys.configuration.key) match {
          case Some(c) => configuration.write(c, builder)
          case _       =>
        }
      }
    }
    implicit val attributedFormat: AutoJson[Attributed[File]] = AutoJson.macroDefault
    implicit val jsonFormat: JsonFormat[Def.Classpath] = AutoJson.jsonFormat[Def.Classpath]
  }
}
