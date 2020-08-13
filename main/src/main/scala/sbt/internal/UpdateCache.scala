/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal

import java.io.File
import sbt.internal.util.AutoJson

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
    implicit val format: AutoJson[ScalaInstanceParams] = AutoJson.macroDefault
  }
}
