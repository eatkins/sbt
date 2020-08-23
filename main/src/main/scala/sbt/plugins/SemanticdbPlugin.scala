/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package plugins

import sbt.Keys._
import sbt.Project.inConfig
import sbt.internal.SysProp
import sbt.internal.inc.ScalaInstance
import sbt.librarymanagement.CrossVersion
import sbt.librarymanagement.syntax._

object SemanticdbPlugin extends AutoPlugin {
  override def requires = JvmPlugin
  override def trigger = allRequirements

  override lazy val globalSettings: Seq[Def.Setting[_]] = Seq(
    semanticdbEnabled := SysProp.semanticdb,
    semanticdbIncludeInJar := false,
    semanticdbOptions := List(),
    semanticdbVersion := "4.3.20"
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] = Seq(
    semanticdbCompilerPlugin := {
      val v = semanticdbVersion.value
      ("org.scalameta" % "semanticdb-scalac" % v).cross(CrossVersion.full)
    },
    allDependencies ++= {
      val sdb = semanticdbEnabled.value
      val m = semanticdbCompilerPlugin.value
      val sv = scalaVersion.value
      if (sdb && !ScalaInstance.isDotty(sv)) List(Build0.compilerPlugin(m))
      else Nil
    }
  ) ++ inConfig(Compile)(configurationSettings) ++ inConfig(Test)(configurationSettings)

  lazy val configurationSettings: Seq[Def.Setting[_]] = List(
    scalacOptions := {
      val old = scalacOptions.value
      val sdb = semanticdbEnabled.value
      val sdbOptions = semanticdbOptions.value
      val sv = scalaVersion.value
      if (sdb) {
        (
          old.toVector ++ sdbOptions ++
            (if (ScalaInstance.isDotty(sv)) Some("-Ysemanticdb") else None)
        ).distinct
      } else old
    },
    semanticdbTargetRoot := {
      val in = semanticdbIncludeInJar.value
      if (in) classDirectory.value
      else semanticdbTargetRoot.value
    },
    semanticdbOptions ++= {
      val tr = semanticdbTargetRoot.value
      val sv = scalaVersion.value
      if (ScalaInstance.isDotty(sv)) List("-semanticdb-target", tr.toString)
      else List(s"-P:semanticdb:targetroot:$tr", "-Yrangepos")
    }
  )
}
