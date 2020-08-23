/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.test

import java.io.File

import sbt.Def.{ Setting, inputKey, settingKey, taskKey }
import sbt.Scope.Global
import sbt.librarymanagement.ModuleID
import sbt.librarymanagement.syntax._
import sbt.{ InputKey, SettingKey, TaskKey }
import sbt.{ LocalProject, ProjectReference, ThisBuild, Zero }

import sjsonnew.BasicJsonProtocol._

object SlashSyntaxTest extends sbt.SlashSyntax {
  final case class Proj(id: String)
  implicit def projToRef(p: Proj): ProjectReference = LocalProject(p.id)

  val projA: Proj = Proj("a")

  val cancelable: SettingKey[Boolean] = settingKey[Boolean]("")
  val console: TaskKey[Unit] = taskKey[Unit]("")
  val libraryDependencies: SettingKey[Seq[ModuleID]] = settingKey[Seq[ModuleID]]("")
  val name: SettingKey[String] = settingKey[String]("")
  val run: InputKey[Unit] = inputKey[Unit]("")
  val scalaVersion: SettingKey[String] = settingKey[String]("")
  val scalacOptions: TaskKey[Seq[String]] = taskKey[Seq[String]]("")

  val foo: TaskKey[Int] = taskKey[Int]("")
  val bar: TaskKey[Int] = taskKey[Int]("")
  val baz: InputKey[Unit] = inputKey[Unit]("")
  val buildInfo: TaskKey[Seq[File]] = taskKey[Seq[File]]("")

  val uTest: ModuleID = "com.lihaoyi" %% "utest" % "0.5.3"

  Seq[Setting[_]](
    Global / cancelable := true,
    ThisBuild / scalaVersion := "2.12.3",
    console / scalacOptions += "-deprecation",
    Compile / console / scalacOptions += "-Ywarn-numeric-widen",
    projA / Compile / console / scalacOptions += "-feature",
    Zero / name := "foo",
    Zero / Zero / name := "foo",
    Zero / Zero / Zero / name := "foo",
    Test / bar := 1,
    Test / foo := (Test / bar).value + 1,
    Compile / foo := {
      (Compile / bar).previous.getOrElse(1)
    },
    Compile / bar := {
      (Compile / foo).previous.getOrElse(2)
    },
    Test / buildInfo := Nil,
    baz := {
      val _ = (Test / buildInfo).taskValue
      (Compile / run).evaluated
    },
    foo := (Test / bar).value + 1,
    libraryDependencies += uTest % Test,
  )
}
