import Project.Initialize
import Util._
import Dependencies._
import Licensed._
import Scope.ThisScope
import LaunchProguard.{ proguard, Proguard }
import Scripted._
import StringUtilities.normalize
import Sxr.sxr

def commonSettings: Seq[Setting[_]] = Seq(
  organization := "org.scala-sbt",
  version := "0.13.8-SNAPSHOT",
  publishArtifact in packageDoc := false,
  scalaVersion := "2.10.4",
  publishMavenStyle := false,
  componentID := None,
  crossPaths := false,
  resolvers += Resolver.typesafeIvyRepo("releases"),
  concurrentRestrictions in Global += Util.testExclusiveRestriction,
  testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-w", "1"),
  javacOptions in compile ++= Seq("-target", "6", "-source", "6", "-Xlint", "-Xlint:-serial"),
  incOptions := incOptions.value.withNameHashing(true)
)

def minimalSettings: Seq[Setting[_]] =
  commonSettings ++ customCommands ++ Status.settings ++ nightlySettings ++
  Seq(
    crossVersion in update <<= (crossVersion, nightly211) { (cv, n) => if (n) CrossVersion.full else cv },
    resolvers += Resolver.typesafeIvyRepo("releases")
  )

def baseSettings: Seq[Setting[_]] =
  minimalSettings ++ Seq(projectComponent) ++ baseScalacOptions ++ Licensed.settings ++ Formatting.settings

def testedBaseSettings: Seq[Setting[_]] =
  baseSettings ++ testDependencies

lazy val root: Project = (project in file(".")).
  configs(Sxr.sxrConf, Proguard).
  aggregate(nonRoots: _*).
  settings(minimalSettings ++ rootSettings: _*)

/* ** Projproject declarations ** */

// defines the Java interfaces through which the launcher and the launched application communicate
lazy val launchInterfaceProj = (project in launchPath / "interface").
  settings(minimalSettings ++ javaOnlySettings: _*).
  settings(
    name := "Launcher Interface"
  )

// the launcher.  Retrieves, loads, and runs applications based on a configuration file.
lazy val launchProj = (project in launchPath).
  dependsOn(ioProj % "test->test", interfaceProj % Test, launchInterfaceProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Launcher",
    libraryDependencies += ivy,
    compile in Test <<= compile in Test dependsOn (publishLocal in interfaceProj, publishLocal in testSamples, publishLocal in launchInterfaceProj)
  ).
  settings(inConfig(Compile)(Transform.configSettings): _*).
  settings(inConfig(Compile)(Transform.transSourceSettings ++ Seq(
    Transform.inputSourceDirectory <<= (sourceDirectory in crossProj) / "input_sources",
    Transform.sourceProperties := Map("cross.package0" -> "xsbt", "cross.package1" -> "boot")
  )): _*)

// used to test the retrieving and loading of an application: sample app is packaged and published to the local repository
lazy val testSamples = (project in launchPath / "test-sample").
  dependsOn(interfaceProj, launchInterfaceProj).
  settings(baseSettings ++ noPublishSettings: _*).
  settings(
    name := "Test Sample",
    libraryDependencies += scalaCompiler.value
  )

// defines Java structures used across Scala versions, such as the API structures and relationships extracted by
//   the analysis compiler phases and passed back to sbt.  The API structures are defined in a simple
//   format from which Java sources are generated by the datatype generator Projproject
lazy val interfaceProj = (project in file("interface")).
  settings(minimalSettings ++ javaOnlySettings: _*).
  settings(
    name := "Interface",
    projectComponent,
    exportJars := true,
    componentID := Some("xsbti"),
    watchSources <++= apiDefinitions,
    resourceGenerators in Compile <+= (version, resourceManaged, streams, compile in Compile) map generateVersionFile,
    apiDefinitions <<= baseDirectory map { base => (base / "definition") :: (base / "other") :: (base / "type") :: Nil },
    sourceGenerators in Compile <+= (cacheDirectory, apiDefinitions, fullClasspath in Compile in datatypeProj, sourceManaged in Compile, mainClass in datatypeProj in Compile, runner, streams) map generateAPICached    
  )

// defines operations on the API of a source, including determining whether it has changed and converting it to a string
//   and discovery of Projclasses and annotations
lazy val apiProj = (project in compilePath / "api").
  dependsOn(interfaceProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "API"
  )

/* **** Utilities **** */

lazy val controlProj = (project in utilPath / "control").
  settings(baseSettings ++ Util.crossBuild: _*).
  settings(
    name := "Control"
  )

lazy val collectionProj = (project in utilPath / "collection").
  settings(testedBaseSettings ++ Util.keywordsSettings ++ Util.crossBuild: _*).
  settings(
    name := "Collections"
  )

lazy val applyMacroProj = (project in utilPath / "appmacro").
  dependsOn(collectionProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Apply Macro",
    libraryDependencies += scalaCompiler.value
  )

// The API for forking, combining, and doing I/O with system processes
lazy val processProj = (project in utilPath / "process").
  dependsOn(ioProj % "test->test").
  settings(baseSettings: _*).
  settings(
    name := "Process",
    libraryDependencies ++= scalaXml.value
  )

// Path, IO (formerly FileUtilities), NameFilter and other I/O utility classes
lazy val ioProj = (project in utilPath / "io").
  dependsOn(controlProj).
  settings(testedBaseSettings ++ Util.crossBuild: _*).
  settings(
    name := "IO",
    libraryDependencies += { "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test }
  )

// Utilities related to reflection, managing Scala versions, and custom class loaders
lazy val classpathProj = (project in utilPath / "classpath").
  dependsOn(launchInterfaceProj, interfaceProj, ioProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Classpath",
    libraryDependencies += scalaCompiler.value
  )

// Command line-related utilities.
lazy val completeProj = (project in utilPath / "complete").
  dependsOn(collectionProj, controlProj, ioProj).
  settings(testedBaseSettings ++ Util.crossBuild: _*).
  settings(
    name := "Completion",
    libraryDependencies += jline
  )

// logging
lazy val logProj = (project in utilPath / "log").
  dependsOn(interfaceProj, processProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Logging",
    libraryDependencies += jline
  ) 

// Relation
lazy val relationProj = (project in utilPath / "relation").
  dependsOn(interfaceProj, processProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Relation"
  )

// class file reader and analyzer
lazy val classfileProj = (project in utilPath / "classfile").
  dependsOn(ioProj, interfaceProj, logProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Classfile"
  )

// generates immutable or mutable Java data types according to a simple input format
lazy val datatypeProj = (project in utilPath / "datatype").
  dependsOn(ioProj).
  settings(baseSettings: _*).
  settings(
    name := "Datatype Generator"
  )

// cross versioning
lazy val crossProj = (project in utilPath / "cross").
  settings(baseSettings: _*).
  settings(inConfig(Compile)(Transform.crossGenSettings): _*).
  settings(
    name := "Cross"
  )

// A logic with restricted negation as failure for a unique, stable model
lazy val logicProj = (project in utilPath / "logic").
  dependsOn(collectionProj, relationProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Logic"
  )

/* **** Intermediate-level Modules **** */

// Apache Ivy integration
lazy val ivyProj = (project in file("ivy")).
  dependsOn(interfaceProj, launchInterfaceProj, crossProj, logProj % "compile;test->test", ioProj % "compile;test->test", launchProj % "test->test", collectionProj).
  settings(baseSettings: _*).
  settings(
    name := "Ivy",
    libraryDependencies ++= Seq(ivy, jsch, json4sNative, jawnParser, jawnJson4s),
    testExclusive)

// Runner for uniform test interface
lazy val testingProj = (project in file("testing")).
  dependsOn(ioProj, classpathProj, logProj, launchInterfaceProj, testAgentProj).
  settings(baseSettings: _*).
  settings(
    name := "Testing",
    libraryDependencies += testInterface
  )

// Testing agent for running tests in a separate process.
lazy val testAgentProj = (project in file("testing") / "agent").
  settings(minimalSettings: _*).
  settings(
    name := "Test Agent",
    libraryDependencies += testInterface
  )

// Basic task engine
lazy val taskProj = (project in tasksPath).
  dependsOn(controlProj, collectionProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Tasks"
  )

// Standard task system.  This provides map, flatMap, join, and more on top of the basic task model.
lazy val stdTaskProj = (project in tasksPath / "standard").
  dependsOn (taskProj % "compile;test->test", collectionProj, logProj, ioProj, processProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Task System",
    testExclusive
  )

// Persisted caching based on SBinary
lazy val cacheProj = (project in cachePath).
  dependsOn (ioProj, collectionProj).
  settings(baseSettings: _*).
  settings(
    name := "Cache",
    libraryDependencies ++= Seq(sbinary) ++ scalaXml.value
  )

// Builds on cache to provide caching for filesystem-related operations
lazy val trackingProj = (project in cachePath / "tracking").
  dependsOn(cacheProj, ioProj).
  settings(baseSettings: _*).
  settings(
    name := "Tracking"
  )

// Embedded Scala code runner
lazy val runProj = (project in file("run")).
  dependsOn (ioProj, logProj % "compile;test->test", classpathProj, processProj % "compile;test->test").
  settings(testedBaseSettings: _*).
  settings(
    name := "Run"
  )

// Compiler-side interface to compiler that is compiled against the compiler being used either in advance or on the fly.
//   Includes API and Analyzer phases that extract source API and relationships.
lazy val compileInterfaceProj = (project in compilePath / "interface").
  dependsOn(interfaceProj % "compile;test->test", ioProj % "test->test", logProj % "test->test", launchProj % "test->test", apiProj % "test->test").
  settings(baseSettings ++ precompiledSettings: _*).
  settings(
    name := "Compiler Interface",
    exportJars := true,
    // we need to fork because in unit tests we set usejavacp = true which means
    // we are expecting all of our dependencies to be on classpath so Scala compiler
    // can use them while constructing its own classpath for compilation
    fork in Test := true,
    // needed because we fork tests and tests are ran in parallel so we have multiple Scala
    // compiler instances that are memory hungry
    javaOptions in Test += "-Xmx1G",
    artifact in (Compile, packageSrc) := Artifact(srcID).copy(configurations = Compile :: Nil).extra("e:component" -> srcID)
  )

lazy val precompiled282 = precompiled("2.8.2")
lazy val precompiled292 = precompiled("2.9.2")
lazy val precompiled293 = precompiled("2.9.3")

// Implements the core functionality of detecting and propagating changes incrementally.
//   Defines the data structures for representing file fingerprints and relationships and the overall source analysis
lazy val compileIncrementalProj = (project in compilePath / "inc").
  dependsOn (apiProj, ioProj, logProj, classpathProj, relationProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Incremental Compiler"
  )

// Persists the incremental data structures using SBinary
lazy val compilePersistProj = (project in compilePath / "persist").
  dependsOn(compileIncrementalProj, apiProj, compileIncrementalProj % "test->test").
  settings(testedBaseSettings: _*).
  settings(
    name := "Persist",
    libraryDependencies += sbinary
  )

// sbt-side interface to compiler.  Calls compiler-side interface reflectively
lazy val compilerProj = (project in compilePath).
  dependsOn(launchInterfaceProj, interfaceProj % "compile;test->test", logProj, ioProj, classpathProj, apiProj, classfileProj,
    logProj % "test->test", launchProj % "test->test").
  settings(testedBaseSettings: _*).
  settings(
    name := "Compile",
    libraryDependencies += scalaCompiler.value % Test,
    unmanagedJars in Test <<= (packageSrc in compileInterfaceProj in Compile).map(x => Seq(x).classpath)
  )

lazy val compilerIntegrationProj = (project in (compilePath / "integration")).
  dependsOn(compileIncrementalProj, compilerProj, compilePersistProj, apiProj, classfileProj).
  settings(baseSettings: _*).
  settings(
    name := "Compiler Integration"
  )

lazy val compilerIvyProj = (project in compilePath / "ivy").
  dependsOn (ivyProj, compilerProj).
  settings(baseSettings: _*).
  settings(
    name := "Compiler Ivy Integration"
  )

lazy val scriptedBaseProj = (project in scriptedPath / "base").
  dependsOn (ioProj, processProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Scripted Framework",
    libraryDependencies ++= scalaParsers.value
  )

lazy val scriptedSbtProj = (project in scriptedPath / "sbt").
  dependsOn (ioProj, logProj, processProj, scriptedBaseProj, launchInterfaceProj % "provided").
  settings(baseSettings: _*).
  settings(
    name := "Scripted sbt"
  )

lazy val scriptedPluginProj = (project in scriptedPath / "plugin").
  dependsOn (sbtProj, classpathProj).
  settings(baseSettings: _*).
  settings(
    name := "Scripted Plugin"
  )

// Implementation and support code for defining actions.
lazy val actionsProj = (project in mainPath / "actions").
  dependsOn (classpathProj, completeProj, apiProj, compilerIntegrationProj, compilerIvyProj,
    interfaceProj, ioProj, ivyProj, logProj, processProj, runProj, relationProj, stdTaskProj,
    taskProj, trackingProj, testingProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Actions"
  )

// General command support and core commands not specific to a build system
lazy val commandProj = (project in mainPath / "command").
  dependsOn(interfaceProj, ioProj, launchInterfaceProj, logProj, completeProj, classpathProj, crossProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Command"
  )

// Fixes scope=Scope for Setting (core defined in collectionProj) to define the settings system used in build definitions
lazy val mainSettingsProj = (project in mainPath / "settings").
  dependsOn (applyMacroProj, interfaceProj, ivyProj, relationProj, logProj, ioProj, commandProj,
    completeProj, classpathProj, stdTaskProj, processProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Main Settings",
    libraryDependencies += sbinary
  )

// The main integration project for sbt.  It brings all of the Projsystems together, configures them, and provides for overriding conventions.
lazy val mainProj = (project in mainPath).
  dependsOn (actionsProj, mainSettingsProj, interfaceProj, ioProj, ivyProj, launchInterfaceProj, logProj, logicProj, processProj, runProj, commandProj).
  settings(testedBaseSettings: _*).
  settings(
    name := "Main",
    libraryDependencies ++= scalaXml.value
  )

// Strictly for bringing implicits and aliases from subsystems into the top-level sbt namespace through a single package object
//  technically, we need a dependency on all of mainProj's dependencies, but we don't do that since this is strictly an integration project
//  with the sole purpose of providing certain identifiers without qualification (with a package object)
lazy val sbtProj = (project in sbtPath).
  dependsOn(mainProj, compileInterfaceProj, precompiled282, precompiled292, precompiled293, scriptedSbtProj % "test->test").
  settings(baseSettings: _*).
  settings(
    name := "sbt",
    normalizedName := "sbt"
  )

def scriptedTask: Initialize[InputTask[Unit]] = InputTask(scriptedSource(dir => (s: State) => scriptedParser(dir))) { result =>
  (proguard in Proguard, fullClasspath in scriptedSbtProj in Test, scalaInstance in scriptedSbtProj, publishAll, scriptedSource, result) map {
    (launcher, scriptedSbtClasspath, scriptedSbtInstance, _, sourcePath, args) =>
      doScripted(launcher, scriptedSbtClasspath, scriptedSbtInstance, sourcePath, args)
  }
}

def scriptedUnpublishedTask: Initialize[InputTask[Unit]] = InputTask(scriptedSource(dir => (s: State) => scriptedParser(dir))) { result =>
  (proguard in Proguard, fullClasspath in scriptedSbtProj in Test, scalaInstance in scriptedSbtProj, scriptedSource, result) map doScripted
}

lazy val publishAll = TaskKey[Unit]("publish-all")
lazy val publishLauncher = TaskKey[Unit]("publish-launcher")

lazy val myProvided = config("provided") intransitive

def allProjects = Seq(launchInterfaceProj, launchProj, testSamples, interfaceProj, apiProj,
  controlProj, collectionProj, applyMacroProj, processProj, ioProj, classpathProj, completeProj,
  logProj, relationProj, classfileProj, datatypeProj, crossProj, logicProj, ivyProj,
  testingProj, testAgentProj, taskProj, stdTaskProj, cacheProj, trackingProj, runProj,
  compileInterfaceProj, compileIncrementalProj, compilePersistProj, compilerProj,
  compilerIntegrationProj, compilerIvyProj,
  scriptedBaseProj, scriptedSbtProj, scriptedPluginProj,
  actionsProj, commandProj, mainSettingsProj, mainProj, sbtProj)
def projectsWithMyProvided = allProjects.map(p => p.copy(configurations = (p.configurations.filter(_ != Provided)) :+ myProvided))
lazy val nonRoots = projectsWithMyProvided.map(p => LocalProject(p.id))

def deepTasks[T](scoped: TaskKey[Seq[T]]): Initialize[Task[Seq[T]]] = deep(scoped.task) { _.join.map(_.flatten.distinct) }
def deep[T](scoped: SettingKey[T]): Initialize[Seq[T]] =
  Util.inAllProjects(projectsWithMyProvided filterNot Set(root, sbtProj, scriptedBaseProj, scriptedSbtProj, scriptedPluginProj) map { p =>
    LocalProject(p.id) }, scoped)

def releaseSettings = Release.settings(nonRoots, proguard in Proguard)
def rootSettings = releaseSettings ++ fullDocSettings ++ LaunchProguard.settings ++ LaunchProguard.specific(launchProj) ++
  Util.publishPomSettings ++ otherRootSettings ++ proguardedLauncherSettings ++ Formatting.sbtFilesSettings ++
  Transform.conscriptSettings(launchProj)
def otherRootSettings = Seq(
  Scripted.scripted <<= scriptedTask,
  Scripted.scriptedUnpublished <<= scriptedUnpublishedTask,
  Scripted.scriptedSource <<= (sourceDirectory in sbtProj) / "sbt-test",
  publishAll <<= inAll(nonRoots, publishLocal.task),
  publishAll <<= (publishAll, publishLocal).map((x, y) => ()) // publish all normal deps as well as the sbt-launch jar
)
def fullDocSettings = Util.baseScalacOptions ++ Docs.settings ++ Sxr.settings ++ Seq(
  scalacOptions += "-Ymacro-no-expand", // for both sxr and doc
  sources in sxr <<= deepTasks(sources in Compile), //sxr
  sources in (Compile, doc) <<= sources in sxr, // doc
  Sxr.sourceDirectories <<= deep(sourceDirectories in Compile).map(_.flatten), // to properly relativize the source paths
  fullClasspath in sxr <<= (externalDependencyClasspath in Compile in sbtProj),
  dependencyClasspath in (Compile, doc) <<= fullClasspath in sxr
)

// the launcher is published with metadata so that the scripted plugin can pull it in
// being proguarded, it shouldn't ever be on a classpath with other jars, however
def proguardedLauncherSettings = Seq(
  publishArtifact in packageSrc := false,
  moduleName := "sbt-launch",
  autoScalaLibrary := false,
  description := "sbt application launcher",
  publishLauncher <<= Release.deployLauncher,
  packageBin in Compile <<= proguard in Proguard
)

/* Nested Projproject paths */
def sbtPath    = file("sbt")
def cachePath  = file("cache")
def tasksPath  = file("tasks")
def launchPath = file("launch")
def utilPath   = file("util")
def compilePath = file("compile")
def mainPath   = file("main")

def precompiledSettings = Seq(
  artifact in packageBin <<= (appConfiguration, scalaVersion) { (app, sv) =>
    val launcher = app.provider.scalaProvider.launcher
    val bincID = binID + "_" + ScalaInstance(sv, launcher).actualVersion
    Artifact(binID) extra ("e:component" -> bincID)
  },
  target <<= (target, scalaVersion) { (base, sv) => base / ("precompiled_" + sv) },
  scalacOptions := Nil,
  ivyScala ~= { _.map(_.copy(checkExplicit = false, overrideScalaVersion = false)) },
  exportedProducts in Compile := Nil,
  libraryDependencies += scalaCompiler.value % "provided"
)

def precompiled(scalav: String): Project = Project(id = normalize("Precompiled " + scalav.replace('.', '_')), base = compilePath / "interface").
  dependsOn(interfaceProj).
  settings(baseSettings ++ precompiledSettings: _*).
  settings(
    name := "Precompiled " + scalav.replace('.', '_'),
    scalaHome := None,
    scalaVersion <<= (scalaVersion in ThisBuild) { sbtScalaV =>
      assert(sbtScalaV != scalav, "Precompiled compiler interface cannot have the same Scala version (" + scalav + ") as sbt.")
      scalav
    },
    // we disable compiling and running tests in precompiled Projprojects of compiler interface
    // so we do not need to worry about cross-versioning testing dependencies
    sources in Test := Nil
  )
