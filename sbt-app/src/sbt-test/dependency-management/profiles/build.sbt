ThisBuild / scalaVersion := "2.11.12"

Compile / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
libraryDependencies += "org.apache.spark" %% "spark-sql" % "1.6.2"
csrMavenProfiles += "hadoop-2.6"
