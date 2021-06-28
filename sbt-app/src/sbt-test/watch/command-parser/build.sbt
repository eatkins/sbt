import Build._

organization := "sbt"

name := "scripted-watch-parser"

setStringValue := setStringValueImpl.evaluated

checkStringValue := checkStringValueImpl.evaluated

setStringValue / watchTriggers := baseDirectory.value.toGlob / "string.txt" :: Nil

watchOnFileInputEvent := { (_, _) => sbt.nio.Watch.CancelWatch }
