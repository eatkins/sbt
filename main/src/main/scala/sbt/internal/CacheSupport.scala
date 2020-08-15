package sbt.internal

import sjsonnew.{ Builder, JsonFormat, Unbuilder, deserializationError }
import java.io.File

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
    private implicit val format: JsonFormat[ScalaInstanceParams] =
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
}
