import java.nio.file.{ Files, Paths }

object A {
  def main(args: Array[String]): Unit = {
    val path = Paths.get(args.head)
    Files.write(path, "start".getBytes)
    System.in.read
    Files.write(path, "finish".getBytes)
  }
}
