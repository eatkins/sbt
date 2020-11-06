import java.nio.file.{ Files, Paths }

object A {
  def main(args: Array[String]): Unit = {
    val path = Paths.get(args.head)
    Files.write(path, "start".getBytes)
    System.err.println(s"huh wrote start ${System.in}")
    System.err.println(System.in.read)
    Files.write(path, "finish".getBytes)
  }
}
