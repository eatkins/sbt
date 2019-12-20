package sbt.client

import java.io.File

import sbt.internal.client.NetworkClient

object Client {
  def main(args: Array[String]): Unit = {
    new NetworkClient(new File("").getCanonicalFile, Nil)
    ()
  }
}
