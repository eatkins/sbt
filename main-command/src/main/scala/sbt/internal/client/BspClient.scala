/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.client

import java.io.{ File, InputStream, OutputStream }
import java.net.Socket

import scala.sys.process.Process
import scala.util.control.NonFatal

import sbt.Exit
import sbt.io.syntax._
import sbt.protocol.ClientSocket

class BspClient private (sbtServer: Socket) {
  private val lock = new AnyRef
  private var terminated = false

  private def transferTo(input: InputStream, output: OutputStream): Thread = {
    val thread = new Thread {
      override def run(): Unit = {
        val buffer = Array.ofDim[Byte](1024)
        try {
          while (!terminated) {
            val size = input.read(buffer)
            if (size == -1) {
              terminated = true
            } else {
              output.write(buffer, 0, size)
              output.flush()
            }
          }
          input.close()
          output.close()
        } catch {
          case NonFatal(_) => ()
        } finally {
          lock.synchronized {
            terminated = true
            lock.notify()
          }
        }
      }
    }
    thread.setDaemon(true)
    thread
  }

  private def run(): Exit = {
    try {
      transferTo(sbtServer.getInputStream, System.out).start()
      transferTo(System.in, sbtServer.getOutputStream).start()

      lock.synchronized {
        while (!terminated) lock.wait()
      }

      Exit(0)
    } catch {
      case NonFatal(_) => Exit(1)
    }
  }
}

object BspClient {
  def run(configuration: xsbti.AppConfiguration): Exit = {
    val baseDirectory = configuration.baseDirectory
    val portFile = baseDirectory / "project" / "target" / "active.json"
    try {
      if (!portFile.exists) {
        forkServer(baseDirectory, portFile)
      }
      val (socket, _) = ClientSocket.socket(portFile)
      new BspClient(socket).run()
    } catch {
      case NonFatal(_) => Exit(1)
    }
  }

  /**
   * Forks another instance of sbt in the background.
   * This instance must be shutdown explicitly via `sbt -client shutdown`
   */
  def forkServer(baseDirectory: File, portfile: File): Unit = {
    val args = List[String]()
    val launchOpts = List("-Xms2048M", "-Xmx2048M", "-Xss2M")

    val launcherJarString = sys.props.get("java.class.path") match {
      case Some(cp) =>
        cp.split(File.pathSeparator)
          .headOption
          .getOrElse(sys.error("launcher JAR classpath not found"))
      case _ => sys.error("property java.class.path expected")
    }

    val cmd = "java" :: launchOpts ::: "-jar" :: launcherJarString :: args
    val process = Process(cmd, baseDirectory).run()

    while (process.isAlive() && !portfile.exists) Thread.sleep(100)

    if (!process.isAlive()) sys.error("sbt server exited")
  }
}
