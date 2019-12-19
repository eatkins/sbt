/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal
package client

import java.io.{ File, IOException }
import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.{ ArrayBlockingQueue, TimeUnit }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }

import sbt.internal.langserver.{ LogMessageParams, MessageType, PublishDiagnosticsParams }
import sbt.internal.nio.FileTreeRepository
import sbt.internal.protocol._
import sbt.internal.util.{ ConsoleAppender, LineReader }
import sbt.io.IO
import sbt.io.syntax._
import sbt.nio.file.Glob
import sbt.protocol._
import sbt.util.Level
import sjsonnew.support.scalajson.unsafe.Converter

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.sys.process.{ BasicIO, Process, ProcessLogger }
import scala.util.control.NonFatal
import scala.util.{ Failure, Success }

class NetworkClient(configuration: xsbti.AppConfiguration, arguments: List[String]) { self =>
  private val status = new AtomicReference("Ready")
  private val lock: AnyRef = new AnyRef {}
  private val running = new AtomicBoolean(true)
  private val pendingExecIds = ListBuffer.empty[String]

  private val console = ConsoleAppender("thin1")
  private def baseDirectory: File = configuration.baseDirectory
  private def portfile = baseDirectory / "project" / "target" / "active.json"

  lazy val connection: ServerConnection = try init()
  catch {
    case _: ConnectionRefusedException =>
      Files.deleteIfExists(portfile.toPath)
      init()
  }

  private[this] val mainThread = Thread.currentThread
  start()
  private class ConnectionRefusedException(t: Throwable) extends Throwable(t)

  // Open server connection based on the portfile
  def init(): ServerConnection = {
    if (!portfile.exists) {
      forkServer(portfile)
    }
    val (sk, tkn) = try ClientSocket.socket(portfile)
    catch { case e: IOException => throw new ConnectionRefusedException(e) }
    val conn = new ServerConnection(sk) {
      override def onNotification(msg: JsonRpcNotificationMessage): Unit = {
        if (msg.method == "shutdown") {
          println("")
          console.appendLog(Level.Info, "Remote server exited. Shutting down.")
          running.set(false)
          mainThread.interrupt()
        } else self.onNotification(msg)
      }
      override def onRequest(msg: JsonRpcRequestMessage): Unit = self.onRequest(msg)
      override def onResponse(msg: JsonRpcResponseMessage): Unit = self.onResponse(msg)
      override def onShutdown(): Unit = {
        running.set(false)
      }
    }
    // initiate handshake
    val execId = UUID.randomUUID.toString
    val initCommand = InitCommand(tkn, Option(execId))
    conn.sendString(Serialization.serializeCommandAsJsonMessage(initCommand))
    conn
  }

  /**
   * Forks another instance of sbt in the background.
   * This instance must be shutdown explicitly via `sbt -client shutdown`
   */
  def forkServer(portfile: File): Unit = {
    console.appendLog(Level.Info, "server was not detected. starting an instance")
    val args = List[String]()
    val launchOpts = List("-Xms2048M", "-Xmx2048M", "-Xss2M")
    val launcherJarString = sys.props.get("java.class.path") match {
      case Some(cp) =>
        cp.split(File.pathSeparator)
          .toList
          .headOption
          .getOrElse(sys.error("launcher JAR classpath not found"))
      case _ => sys.error("property java.class.path expected")
    }
    val cmd = "java" :: launchOpts ::: "-jar" :: launcherJarString :: args
    // val cmd = "sbt"
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val _ = Process(cmd, baseDirectory).run(io)
    val buffer = new ArrayBlockingQueue[Unit](1)
    @tailrec def waitForPortfile(limit: Deadline, n: Int): Unit =
      if (portfile.exists) {
        console.appendLog(Level.Info, "server found")
      } else {
        if (limit.isOverdue) sys.error(s"timeout. $portfile is not found.")
        else {
          buffer.poll(1, TimeUnit.SECONDS)
          if (n % 10 == 0) console.appendLog(Level.Info, "waiting for the server...")
          waitForPortfile(limit, n + 1)
        }
      }
    val repo = FileTreeRepository.default
    repo.register(Glob(portfile)) match {
      case Left(e)  => throw e
      case Right(o) => o.addObserver(t => if (t.exists) buffer.put(()))
    }
    try waitForPortfile(90.seconds.fromNow, 0)
    finally repo.close()
  }

  /** Called on the response for a returning message. */
  def onReturningReponse(msg: JsonRpcResponseMessage): Unit = {
    def printResponse(): Unit = {
      msg.result match {
        case Some(result) =>
          // ignore result JSON
          console.success("completed")
        case _ =>
          msg.error match {
            case Some(err) =>
              // ignore err details
              console.appendLog(Level.Error, "completed")
            case _ => // ignore
          }
      }
    }
    printResponse()
  }

  def onResponse(msg: JsonRpcResponseMessage): Unit = {
    msg.id foreach {
      case execId if pendingExecIds contains execId =>
        onReturningReponse(msg)
        lock.synchronized {
          pendingExecIds -= execId
          lock.notifyAll()
        }
        ()
      case _ =>
    }
  }

  def onNotification(msg: JsonRpcNotificationMessage): Unit = {
    def splitToMessage: Vector[(Level.Value, String)] =
      (msg.method, msg.params) match {
        case ("window/logMessage", Some(json)) =>
          import sbt.internal.langserver.codec.JsonProtocol._
          Converter.fromJson[LogMessageParams](json) match {
            case Success(params) => splitLogMessage(params)
            case Failure(_)      => Vector()
          }
        case ("textDocument/publishDiagnostics", Some(json)) =>
          import sbt.internal.langserver.codec.JsonProtocol._
          Converter.fromJson[PublishDiagnosticsParams](json) match {
            case Success(params) => splitDiagnostics(params)
            case Failure(_)      => Vector()
          }
        case ("shutdown", Some(_)) => Vector.empty
        case _ =>
          Vector(
            (
              Level.Warn,
              s"unknown event: ${msg.method} " + Serialization.compactPrintJsonOpt(msg.params)
            )
          )
      }
    splitToMessage foreach {
      case (level, msg) => console.appendLog(level, msg)
    }
  }

  def splitLogMessage(params: LogMessageParams): Vector[(Level.Value, String)] = {
    val level = messageTypeToLevel(params.`type`)
    if (level == Level.Debug) Vector()
    else Vector((level, params.message))
  }

  def messageTypeToLevel(severity: Long): Level.Value = {
    severity match {
      case MessageType.Error   => Level.Error
      case MessageType.Warning => Level.Warn
      case MessageType.Info    => Level.Info
      case MessageType.Log     => Level.Debug
    }
  }

  def splitDiagnostics(params: PublishDiagnosticsParams): Vector[(Level.Value, String)] = {
    val uri = new URI(params.uri)
    val f = IO.toFile(uri)

    params.diagnostics map { d =>
      val level = d.severity match {
        case Some(severity) => messageTypeToLevel(severity)
        case _              => Level.Error
      }
      val line = d.range.start.line + 1
      val offset = d.range.start.character + 1
      val msg = s"$f:$line:$offset: ${d.message}"
      (level, msg)
    }
  }

  def onRequest(msg: JsonRpcRequestMessage): Unit = {
    // ignore
  }

  def start(): Unit = {
    console.appendLog(Level.Info, "entering *experimental* thin client - BEEP WHIRR")
    val _ = connection
    val userCommands = arguments filterNot { cmd =>
      cmd.startsWith("-")
    }
    if (userCommands.isEmpty) shell()
    else batchExecute(userCommands)
  }

  def batchExecute(userCommands: List[String]): Unit = {
    userCommands foreach { cmd =>
      println("> " + cmd)
      if (cmd == "shutdown") sendAndWait("exit", Some(100.millis.fromNow))
      else sendAndWait(cmd, None)
    }
  }

  private def sendAndWait(cmd: String, limit: Option[Deadline]): Unit = {
    val execId = sendExecCommand(cmd)
    while (running.get && pendingExecIds.contains(execId) && limit.fold(true)(!_.isOverdue())) {
      limit match {
        case None    => lock.synchronized(lock.wait())
        case Some(l) => lock.synchronized(lock.wait((l - Deadline.now).toMillis))
      }
    }
  }
  private[this] class ReadThread extends Thread("sbt-client-read-thread") with AutoCloseable {
    val reader = LineReader.simple(None, LineReader.HandleCONT, injectThreadSleep = true)
    private[this] val stopped = new AtomicBoolean(false)
    private[this] val queue = new ArrayBlockingQueue[Unit](1)
    private[this] val nextCommand = new ArrayBlockingQueue[Option[String]](1)
    setDaemon(true)
    start()
    @tailrec override final def run(): Unit = {
      try {
        queue.take()
        nextCommand.put(reader.readLine("> ", None))
      } catch { case _: InterruptedException => stopped.set(true) }
      if (!stopped.get()) run()
    }
    def readLine: Option[String] = {
      queue.put(())
      nextCommand.take()
    }
    override def close(): Unit = {
      stopped.set(true)
      interrupt()
    }
  }
  def shell(): Unit = {
    val thread = new ReadThread
    while (running.get) {
      try {
        thread.readLine match {
          case Some("shutdown") =>
            // `sbt -client shutdown` shuts down the server
            sendAndWait("exit", Some(100.millis.fromNow))
            running.set(false)
          case Some("exit") =>
            running.set(false)
          case Some(s) if s.trim.nonEmpty => sendAndWait(s, None)
          case _                          => //
        }
      } catch { case _: InterruptedException => thread.close() }
    }
  }

  def sendExecCommand(commandLine: String): String = {
    val execId = UUID.randomUUID.toString
    sendCommand(ExecCommand(commandLine, execId))
    lock.synchronized {
      pendingExecIds += execId
    }
    execId
  }

  def sendCommand(command: CommandMessage): Unit = {
    try {
      val s = Serialization.serializeCommandAsJsonMessage(command)
      connection.sendString(s)
      lock.synchronized {
        status.set("Processing")
      }
    } catch {
      case e: IOException =>
        System.err.println(s"Caught exception writing command to server: $e")
        running.set(false)
    }
  }
}

object NetworkClient {
  def run(configuration: xsbti.AppConfiguration, arguments: List[String]): Unit =
    try {
      new NetworkClient(configuration, arguments)
      ()
    } catch {
      case NonFatal(e) => println(e.getMessage)
    }
}
