/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal
package client

import java.io.{ File, IOException, InputStream }
import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import java.util.concurrent.{ ConcurrentHashMap, LinkedBlockingQueue, TimeUnit }

import org.scalasbt.ipcsocket.UnixDomainSocketLibraryProvider
import sbt.internal.client.NetworkClient.Arguments
import sbt.internal.langserver.{ LogMessageParams, MessageType, PublishDiagnosticsParams }
import sbt.internal.protocol._
import sbt.internal.util.{ ConsoleAppender, NetworkReader, Terminal }
import sbt.io.IO
import sbt.io.syntax._
import sbt.protocol._
import sbt.util.Level
import sjsonnew.BasicJsonProtocol._
import sjsonnew.shaded.scalajson.ast.unsafe.JObject
import sjsonnew.support.scalajson.unsafe.Converter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{ Failure, Success }

trait ConsoleInterface {
  def appendLog(level: Level.Value, message: => String): Unit
  def success(msg: String): Unit
}

trait NetworkClientImpl { self =>
  def baseDirectory: File
  def arguments: List[String] = (sbtArguments ++ commandArguments).toList
  def console: ConsoleInterface
  def provider: UnixDomainSocketLibraryProvider
  def sbtArguments: Seq[String]
  def commandArguments: Seq[String]
  val sbtScript: String
  private val status = new AtomicReference("Ready")
  private val lock: AnyRef = new AnyRef {}
  private val running = new AtomicBoolean(true)
  private val pendingExecIds = ListBuffer.empty[String]
  private val pendingCompletions = new ConcurrentHashMap[String, Seq[String] => Unit]

  private def portfile = baseDirectory / "project" / "target" / "active.json"

  lazy val connection: ServerConnection =
    try init()
    catch {
      case _: ConnectionRefusedException =>
        Files.deleteIfExists(portfile.toPath)
        init()
    }

  private[this] val mainThread = Thread.currentThread
  private[this] val stdinBytes = new LinkedBlockingQueue[Int]
  private[this] val stdin: InputStream = new InputStream {
    override def available(): Int = stdinBytes.size
    override def read: Int = stdinBytes.take
  }
  private[this] val inputThread = new RawInputThread
  start()
  private class ConnectionRefusedException(t: Throwable) extends Throwable(t)

  // Open server connection based on the portfile
  def init(): ServerConnection = {
    if (!portfile.exists) {
      forkServer(portfile)
    }
    val (sk, tkn) =
      try ClientSocket.socket(portfile, provider)
      catch { case e: IOException => throw new ConnectionRefusedException(e) }
    val conn = new ServerConnection(sk) {
      override def onNotification(msg: JsonRpcNotificationMessage): Unit = {
        msg.method match {
          case "shutdown" =>
            val log = msg.params match {
              case Some(jvalue) => Converter.fromJson[Boolean](jvalue).getOrElse(true)
              case _            => false
            }
            if (running.compareAndSet(true, false) && log)
              console.appendLog(Level.Info, "Remote server exited. Shutting down.")
            inputThread.close()
            stdinBytes.offer(-1)
            mainThread.interrupt()
          case "readInput" =>
          case _           => self.onNotification(msg)
        }
      }
      override def onRequest(msg: JsonRpcRequestMessage): Unit = self.onRequest(msg)
      override def onResponse(msg: JsonRpcResponseMessage): Unit = self.onResponse(msg)
      override def onShutdown(): Unit = {
        running.set(false)
      }
    }
    // initiate handshake
    val execId = UUID.randomUUID.toString
    val initCommand = InitCommand(tkn, Option(execId), Some(true))
    conn.sendString(Serialization.serializeCommandAsJsonMessage(initCommand))
    conn
  }

  /**
   * Forks another instance of sbt in the background.
   * This instance must be shutdown explicitly via `sbt -client shutdown`
   */
  def forkServer(portfile: File): Unit = {
    console.appendLog(Level.Info, "server was not detected. starting an instance")
    val cmd = sbtScript +: sbtArguments
    val process = new ProcessBuilder(cmd: _*).directory(baseDirectory).start()
    val stdout = process.getInputStream
    val stderr = process.getErrorStream
    @tailrec
    def blockUntilStart(): Unit = {
      while (stdout.available > 0) {
        System.out.write(stdout.read)
      }
      while (stderr.available > 0) {
        System.err.write(stderr.read)
      }
      Thread.sleep(10)
      if (!portfile.exists) blockUntilStart()
      else {
        stdout.close()
        stderr.close()
        process.getOutputStream.close()
      }
    }

    try blockUntilStart()
    catch { case t: Throwable => t.printStackTrace() }
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
          stdinBytes.offer(-1)
          lock.notifyAll()
        }
        ()
      case execId =>
        pendingCompletions.remove(execId) match {
          case null =>
          case completions =>
            completions(msg.result match {
              case Some(o: JObject) =>
                o.value
                  .collectFirst {
                    case i if i.field == "items" =>
                      Converter.fromJson[Seq[String]](i.value).getOrElse(Nil)
                  }
                  .getOrElse(Nil)
              case _ => Nil
            })
        }
    }
  }

  def onNotification(msg: JsonRpcNotificationMessage): Unit = {
    def splitToMessage: Vector[(Level.Value, String)] =
      (msg.method, msg.params) match {
        case ("window/logMessage", Some(json)) =>
//          import sbt.internal.langserver.codec.JsonProtocol._
//          Converter.fromJson[LogMessageParams](json) match {
//            case Success(params) => splitLogMessage(params)
//            case Failure(_)      => Vector()
//          }
          Vector()
        case ("sbt/terminalprops", Some(json)) =>
          Converter.fromJson[String](json) match {
            case Success(id) =>
              val response = TerminalPropertiesResponse.apply(
                width = Terminal.console.getWidth,
                height = Terminal.console.getHeight,
                isAnsiSupported = Terminal.console.isAnsiSupported,
                isColorEnabled = Terminal.console.isColorEnabled,
                isSupershellEnabled = Terminal.console.isSupershellEnabled,
                isEchoEnabled = Terminal.console.isEchoEnabled
              )
              sendCommandResponse("sbt/terminalpropsresponse", response, id)
            case Failure(_) =>
          }
          Vector.empty
        case ("sbt/terminalcap", Some(json)) =>
          import sbt.protocol.codec.JsonProtocol._
          Converter.fromJson[TerminalCapabilitiesQuery](json) match {
            case Success(terminalCapabilitiesQuery) =>
              val response = TerminalCapabilitiesResponse.apply(
                terminalCapabilitiesQuery.id,
                terminalCapabilitiesQuery.boolean.map(Terminal.getBooleanCapability),
                terminalCapabilitiesQuery.numeric.map(Terminal.getNumericCapability),
                terminalCapabilitiesQuery.string.map(Terminal.getStringCapability),
              )
              sendCommandResponse("sbt/terminalcapresponse", response, terminalCapabilitiesQuery.id)
            case Failure(_) =>
          }
          Vector.empty
        case ("systemOut", Some(json)) =>
          Converter.fromJson[Seq[Byte]](json) match {
            case Success(params) =>
              System.out.write(params.toArray)
              System.out.flush()
            case Failure(_) =>
          }
          Vector.empty
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
    val cleaned = arguments.collect { case c if !c.startsWith("-") => c.trim }
    val userCommands = cleaned.takeWhile(_ != "exit")
    @tailrec def loop(limit: Option[Deadline]): Unit = {
      val byte: Int = limit match {
        case Some(d) => stdinBytes.poll((d - Deadline.now).toMillis, TimeUnit.MILLISECONDS)
        case _       => stdin.read
      }
      byte match {
        case -1 =>
        case byte =>
          sendJson("inputStream", byte.toString)
          if (running.get && !limit.fold(false)(_.isOverdue)) loop(limit)
      }
    }
    if (cleaned.isEmpty) {
      //shell()
      connection.sendString(Serialization.serializeCommandAsJsonMessage(Attach()))
      try loop(None)
      catch { case _: InterruptedException => }
    } else {
      batchExecute(userCommands, loop)
    }
    System.exit(0)
  }

  def batchExecute(userCommands: List[String], wait: Option[Deadline] => Unit): Unit = {
    userCommands foreach { cmd =>
      println("> " + cmd)
      if (cmd == "shutdown") sendAndWait("exit", Some(100.millis.fromNow), wait)
      else sendAndWait(cmd, None, wait)
      System.exit(0)
    }
  }

  private def sendAndWait(
      cmd: String,
      limit: Option[Deadline],
      wait: Option[Deadline] => Unit
  ): Unit = {
    val execId = sendExecCommand(cmd)
    while (running.get && pendingExecIds.contains(execId) && limit.fold(true)(!_.isOverdue())) {
      wait(limit)
    }
  }

  def shell(): Unit = shell(_.foreach(d => Thread.sleep((d - Deadline.now).toMillis)))
  def shell(wait: Option[Deadline] => Unit): Unit = {
    val reader =
      new NetworkReader(Terminal.console, (prefix, level) => {
        val execId = sendJson("sbt/completion", s"""{"query":"$prefix","level":$level}""")
        val result = new LinkedBlockingQueue[Seq[String]]()
        pendingCompletions.put(execId, result.put)
        val completions = result.take
        val insert = completions.collect {
          case c if c.startsWith(prefix) => c.substring(prefix.length)
        }
        (insert, completions)
      })
    try {
      while (running.get) {
        reader.readLine("> ", None) match {
          case Some("shutdown") =>
            // `sbt -client shutdown` shuts down the server
            sendAndWait("exit", Some(100.millis.fromNow), wait)
            running.set(false)
          case Some("exit")               => running.set(false)
          case Some(s) if s.trim.nonEmpty => sendAndWait(s, None, wait)
          case _                          => //
        }
      }
    } catch { case _: InterruptedException => running.set(false) }
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
  def sendCommandResponse(method: String, command: EventMessage, id: String): Unit = {
    try {
      val s = new String(Serialization.serializeEventMessage(command))
      val msg =
        s"""{ "jsonrpc": "2.0", "id": "$id", "method": "$method", "params": $s }"""
      connection.sendString(msg)
    } catch {
      case e: IOException =>
        System.err.println(s"Caught exception writing command to server: $e")
        running.set(false)
    }
  }
  def sendJson(method: String, params: String): String = {
    val uuid = UUID.randomUUID.toString
    val msg = s"""{ "jsonrpc": "2.0", "id": "$uuid", "method": "$method", "params": $params }"""
    connection.sendString(msg)
    uuid
  }

  private[this] class RawInputThread extends Thread("sbt-read-input-thread") with AutoCloseable {
    setDaemon(true)
    start()
    val stopped = new AtomicBoolean(false)
    override final def run(): Unit = {
      @tailrec def read(): Unit = System.in.read match {
        case -1 =>
        case b =>
          stdinBytes.offer(b)
          if (!stopped.get()) read()
      }
      try Terminal.console.withRawSystemIn(read())
      catch { case _: InterruptedException => stopped.set(true) }
    }

    override def close(): Unit = {
      stopped.set(true)
      RawInputThread.this.interrupt()
    }
  }
}
class NetworkClient(
    configuration: xsbti.AppConfiguration,
    override val sbtArguments: Seq[String],
    override val commandArguments: Seq[String],
    override val sbtScript: String,
    override val provider: UnixDomainSocketLibraryProvider
) extends {
  override val console: ConsoleInterface = {
    val appender = ConsoleAppender("thin")
    new ConsoleInterface {
      override def appendLog(level: Level.Value, message: => String): Unit =
        appender.appendLog(level, message)
      override def success(msg: String): Unit = appender.success(msg)
    }
  }
} with NetworkClientImpl {
  def this(configuration: xsbti.AppConfiguration, arguments: Arguments) =
    this(
      configuration,
      arguments.sbtArguments,
      arguments.commandArguments,
      arguments.sbtScript,
      arguments.provider
    )
  def this(configuration: xsbti.AppConfiguration, args: List[String]) =
    this(configuration, NetworkClient.parseArgs(args.toArray))
  override def baseDirectory: File = configuration.baseDirectory
}

class SimpleClient(
    override val baseDirectory: File,
    val sbtArguments: Seq[String],
    val commandArguments: Seq[String],
    override val sbtScript: String,
    override val provider: UnixDomainSocketLibraryProvider
) extends {
  override val console: ConsoleInterface = new ConsoleInterface {
    import scala.Console.{ GREEN, RED, RESET, YELLOW }
    override def appendLog(level: Level.Value, message: => String): Unit = {
      val prefix = level match {
        case Level.Error => s"[$RED$level$RESET]"
        case Level.Warn  => s"[$YELLOW$level$RESET]"
        case _           => s"[$RESET$level$RESET]"
      }
      println(s"$prefix $message")
    }

    override def success(msg: String): Unit = println(s"[${GREEN}success$RESET] $msg")
  }
} with NetworkClientImpl
object SimpleClient {
  def main(args: Array[String]): Unit = {
    apply(args)
    ()
  }
  def apply(args: Array[String]): SimpleClient = {
    val arguments = NetworkClient.parseArgs(args)
    new SimpleClient(
      arguments.baseDirectory,
      arguments.sbtArguments,
      arguments.commandArguments,
      arguments.sbtScript,
      arguments.provider
    )
  }
}

object NetworkClient {
  private[client] class Arguments(
      val baseDirectory: File,
      val sbtArguments: Seq[String],
      val commandArguments: Seq[String],
      val sbtScript: String,
      val provider: UnixDomainSocketLibraryProvider
  )
  private[client] def parseArgs(args: Array[String]): Arguments = {
    var i = 0
    var jni = false
    var sbtScript = "sbt"
    val commandArgs = new mutable.ArrayBuffer[String]
    val sbtArguments = new mutable.ArrayBuffer[String]
    var pwd: Option[File] = None
    while (i < args.length) {
      args(i) match {
        case "--jni" => jni = true
        case a if a.startsWith("--pwd=") =>
          pwd = a.split("--pwd=").lastOption.map(new File(_).getCanonicalFile)
        case a if a.startsWith("--sbt-script=") =>
          sbtScript = a.split("--sbt-script=").lastOption.getOrElse(sbtScript)
        case a if !a.startsWith("-") => commandArgs += a
        case a =>
          if (a.startsWith("-Dsbt.")) {
            a.drop(2).split("=") match {
              case Array(key, value) => System.setProperty(key, value)
              case _                 =>
            }
          }
          sbtArguments += a
      }
      i += 1
    }
    System.getProperties.forEach((k, v) => println(s"$k -> $v"))
    val file = pwd.getOrElse(new File("").getCanonicalFile)
    val provider =
      if (jni) UnixDomainSocketLibraryProvider.jni else UnixDomainSocketLibraryProvider.jna
    new Arguments(file, sbtArguments, commandArgs, sbtScript, provider)
  }
  def run(configuration: xsbti.AppConfiguration, arguments: List[String]): Unit =
    try {
      val args = parseArgs(arguments.toArray)
      new NetworkClient(
        configuration,
        args.sbtArguments,
        args.commandArguments,
        args.sbtScript,
        args.provider
      )
      ()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
}
