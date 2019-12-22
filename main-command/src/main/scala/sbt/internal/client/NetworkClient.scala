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
import java.util.concurrent.{
  ArrayBlockingQueue,
  ConcurrentHashMap,
  Executors,
  LinkedBlockingQueue,
  TimeUnit
}
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }

import org.scalasbt.ipcsocket.UnixDomainSocketLibraryProvider
import sbt.internal.langserver.{ LogMessageParams, MessageType, PublishDiagnosticsParams }
import sbt.internal.nio.FileTreeRepository
import sbt.internal.protocol._
import sbt.internal.util.{ ConsoleAppender, NetworkReader, Terminal }
import sbt.io.IO
import sbt.io.syntax._
import sbt.nio.file.Glob
import sbt.protocol._
import sbt.util.Level
import sjsonnew.BasicJsonProtocol._
import sjsonnew.shaded.scalajson.ast.unsafe.JObject
import sjsonnew.support.scalajson.unsafe.Converter

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.sys.process.{ BasicIO, Process, ProcessLogger }
import scala.util.control.NonFatal
import scala.util.{ Failure, Success }

trait ConsoleInterface {
  def appendLog(level: Level.Value, message: => String): Unit
  def success(msg: String): Unit
}

trait NetworkClientImpl { self =>
  def baseDirectory: File
  def arguments: List[String]
  def console: ConsoleInterface
  def provider: UnixDomainSocketLibraryProvider
  private val status = new AtomicReference("Ready")
  private val lock: AnyRef = new AnyRef {}
  private val running = new AtomicBoolean(true)
  private val pendingExecIds = ListBuffer.empty[String]
  private val pendingCompletions = new ConcurrentHashMap[String, Seq[String] => Unit]

  private def portfile = baseDirectory / "project" / "target" / "active.json"

  lazy val connection: ServerConnection = try init()
  catch {
    case _: ConnectionRefusedException =>
      Files.deleteIfExists(portfile.toPath)
      init()
  }

  private[this] val mainThread = Thread.currentThread
  private[this] val executor =
    Executors.newSingleThreadExecutor(r => new Thread(r, "sbt-client-read-input-thread"))
  private[this] val stdinBytes = new LinkedBlockingQueue[Byte]
  private[this] val stdinAlive = new AtomicBoolean(true)
  private[this] val reading = new AtomicBoolean(false)
  private[this] def readOne =
    if (reading.compareAndSet(false, true)) executor.submit((() => {
      if (stdinAlive.get) System.in.read match {
        case -1 => stdinAlive.set(false)
        case b  => stdinBytes.put(b.toByte)
      }
      reading.set(false)
    }): Runnable)
  private[this] val stdin: InputStream = new InputStream {
    override def available(): Int = stdinBytes.size
    override def read: Int = {
      if (stdinBytes.isEmpty) readOne
      stdinBytes.take & 0xFF
    }
  }
  private[this] val rawInputThread = new AtomicReference[RawInputThread]
  start()
  private class ConnectionRefusedException(t: Throwable) extends Throwable(t)

  // Open server connection based on the portfile
  def init(): ServerConnection = {
    if (!portfile.exists) {
      forkServer(portfile)
    }
    val (sk, tkn) = try ClientSocket.socket(portfile, provider)
    catch { case e: IOException => throw new ConnectionRefusedException(e) }
    val conn = new ServerConnection(sk) {
      override def onNotification(msg: JsonRpcNotificationMessage): Unit = {
        msg.method match {
          case "shutdown" =>
            console.appendLog(Level.Info, "Remote server exited. Shutting down.")
            running.set(false)
            mainThread.interrupt()
          case "readInput" =>
            rawInputThread.synchronized(rawInputThread.get match {
              case null => rawInputThread.set(new RawInputThread)
              case t    =>
            })
          case _ => self.onNotification(msg)
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
          .find(_.contains("sbt-launch"))
          .getOrElse(
            "/Users/ethanatkins/.ivy2/local/org.scala-sbt/sbt-launch/1.4.0-SNAPSHOT/jars/sbt-launch.jar"
          )
      case _ => sys.error("property java.class.path expected")
    }
    val cmd = "java" :: launchOpts ::: "-jar" :: launcherJarString :: args
    // val cmd = "sbt"
    val io = BasicIO(false, ProcessLogger(x => println(x)))
    val p = Process(cmd, baseDirectory).run(io)
    val buffer = new ArrayBlockingQueue[Unit](1)
    @tailrec def waitForPortfile(limit: Deadline, n: Int): Unit =
      if (portfile.exists) {
        console.appendLog(Level.Info, "server found")
      } else {
        if (limit.isOverdue || !p.isAlive) {
          sys.error(s"timeout. $portfile is not found.")
        } else {
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
          import sbt.internal.langserver.codec.JsonProtocol._
          Converter.fromJson[LogMessageParams](json) match {
            case Success(params) => splitLogMessage(params)
            case Failure(_)      => Vector()
          }
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
    println(msg)
    // ignore
  }

  def start(): Unit = {
    console.appendLog(Level.Info, "entering *experimental* thin client - BEEP WHIRR")
    val _ = connection
    val cleaned = arguments.collect { case c if !c.startsWith("-") => c.trim }
    val userCommands = cleaned.takeWhile(_ != "exit")
    if (cleaned.isEmpty) shell()
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
    Terminal.withRawSystemIn {
      while (running.get && pendingExecIds.contains(execId) && limit.fold(true)(!_.isOverdue())) {
        limit match {
          case None =>
            lock.synchronized(lock.wait)
          case Some(l) => lock.synchronized(lock.wait((l - Deadline.now).toMillis))
        }
      }
    }
  }

  def shell(): Unit = {
    val reader = new NetworkReader(stdin, (prefix, level) => {
      val execId = sendJson("sbt/completion", s"""{"query":"$prefix","level":$level}""")
      val result = new LinkedBlockingQueue[Seq[String]]()
      pendingCompletions.put(execId, result.put)
      val completions = result.take
      val insert = completions.collect {
        case c if c.startsWith(prefix) => c.substring(prefix.length)
      }
      (insert, completions)
    })
    while (running.get) {
      try {
        rawInputThread.synchronized(Option(rawInputThread.getAndSet(null)).foreach(_.close()))
        reader.readLine("> ", None) match {
          case Some("shutdown") =>
            // `sbt -client shutdown` shuts down the server
            sendAndWait("exit", Some(100.millis.fromNow))
            running.set(false)
            executor.shutdownNow()
          case Some("exit") =>
            running.set(false)
            executor.shutdownNow()
          case Some(s) if s.trim.nonEmpty => sendAndWait(s, None)
          case _                          => //
        }
      } catch { case _: InterruptedException => running.set(false) }
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
  def sendJson(method: String, params: String): String = {
    val uuid = UUID.randomUUID.toString
    val msg = s"""{ "jsonrpc": "2.0", "id": "$uuid", "method": "$method", "params": $params }"""
    connection.sendString(msg)
    uuid
  }

  private[this] class RawInputThread extends Thread("sbt-raw-input-thread") with AutoCloseable {
    setDaemon(true)
    start()
    val stopped = new AtomicBoolean(false)
    override final def run(): Unit = {
      try {
        val byte = stdin.read().toByte
        if (!stopped.get) {
          sendJson("inputStream", byte.toString)
          ()
        } else stdinBytes.put(byte)
      } catch {
        case _: InterruptedException => stopped.set(true)
      } finally rawInputThread.set(null)
    }

    override def close(): Unit = {
      stopped.set(true)
      RawInputThread.this.interrupt()
    }
  }
}
class NetworkClient(
    configuration: xsbti.AppConfiguration,
    override val arguments: List[String],
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
  def this(configuration: xsbti.AppConfiguration, args: List[String]) =
    this(configuration, args, UnixDomainSocketLibraryProvider.jna)
  override def baseDirectory: File = configuration.baseDirectory
}

class SimpleClient(
    override val baseDirectory: File,
    val arguments: List[String],
    override val provider: UnixDomainSocketLibraryProvider
) extends {
  override val console: ConsoleInterface = new ConsoleInterface {
    import scala.Console.{ GREEN, RED, YELLOW, RESET }
    override def appendLog(level: Level.Value, message: => String): Unit = {
      val prefix = level match {
        case Level.Warn | Level.Error => s"[$RED$level$RESET]"
        case Level.Debug              => s"[$YELLOW$level$RESET]"
        case _                        => s"[$level]"
      }
      println(s"$prefix $message")
    }

    override def success(msg: String): Unit = println(s"[${GREEN}success$RESET] $msg")
  }
} with NetworkClientImpl {
  println(provider)
}
object SimpleClient {
  def apply(args: Array[String]): SimpleClient = {
    val file =
      if (args.length == 0) new File("").getCanonicalFile
      else new File(args(0)).getCanonicalFile
    val jni = args.contains("--jni")
    new SimpleClient(
      file,
      args.tail.toList,
      if (jni) UnixDomainSocketLibraryProvider.jni else UnixDomainSocketLibraryProvider.jna
    )
  }
}

object NetworkClient {
  def run(configuration: xsbti.AppConfiguration, arguments: List[String]): Unit =
    try {
      new NetworkClient(configuration, arguments)
      ()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
}
