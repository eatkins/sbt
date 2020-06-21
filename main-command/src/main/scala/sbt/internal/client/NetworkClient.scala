/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal
package client

import java.io.{ File, IOException, InputStream, PrintStream }
import java.lang.ProcessBuilder.Redirect
import java.net.Socket
import java.nio.channels.ClosedChannelException
import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import java.util.concurrent.{ ConcurrentHashMap, LinkedBlockingQueue, TimeUnit }

import sbt.internal.client.NetworkClient.Arguments
import sbt.internal.langserver.{ LogMessageParams, MessageType, PublishDiagnosticsParams }
import sbt.internal.protocol._
import sbt.internal.util.{ ConsoleAppender, NetworkReader, Signals, Terminal, Util }
import sbt.io.IO
import sbt.io.syntax._
import sbt.protocol._
import sbt.util.Level
import sjsonnew.BasicJsonProtocol._
import sjsonnew.shaded.scalajson.ast.unsafe.{ JObject, JValue }
import sjsonnew.support.scalajson.unsafe.Converter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{ Failure, Properties, Success }
import Serialization.{
  attach,
  cancelRequest,
  systemIn,
  systemOut,
  terminalCapabilities,
  terminalCapabilitiesResponse,
  terminalPropertiesQuery,
  terminalPropertiesResponse
}

trait ConsoleInterface {
  def appendLog(level: Level.Value, message: => String): Unit
  def success(msg: String): Unit
}

trait NetworkClientImpl extends AutoCloseable { self =>
  def baseDirectory: File
  def arguments: List[String] = (sbtArguments ++ commandArguments).toList
  def console: ConsoleInterface
  def sbtArguments: Seq[String]
  def commandArguments: Seq[String]
  def inputStream: InputStream
  def errorStream: PrintStream
  def printStream: PrintStream
  def mkSocket(file: File): (Socket, Option[String])
  val sbtScript: String
  private val status = new AtomicReference("Ready")
  private val lock: AnyRef = new AnyRef {}
  private val running = new AtomicBoolean(true)
  private val pendingResults = new ConcurrentHashMap[String, (LinkedBlockingQueue[Integer], Long)]
  private val pendingCancellations = new ConcurrentHashMap[String, LinkedBlockingQueue[Boolean]]
  private val pendingCompletions = new ConcurrentHashMap[String, CompletionResponse => Unit]
  private val attached = new AtomicBoolean(false)
  private val attachUUID = new AtomicReference[String](null)
  private val connectionHolder = new AtomicReference[ServerConnection]
  private val batchMode = new AtomicBoolean(false)
  private val interactiveThread = new AtomicReference[Thread](null)

  private def portfile = baseDirectory / "project" / "target" / "active.json"

  def connection: ServerConnection = connectionHolder.synchronized {
    connectionHolder.get match {
      case null => init(false, true)
      case c    => c
    }
  }

  private[this] val stdinBytes = new LinkedBlockingQueue[Int]
  private[this] val stdin: InputStream = new InputStream {
    override def available(): Int = stdinBytes.size
    override def read: Int = stdinBytes.take
  }
  private[this] val inputThread = new AtomicReference(new RawInputThread)
  private[this] val exitClean = new AtomicBoolean(true)
  private[this] val sbtProcess = new AtomicReference[Process](null)
  private class ConnectionRefusedException(t: Throwable) extends Throwable(t)

  // Open server connection based on the portfile
  def init(prompt: Boolean, retry: Boolean): ServerConnection =
    try {
      if (!portfile.exists) {
        if (prompt) {
          errorStream.print("\nNo sbt server is running. Press <tab> to start one...")
          stdinBytes.take match {
            case 9 =>
              errorStream.println("\nStarting server...")
              forkServer(portfile, !prompt)
            case _ => System.exit(0)
          }
        } else forkServer(portfile, log = true)
      }
      val (sk, tkn) =
        try mkSocket(portfile)
        catch { case e: IOException => throw new ConnectionRefusedException(e) }
      val conn = new ServerConnection(sk) {
        override def onNotification(msg: JsonRpcNotificationMessage): Unit = {
          msg.method match {
            case "shutdown" =>
              val log = msg.params match {
                case Some(jvalue) => Converter.fromJson[Boolean](jvalue).getOrElse(true)
                case _            => false
              }
              if (running.compareAndSet(true, false) && log) {
                errorStream.println()
                console.appendLog(Level.Error, "sbt server disconnected")
              }
              stdinBytes.offer(-1)
              Option(inputThread.get).foreach(_.close())
              Option(interactiveThread.get).foreach(_.interrupt)
            case "readInput" =>
            case _           => self.onNotification(msg)
          }
        }
        override def onRequest(msg: JsonRpcRequestMessage): Unit = self.onRequest(msg)
        override def onResponse(msg: JsonRpcResponseMessage): Unit = self.onResponse(msg)
        override def onShutdown(): Unit = {
          if (exitClean.get != false) exitClean.set(!running.get)
          running.set(false)
          Option(interactiveThread.get).foreach(_.interrupt())
        }
      }
      // initiate handshake
      val execId = UUID.randomUUID.toString
      val initCommand = InitCommand(tkn, Option(execId), Some(true))
      conn.sendString(Serialization.serializeCommandAsJsonMessage(initCommand))
      connectionHolder.set(conn)
      conn
    } catch {
      case e: ConnectionRefusedException if retry =>
        if (Files.deleteIfExists(portfile.toPath)) init(prompt, retry = false)
        else throw e
    }

  /**
   * Forks another instance of sbt in the background.
   * This instance must be shutdown explicitly via `sbt -client shutdown`
   */
  def forkServer(portfile: File, log: Boolean): Unit = {
    if (log) console.appendLog(Level.Info, "server was not detected. starting an instance")
    val args = if (!sbtArguments.exists(_.startsWith("-Dsbt.color="))) {
      s"-Dsbt.color=${Terminal.console.isColorEnabled}" +: sbtArguments
    } else sbtArguments
    val cmd = sbtScript +: args :+ BasicCommandStrings.CloseIOStreams
    val process =
      new ProcessBuilder(cmd: _*)
        .directory(baseDirectory)
        .redirectInput(Redirect.PIPE)
        .start()
    sbtProcess.set(process)
    val hook = new Thread(() => Option(sbtProcess.get).foreach(_.destroyForcibly()))
    Runtime.getRuntime.addShutdownHook(hook)
    val stdout = process.getInputStream
    val stderr = process.getErrorStream
    val stdin = process.getOutputStream
    @tailrec
    def blockUntilStart(): Unit = {
      val stop = try {
        while (stdout.available > 0) {
          val byte = stdout.read
          printStream.write(byte)
        }
        while (stderr.available > 0) {
          val byte = stderr.read
          errorStream.write(byte)
        }
        while (!stdinBytes.isEmpty) {
          stdin.write(stdinBytes.take)
          stdin.flush()
        }
        false
      } catch {
        case _: IOException => false
      }
      Thread.sleep(10)
      if (!portfile.exists && !stop) blockUntilStart()
      else {
        stdin.close()
        stdout.close()
        stderr.close()
        process.getOutputStream.close()
      }
    }

    try blockUntilStart()
    catch { case t: Throwable => t.printStackTrace() } finally {
      sbtProcess.set(null)
      Util.ignoreResult(Runtime.getRuntime.removeShutdownHook(hook))
    }
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

  private def getExitCode(jvalue: Option[JValue]): Integer = jvalue match {
    case Some(o: JObject) =>
      o.value
        .collectFirst {
          case v if v.field == "exitCode" =>
            Converter.fromJson[Integer](v.value).getOrElse(Integer.valueOf(1))
        }
        .getOrElse(1)
    case _ => 1
  }
  def onResponse(msg: JsonRpcResponseMessage): Unit = {
    pendingResults.remove(msg.id) match {
      case null =>
      case (q, startTime) =>
        val now = System.currentTimeMillis
        val message = timing(startTime, now)
        val exitCode = getExitCode(msg.result)
        if (batchMode.get || !attached.get) {
          if (exitCode == 0) console.success(message)
          else if (!attached.get) console.appendLog(Level.Error, message)
        }
        q.offer(exitCode)
    }
    pendingCancellations.remove(msg.id) match {
      case null =>
      case q    => q.offer(msg.toString.contains("Task cancelled"))
    }
    msg.id match {
      case execId =>
        if (attachUUID.get == msg.id) {
          attachUUID.set(null)
          attached.set(true)
          Option(inputThread.get).foreach(_.drain())
        }
        pendingCompletions.remove(execId) match {
          case null =>
          case completions =>
            completions(msg.result match {
              case Some(o: JObject) =>
                o.value
                  .foldLeft(CompletionResponse(Vector.empty[String])) {
                    case (resp, i) =>
                      if (i.field == "items")
                        resp.withItems(
                          Converter
                            .fromJson[Vector[String]](i.value)
                            .getOrElse(Vector.empty[String])
                        )
                      else if (i.field == "cachedTestNames")
                        resp.withCachedTestNames(
                          Converter.fromJson[Boolean](i.value).getOrElse(true)
                        )
                      else if (i.field == "cachedMainClassNames")
                        resp.withCachedMainClassNames(
                          Converter.fromJson[Boolean](i.value).getOrElse(true)
                        )
                      else resp
                  }
              case _ => CompletionResponse(Vector.empty[String])
            })
        }
    }
  }

  def onNotification(msg: JsonRpcNotificationMessage): Unit = {
    def splitToMessage: Vector[(Level.Value, String)] =
      (msg.method, msg.params) match {
        case ("build/logMessage", Some(json)) =>
          if (!attached.get) {
            import sbt.internal.langserver.codec.JsonProtocol._
            Converter.fromJson[LogMessageParams](json) match {
              case Success(params) => splitLogMessage(params)
              case Failure(_)      => Vector()
            }
          } else Vector()
        case (`terminalPropertiesQuery`, Some(json)) =>
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
              sendCommandResponse(terminalPropertiesResponse, response, id)
            case Failure(_) =>
          }
          Vector.empty
        case (`terminalCapabilities`, Some(json)) =>
          import sbt.protocol.codec.JsonProtocol._
          Converter.fromJson[TerminalCapabilitiesQuery](json) match {
            case Success(terminalCapabilitiesQuery) =>
              val response = TerminalCapabilitiesResponse.apply(
                terminalCapabilitiesQuery.id,
                terminalCapabilitiesQuery.boolean.map(Terminal.console.getBooleanCapability),
                terminalCapabilitiesQuery.numeric.map(Terminal.console.getNumericCapability),
                terminalCapabilitiesQuery.string
                  .map(s => Option(Terminal.console.getStringCapability(s)).getOrElse("null")),
              )
              sendCommandResponse(
                terminalCapabilitiesResponse,
                response,
                terminalCapabilitiesQuery.id
              )
            case Failure(_) =>
          }
          Vector.empty
        case (`systemOut`, Some(json)) =>
          Converter.fromJson[Seq[Byte]](json) match {
            case Success(params) =>
              if (params.nonEmpty) {
                if (attached.get) {
                  printStream.write(params.toArray)
                  printStream.flush()
                }
              }
            case Failure(_) =>
          }
          Vector.empty
        case ("textDocument/publishDiagnostics", Some(json)) =>
          import sbt.internal.langserver.codec.JsonProtocol._
          Converter.fromJson[PublishDiagnosticsParams](json) match {
            case Success(params) => splitDiagnostics(params); Vector()
            case Failure(_)      => Vector()
          }
        case ("shutdown", Some(_))                => Vector.empty
        case (msg, _) if msg.startsWith("build/") => Vector.empty
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

  def connect(log: Boolean, prompt: Boolean): Unit = {
    if (log) console.appendLog(Level.Info, "entering *experimental* thin client - BEEP WHIRR")
    init(prompt, true)
    ()
  }

  private[this] val contHandler: () => Unit = () => {
    if (Terminal.console.getLastLine.nonEmpty)
      printStream.print(ConsoleAppender.DeleteLine + Terminal.console.getLastLine.get)
  }
  private[this] def withSignalHandler[R](handler: () => Unit, sig: String)(f: => R): R = {
    val registration = Signals.register(handler, sig)
    try f
    finally registration.remove()
  }
  private[this] val cancelled = new AtomicBoolean(false)

  def run(): Int =
    withSignalHandler(contHandler, Signals.CONT) {
      interactiveThread.set(Thread.currentThread)
      val cleaned = arguments.collect { case c if !c.startsWith("-") => c.trim }
      val userCommands = cleaned.takeWhile(_ != "exit")
      val interactive = cleaned.isEmpty
      val exit = cleaned.nonEmpty && userCommands.isEmpty
      attachUUID.set(sendJson(attach, s"""{"interactive": $interactive}"""))
      val handler: () => Unit = () => {
        def exitAbruptly() = {
          exitClean.set(false)
          close()
        }
        if (cancelled.compareAndSet(false, true)) {
          val cancelledTasks = {
            val queue = sendCancelAllCommand()
            Option(queue.poll(1, TimeUnit.SECONDS)).getOrElse(true)
          }
          if ((!interactive && pendingResults.isEmpty) || !cancelledTasks) exitAbruptly()
          else cancelled.set(false)
        } else exitAbruptly() // handles double ctrl+c to force a shutdown
      }
      withSignalHandler(handler, Signals.INT) {
        if (interactive) {
          try this.synchronized(this.wait)
          catch { case _: InterruptedException => }
          if (exitClean.get) 0 else 1
        } else if (exit) {
          0
        } else {
          batchMode.set(true)
          batchExecute(userCommands)
        }
      }
    }

  def batchExecute(userCommands: List[String]): Int = {
    val cmd = userCommands mkString " "
    printStream.println("> " + cmd)
    sendAndWait(cmd, None)
  }

  private def sendAndWait(cmd: String, limit: Option[Deadline]): Int = {
    val queue = sendExecCommand(cmd)
    var result: Integer = null
    while (running.get && result == null && limit.fold(true)(!_.isOverdue())) {
      try {
        result = limit match {
          case Some(l) => queue.poll((l - Deadline.now).toMillis, TimeUnit.MILLISECONDS)
          case _       => queue.take
        }
      } catch {
        case _: InterruptedException if cmd == "shutdown" => result = 0
        case _: InterruptedException                      => result = if (exitClean.get) 0 else 1
      }
    }
    if (result == null) 1 else result
  }

  private def cancelAll: Boolean = false

  def getCompletions(query: String, fish: Boolean): Seq[String] = {
    connect(log = false, prompt = true)
    val quoteCount = query.foldLeft(0) {
      case (count, '"') => count + 1
      case (count, _)   => count
    }
    val inQuote = quoteCount % 2 != 0
    val (rawPrefix, prefix, rawSuffix, suffix) = if (quoteCount > 0) {
      query.lastIndexOf('"') match {
        case -1 => (query, query, None, None) // shouldn't happen
        case i =>
          val rawPrefix = query.substring(0, i)
          val prefix = rawPrefix.replaceAllLiterally("\"", "").replaceAllLiterally("\\;", ";")
          val rawSuffix = query.substring(i).replaceAllLiterally("\\;", ";")
          val suffix = if (rawSuffix.length > 1) rawSuffix.substring(1) else ""
          (rawPrefix, prefix, Some(rawSuffix), Some(suffix))
      }
    } else (query, query.replaceAllLiterally("\\;", ";"), None, None)
    val tailSpace = query.endsWith(" ") || query.endsWith("\"")
    val sanitizedQuery = suffix.foldLeft(prefix) { _ + _ }
    def getCompletions(query: String, sendCommand: Boolean): Seq[String] = {
      val result = new LinkedBlockingQueue[CompletionResponse]()
      val json = s"""{"query":"$query","level":1}"""
      val execId = sendJson("sbt/completion", json)
      pendingCompletions.put(execId, result.put)
      val response = result.take
      def fillCompletions(label: String, regex: String, command: String): Seq[String] = {
        errorStream.print(s"\nNo cached $label names found. Press '<tab>' to compile: ")
        stdinBytes.take match {
          case 9 =>
            errorStream.println()
            sendJson(attach, s"""{"interactive": false}""")
            sendAndWait(query.replaceAll(regex + ".*", command).trim, None)
            getCompletions(query, false)
          case _ => Nil
        }
      }
      val testNameCompletions =
        if (!response.cachedTestNames.getOrElse(true) && sendCommand)
          fillCompletions("test", "test(Only|Quick)", "definedTestNames")
        else Nil
      val classNameCompletions =
        if (!response.cachedMainClassNames.getOrElse(true) && sendCommand)
          fillCompletions("main class", "runMain", "discoveredMainClasses")
        else Nil
      val completions = response.items
      testNameCompletions ++ classNameCompletions ++ completions
    }
    getCompletions(sanitizedQuery, true) collect {
      case c if inQuote                      => c
      case c if tailSpace && c.contains(" ") => c.replaceAllLiterally(prefix, "")
      case c if !tailSpace                   => c.split(" ").last
    }
  }

  /**
   * This is not used but it provides an example of how one could build an
   * alternative interactive shell without using server side rendering.
   */
  def shell(): Unit = {
    val reader =
      new NetworkReader(Terminal.console, (prefix, level) => {
        val execId = sendJson("sbt/completion", s"""{"query":"$prefix","level":$level}""")
        val result = new LinkedBlockingQueue[CompletionResponse]()
        pendingCompletions.put(execId, result.put)
        val completions = result.take.items
        val insert = completions.collect {
          case c if c.startsWith(prefix) => c.substring(prefix.length)
        }
        (insert, completions)
      })
    try {
      while (running.get) {
        reader.readLine("> ", None) match {
          case Some("exit")               => running.set(false)
          case Some(s) if s.trim.nonEmpty => sendAndWait(s, None)
          case _                          => //
        }
      }
    } catch { case _: InterruptedException => running.set(false) }
  }

  def sendExecCommand(commandLine: String): LinkedBlockingQueue[Integer] = {
    val execId = UUID.randomUUID.toString
    val queue = new LinkedBlockingQueue[Integer]
    sendCommand(ExecCommand(commandLine, execId))
    pendingResults.put(execId, (queue, System.currentTimeMillis))
    queue
  }

  def sendCancelAllCommand(): LinkedBlockingQueue[Boolean] = {
    val queue = new LinkedBlockingQueue[Boolean]
    val execId = sendJson(cancelRequest, """{"id":"__all"}""")
    pendingCancellations.put(execId, queue)
    queue
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
        errorStream.println(s"Caught exception writing command to server: $e")
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
        errorStream.println(s"Caught exception writing command to server: $e")
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
    val lock = new Object
    override final def run(): Unit = {
      @tailrec def read(): Unit = {
        inputStream.read match {
          case -1 =>
          case b =>
            lock.synchronized(stdinBytes.offer(b))
            if (attached.get()) drain()
            if (!stopped.get()) read()
        }
      }
      try Terminal.console.withRawSystemIn(read())
      catch { case _: InterruptedException | _: ClosedChannelException => stopped.set(true) }
    }

    def drain(): Unit = lock.synchronized {
      while (!stdinBytes.isEmpty) {
        val byte = stdinBytes.poll()
        sendJson(systemIn, byte.toString)
      }
    }

    override def close(): Unit = {
      RawInputThread.this.interrupt()
    }
  }

  // copied from Aggregation
  private def timing(startTime: Long, endTime: Long): String = {
    import java.text.DateFormat
    val format = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM)
    val nowString = format.format(new java.util.Date(endTime))
    val total = (endTime - startTime + 500) / 1000
    val totalString = s"$total s" +
      (if (total <= 60) ""
       else {
         val maybeHours = total / 3600 match {
           case 0 => ""
           case h => f"$h%02d:"
         }
         val mins = f"${total % 3600 / 60}%02d"
         val secs = f"${total % 60}%02d"
         s" ($maybeHours$mins:$secs)"
       })
    s"Total time: $totalString, completed $nowString"
  }
  override def close(): Unit =
    try {
      running.set(false)
      stdinBytes.offer(-1)
      val mainThread = interactiveThread.getAndSet(null)
      if (mainThread != null && mainThread != Thread.currentThread) mainThread.interrupt
      connection.shutdown()
      Option(inputThread.get).foreach(_.interrupt())
    } catch {
      case t: Throwable => t.printStackTrace(); throw t
    }
}
class NetworkClient(
    configuration: xsbti.AppConfiguration,
    override val sbtArguments: Seq[String],
    override val commandArguments: Seq[String],
    override val sbtScript: String,
    override val inputStream: InputStream,
    override val errorStream: PrintStream,
    override val printStream: PrintStream
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
      System.in,
      System.err,
      System.out
    )
  override def mkSocket(file: File): (Socket, Option[String]) = ClientSocket.socket(file)
  def this(configuration: xsbti.AppConfiguration, args: List[String]) =
    this(configuration, NetworkClient.parseArgs(args.toArray))
  override def baseDirectory: File = configuration.baseDirectory
}

class SimpleClient(
    override val baseDirectory: File,
    val sbtArguments: Seq[String],
    val commandArguments: Seq[String],
    override val sbtScript: String,
    override val inputStream: InputStream,
    override val errorStream: PrintStream,
    override val printStream: PrintStream,
    useJNI: Boolean,
) extends {
  override val console: ConsoleInterface = new ConsoleInterface {
    import scala.Console.{ GREEN, RED, RESET, YELLOW }
    override def appendLog(level: Level.Value, message: => String): Unit = {
      val prefix = level match {
        case Level.Error => s"[$RED$level$RESET]"
        case Level.Warn  => s"[$YELLOW$level$RESET]"
        case _           => s"[$RESET$level$RESET]"
      }
      message.split("\n").foreach { line =>
        if (!line.trim.isEmpty) printStream.println(s"$prefix $line")
      }
    }

    override def success(msg: String): Unit = printStream.println(s"[${GREEN}success$RESET] $msg")
  }
} with NetworkClientImpl {
  override def mkSocket(file: File): (Socket, Option[String]) = ClientSocket.socket(file, useJNI)
}
object SimpleClient {
  def complete(args: Array[String], useJNI: Boolean, in: InputStream, out: PrintStream): Int = {
    val cmd: String = args.find(_.startsWith(NetworkClient.completions)) match {
      case Some(c) =>
        c.split('=').lastOption match {
          case Some(query) =>
            query.indexOf(" ") match {
              case -1 => throw new IllegalArgumentException(query)
              case i  => query.substring(i + 1)
            }
          case _ => throw new IllegalArgumentException(c)
        }
      case _ => throw new IllegalStateException("should be unreachable")
    }
    val quiet = args.exists(_ == "--quiet")
    val errorStream = if (quiet) new PrintStream(_ => {}, false) else System.err
    val sbtArgs = args.takeWhile(!_.startsWith(NetworkClient.completions))
    val arguments = NetworkClient.parseArgs(sbtArgs)
    val client =
      simpleClient(
        arguments,
        inputStream = in,
        errorStream = errorStream,
        printStream = errorStream,
        useJNI = useJNI
      )
    try {
      val results = client.getCompletions(cmd, args.contains("--fish"))
      out.println(results.sorted.distinct mkString "\n")
      0
    } catch { case _: Exception => 1 } finally client.close()
  }
  def client(
      args: Array[String],
      inputStream: InputStream,
      errorStream: PrintStream,
      printStream: PrintStream,
      useJNI: Boolean
  ): Int = {
    val client =
      simpleClient(NetworkClient.parseArgs(args), inputStream, errorStream, printStream, useJNI)
    try {
      client.connect(log = true, prompt = false)
      client.run()
    } catch { case _: Exception => 1 } finally client.close()
  }
  def client(args: Array[String]): Int = client(args, System.in, System.err, System.out, false)
  private def simpleClient(
      arguments: Arguments,
      inputStream: InputStream,
      errorStream: PrintStream,
      printStream: PrintStream,
      useJNI: Boolean,
  ): SimpleClient =
    new SimpleClient(
      arguments.baseDirectory,
      arguments.sbtArguments,
      arguments.commandArguments,
      arguments.sbtScript,
      inputStream,
      errorStream,
      printStream,
      useJNI
    )
  def main(useJNI: Boolean, args: Array[String]): Unit = {
    if (args.exists(_.startsWith(NetworkClient.completions)))
      System.exit(complete(args, useJNI, System.in, System.out))
    else {
      val hook = new Thread(() => {
        System.out.print(ConsoleAppender.ClearScreenAfterCursor)
        System.out.flush()
      })
      Runtime.getRuntime.addShutdownHook(hook)
      System.exit(Terminal.withStreams {
        try client(args, System.in, System.err, System.out, useJNI)
        finally {
          Runtime.getRuntime.removeShutdownHook(hook)
          hook.run()
        }
      })
    }
  }
}

object NetworkClient {
  private[client] class Arguments(
      val baseDirectory: File,
      val sbtArguments: Seq[String],
      val commandArguments: Seq[String],
      val sbtScript: String,
  )
  private[client] val completions = "--completions"
  private[client] val sbtBase = "--sbt-base-directory"
  private[client] def parseArgs(args: Array[String]): Arguments = {
    var i = 0
    var sbtScript = if (Properties.isWin) "sbt.cmd" else "sbt"
    val commandArgs = new mutable.ArrayBuffer[String]
    val sbtArguments = new mutable.ArrayBuffer[String]
    val completeArgs = new mutable.ArrayBuffer[String]
    val SysProp = "-D([^=]+)=(.*)".r
    var base: File = new File("")
    val sanitized = args.flatMap {
      case a if a.startsWith("\"") => Array(a)
      case a                       => a.split(" ")
    }
    var foundCompletions = false
    while (i < sanitized.length) {
      sanitized(i) match {
        case a if a.startsWith(completions) => foundCompletions = true
        case a if a.startsWith("--sbt-script=") =>
          sbtScript = a.split("--sbt-script=").lastOption.getOrElse(sbtScript)
        case a if a.startsWith("--sbt-base-directory=") =>
          base = a.split("--sbt-base-directory=").lastOption.map(file).getOrElse(base)
        case a if !a.startsWith("-") && !foundCompletions => commandArgs += a
        case a @ SysProp(key, value) =>
          System.setProperty(key, value)
          sbtArguments += a
        case a if !foundCompletions =>
          sbtArguments += a
      }
      i += 1
    }
    new Arguments(base.getCanonicalFile, sbtArguments, commandArgs, sbtScript)
  }
  def run(configuration: xsbti.AppConfiguration, arguments: List[String]): Unit =
    try {
      val args = parseArgs(arguments.toArray)
      val client = new NetworkClient(
        configuration,
        args.sbtArguments,
        args.commandArguments,
        args.sbtScript,
        System.in,
        System.err,
        System.out
      )
      System.exit(try {
        client.connect(log = true, prompt = false)
        client.run()
      } catch { case _: Throwable => 1 } finally client.close())
      ()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
}
