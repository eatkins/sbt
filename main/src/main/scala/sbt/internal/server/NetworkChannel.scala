/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal
package server

import java.io.{ IOException, InputStream, OutputStream }
import java.net.{ Socket, SocketTimeoutException }
import java.nio.channels.ClosedChannelException
import java.util.UUID
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import java.util.concurrent.{ ArrayBlockingQueue, ConcurrentHashMap, LinkedBlockingQueue }

import sbt.internal.langserver.{ CancelRequestParams, ErrorCodes, LogMessageParams, MessageType }
import sbt.internal.langserver.{ CancelRequestParams, ErrorCodes }
import sbt.internal.util.complete.{ Parser, Parsers }
import sbt.internal.protocol.{
  JsonRpcNotificationMessage,
  JsonRpcRequestMessage,
  JsonRpcResponseError,
  JsonRpcResponseMessage
}
import sbt.internal.util.ObjectEvent
import sbt.internal.util.complete.Parser
import sbt.internal.util.{ ObjectEvent, Prompt }
import sbt.protocol._
import sbt.util.Logger
import sjsonnew._
import sjsonnew.support.scalajson.unsafe.{ CompactPrinter, Converter }

import sbt.internal.ui.{ UITask, UserThread }
import sbt.internal.util.Terminal.TerminalImpl
import sbt.internal.util.complete.Parser
import sbt.internal.util.{ ObjectEvent, Terminal, Util }
import sbt.protocol._
import sbt.util.Logger
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import sjsonnew._
import sjsonnew.support.scalajson.unsafe.Converter

import scala.util.Try
import scala.util.control.NonFatal
import Serialization.{
  attach,
  systemIn,
  terminalCapabilities,
  terminalCapabilitiesResponse,
  terminalPropertiesQuery,
  terminalPropertiesResponse
}

final class NetworkChannel(
    val name: String,
    connection: Socket,
    protected val structure: BuildStructure,
    auth: Set[ServerAuthentication],
    instance: ServerInstance,
    handlers: Seq[ServerHandler],
    val log: Logger,
    mkUIThreadImpl: (State, CommandChannel) => UITask
) extends CommandChannel { self =>
  def this(
      name: String,
      connection: Socket,
      structure: BuildStructure,
      auth: Set[ServerAuthentication],
      instance: ServerInstance,
      handlers: Seq[ServerHandler],
      log: Logger
  ) =
    this(
      name,
      connection,
      structure,
      auth,
      instance,
      handlers,
      log,
      new UITask.AskUserTask(_, _)
    )
  import NetworkChannel._

  private[this] val inputBuffer = new LinkedBlockingQueue[Byte]()
  private[this] val pendingWrites = new LinkedBlockingQueue[(Array[Byte], Boolean)]()
  private[this] val attached = new AtomicBoolean(false)
  private val running = new AtomicBoolean(true)
  private val delimiter: Byte = '\n'.toByte
  private val out = connection.getOutputStream
  private var initialized = false
  private val ContentLength = """^Content\-Length\:\s*(\d+)""".r
  private val ContentType = """^Content\-Type\:\s*(.+)""".r
  private var _contentType: String = ""
  private val pendingRequests: mutable.Map[String, JsonRpcRequestMessage] = mutable.Map()
  private[this] val alive = new AtomicBoolean(true)
  private[sbt] def isInteractive = interactive.get
  private[this] val interactive = new AtomicBoolean(false)

  override private[sbt] val terminal: Terminal = new NetworkTerminal
  override val userThread: UserThread = new UserThread(this)

  private lazy val callback: ServerCallback = new ServerCallback {
    def jsonRpcRespond[A: JsonFormat](event: A, execId: Option[String]): Unit =
      self.respondResult(event, execId)

    def jsonRpcRespondError(execId: Option[String], code: Long, message: String): Unit =
      self.respondError(code, message, execId)

    def jsonRpcNotify[A: JsonFormat](method: String, params: A): Unit =
      self.jsonRpcNotify(method, params)

    def appendExec(commandLine: String, execId: Option[String]): Boolean =
      self.append(Exec(commandLine, execId, Some(CommandSource(name))))

    def appendExec(exec: Exec): Boolean = self.append(exec)

    def log: Logger = self.log
    def name: String = self.name
    private[sbt] def authOptions: Set[ServerAuthentication] = self.authOptions
    private[sbt] def authenticate(token: String): Boolean = self.authenticate(token)
    private[sbt] def setInitialized(value: Boolean): Unit = self.setInitialized(value)
    private[sbt] def onSettingQuery(execId: Option[String], req: SettingQuery): Unit =
      self.onSettingQuery(execId, req)
    private[sbt] def onCompletionRequest(execId: Option[String], cp: CompletionParams): Unit =
      self.onCompletionRequest(execId, cp)
    private[sbt] def onCancellationRequest(execId: Option[String], crp: CancelRequestParams): Unit =
      self.onCancellationRequest(execId, crp)
  }

  def setContentType(ct: String): Unit = synchronized { _contentType = ct }
  def contentType: String = _contentType

  protected def authenticate(token: String): Boolean = instance.authenticate(token)

  protected def setInitialized(value: Boolean): Unit = initialized = value

  protected def authOptions: Set[ServerAuthentication] = auth

  override def mkUIThread: (State, CommandChannel) => UITask = (state, command) => {
    if (interactive.get || ContinuousCommands.isInWatch(this)) mkUIThreadImpl(state, command)
    else
      new UITask {
        override private[sbt] def channel = NetworkChannel.this
        override def reader: UITask.Reader = () => {
          try {
            this.synchronized(this.wait)
            Left("exit")
          } catch {
            case _: InterruptedException => Right("")
          }
        }
      }
  }

  val thread = new Thread(s"sbt-networkchannel-${connection.getPort}") {
    var contentLength: Int = 0
    var state: ChannelState = SingleLine

    override def run(): Unit = {
      try {
        val in = connection.getInputStream
        connection.setSoTimeout(5000)

        // keep going unless the socket has closed
        while (running.get) {
          try {
            var bytes = new mutable.ArrayBuffer[Byte]
            var content: Seq[Byte] = Seq.empty[Byte]
            var consecutiveLineEndings = 0
            do {
              val byte = in.read
              byte match {
                case 10 =>
                  val lineBytes = bytes.toArray
                  bytes = new mutable.ArrayBuffer[Byte]
                  val line = lineBytes.lastOption match {
                    case Some(13) => new String(lineBytes, 0, lineBytes.length - 1, "UTF-8")
                    case _        => new String(lineBytes, "UTF-8")
                  }
                  consecutiveLineEndings += 1
                  if (line.startsWith("Content-Length: ")) {
                    Try(line.drop(16).toInt) foreach { len =>
                      var state = 0
                      do {
                        in.read match {
                          case 10 if state == 1 =>
                            state = 0
                            consecutiveLineEndings += 1
                          case 13 => state = 1
                          case c =>
                            state = 0
                            consecutiveLineEndings = 0
                        }
                      } while (consecutiveLineEndings < 2)
                      val buf = new Array[Byte](len)
                      var offset = 0
                      do {
                        offset += in.read(buf, offset, len - offset)
                      } while (offset < len)
                      content = buf.toSeq
                    }
                  } else if (line.startsWith("{")) {
                    content = lineBytes.toSeq
                  }
                case i if i < 0 =>
                  running.set(false)
                  alive.set(false)
                case i =>
                  consecutiveLineEndings = 0
                  bytes += i.toByte
              }
            } while (content.isEmpty && running.get)
            if (content.nonEmpty) handleBody(content)
          } catch {
            case _: SocketTimeoutException => // its ok
            case _: IOException | _: InterruptedException =>
              running.set(false)
              alive.set(false)
          }
        } // while
      } finally {
        shutdown(false)
      }
    }

    private lazy val intents = {
      handlers.toVector map { h =>
        h.handler(callback)
      }
    }

    lazy val onRequestMessage: PartialFunction[JsonRpcRequestMessage, Unit] =
      intents.foldLeft(PartialFunction.empty[JsonRpcRequestMessage, Unit]) {
        case (f, i) => f orElse i.onRequest
      }

    lazy val onNotification: PartialFunction[JsonRpcNotificationMessage, Unit] =
      intents.foldLeft(PartialFunction.empty[JsonRpcNotificationMessage, Unit]) {
        case (f, i) => f orElse i.onNotification
      }

    def handleBody(chunk: Seq[Byte]): Unit = {
      Serialization.deserializeJsonMessage(chunk) match {
        case Right(req: JsonRpcRequestMessage) =>
          registerRequest(req)
          /*
           * The following pattern match is to handle thin client specific requests
           */
          req.method match {
            case `systemIn` =>
              import sjsonnew.BasicJsonProtocol._
              pendingRequests.remove(req.id)
              val byte = req.params.flatMap(Converter.fromJson[Byte](_).toOption)
              byte.foreach(inputBuffer.put)
            case `terminalPropertiesResponse` =>
              import sbt.protocol.codec.JsonProtocol._
              val response =
                req.params.flatMap(Converter.fromJson[TerminalPropertiesResponse](_).toOption)
              pendingRequests.remove(req.id)
              pendingTerminalProperties.remove(req.id) match {
                case null   =>
                case buffer => response.foreach(buffer.put)
              }
            case `terminalCapabilitiesResponse` =>
              import sbt.protocol.codec.JsonProtocol._
              val response =
                req.params.flatMap(
                  Converter.fromJson[TerminalCapabilitiesResponse](_).toOption
                )
              pendingRequests.remove(req.id)
              pendingTerminalCapability.remove(req.id) match {
                case null =>
                case buffer =>
                  buffer.put(
                    response.getOrElse(TerminalCapabilitiesResponse("", None, None, None))
                  )
              }
            case `attach` =>
              import sbt.protocol.codec.JsonProtocol.AttachFormat
              val isInteractive = req.params
                .flatMap(Converter.fromJson[Attach](_).toOption.map(_.interactive))
                .exists(identity)
              interactive.set(isInteractive)
              if (!isInteractive) terminal.setPrompt(Prompt.Batch)
              initiateMaintenance(attach)
              import sjsonnew.BasicJsonProtocol._
              pendingRequests -= req.id
              jsonRpcRespond("", req.id)
              attached.set(true)
            case t =>
              // handle default LSP requests
              onRequestMessage(req)
          }
        case Right(ntf: JsonRpcNotificationMessage) =>
          try {
            onNotification(ntf)
          } catch {
            case LangServerError(code, message) =>
              logMessage("error", s"error $code while handling notification: $message")
          }
        case Right(msg) =>
          log.debug(s"unhandled message: $msg")
        case Left(errorDesc) =>
          val msg =
            s"got invalid chunk from client (${new String(chunk.toArray, "UTF-8")}): $errorDesc"
          log.error(msg)
          logMessage("error", msg)
      }
    }

    def handleHeader(str: String): Option[Unit] = {
      val sbtX1Protocol = "application/sbt-x1"
      str match {
        case ContentLength(len) =>
          contentLength = len.toInt
          Some(())
        case ContentType(ct) =>
          if (ct == sbtX1Protocol) {
            logMessage("error", s"server protocol $ct is no longer supported")
          }
          setContentType(ct)
          Some(())
        case _ => None
      }
    }
  }
  thread.start()

  private[sbt] def isLanguageServerProtocol: Boolean = true

  private def registerRequest(request: JsonRpcRequestMessage): Unit = {
    this.synchronized {
      pendingRequests += (request.id -> request)
      ()
    }
  }

  private[sbt] def respondError(
      err: JsonRpcResponseError,
      execId: Option[String]
  ): Unit = this.synchronized {
    def respond(id: String) = {
      pendingRequests -= id
      jsonRpcRespondError(id, err)
    }
    def error(): Unit = logMessage("error", s"Error ${err.code}: ${err.message}")
    execId match {
      case Some(id) if pendingRequests.contains(id) => respond(id)
      // This handles multi commands from the network that were remapped to a different
      // exec id for reporting purposes.
      case Some(id) if id.startsWith(BasicCommands.networkExecPrefix) =>
        StandardMain.exchange.withState { s =>
          s.get(BasicCommands.execMap).flatMap(_.collectFirst { case (k, `id`) => k }) match {
            case Some(id) if pendingRequests.contains(id) => respond(id)
            case _                                        => error()
          }
        }
      case _ => error()
    }
  }

  private[sbt] def respondError(
      code: Long,
      message: String,
      execId: Option[String]
  ): Unit = {
    respondError(JsonRpcResponseError(code, message), execId)
  }

  private[sbt] def respondResult[A: JsonFormat](
      event: A,
      execId: Option[String]
  ): Unit = this.synchronized {
    def error(): Unit = {
      val msg =
        s"unmatched json response for requestId $execId: ${CompactPrinter(Converter.toJsonUnsafe(event))}"
      log.error(msg)
    }
    def respond(id: String): Unit = {
      pendingRequests -= id
      jsonRpcRespond(event, id)
    }
    execId match {
      case Some(id) if pendingRequests.contains(id) => respond(id)
      // This handles multi commands from the network that were remapped to a different
      // exec id for reporting purposes.
      case Some(id) if id.startsWith(BasicCommands.networkExecPrefix) =>
        StandardMain.exchange.withState { s =>
          s.get(BasicCommands.execMap).flatMap(_.collectFirst { case (k, `id`) => k }) match {
            case Some(id) if pendingRequests.contains(id) =>
              respond(id)
            case _ => error()
          }
        }
      case _ => error()
    }
  }

  private[sbt] def notifyEvent[A: JsonFormat](method: String, params: A): Unit = {
    jsonRpcNotify(method, params)
  }

  def respond[A: JsonFormat](event: A): Unit = respond(event, None)

  def respond[A: JsonFormat](event: A, execId: Option[String]): Unit = if (alive.get) {
    respondResult(event, execId)
  }

  def notifyEvent(event: EventMessage): Unit = {
    event match {
      case entry: LogEvent        => logMessage(entry.level, entry.message)
      case entry: ExecStatusEvent => logMessage("debug", entry.status)
      case _                      => ()
    }
  }

  /**
   * This publishes object events. The type information has been
   * erased because it went through logging.
   */
  private[sbt] def respond(event: ObjectEvent[_]): Unit = if (alive.get) {
    onObjectEvent(event)
  }

  def publishBytes(event: Array[Byte]): Unit = publishBytes(event, false)

  private[this] val writeThread = new Thread(() => {
    @tailrec def impl(): Unit = {
      val (event, delimit) =
        try pendingWrites.take
        catch {
          case _: InterruptedException =>
            alive.set(false)
            (Array.empty[Byte], false)
        }
      if (alive.get) {
        try {
          out.write(event)
          if (delimit) {
            out.write(delimiter.toInt)
          }
          out.flush()
        } catch {
          case _: IOException =>
            alive.set(false)
            shutdown(true)
          case _: InterruptedException =>
            alive.set(false)
        }
        impl()
      }
    }
    impl()
  }, s"sbt-$name-write-thread")
  writeThread.setDaemon(true)
  writeThread.start()
  def publishBytes(event: Array[Byte], delimit: Boolean): Unit =
    try pendingWrites.put(event -> delimit)
    catch { case _: InterruptedException => }

  def onCommand(command: CommandMessage): Unit = {
    command match {
      case x: InitCommand             => onInitCommand(x)
      case x: ExecCommand             => onExecCommand(x)
      case x: SettingQuery            => onSettingQuery(None, x)
      case _: TerminalPropertiesQuery =>
    }
  }

  private def onInitCommand(cmd: InitCommand): Unit = {
    if (auth(ServerAuthentication.Token)) {
      cmd.token match {
        case Some(x) =>
          authenticate(x) match {
            case true =>
              initialized = true
              notifyEvent(ChannelAcceptedEvent(name))
            case _ => sys.error("invalid token")
          }
        case None => sys.error("init command but without token.")
      }
    } else {
      initialized = true
    }
  }

  private def onExecCommand(cmd: ExecCommand) = {
    if (initialized) {
      append(
        Exec(cmd.commandLine, cmd.execId orElse Some(Exec.newExecId), Some(CommandSource(name)))
      )
      ()
    } else {
      log.warn(s"ignoring command $cmd before initialization")
    }
  }

  protected def onSettingQuery(execId: Option[String], req: SettingQuery) = {
    if (initialized) {
      import sbt.protocol.codec.JsonProtocol._
      SettingQuery.handleSettingQueryEither(req, structure) match {
        case Right(x) => respondResult(x, execId)
        case Left(s)  => respondError(ErrorCodes.InvalidParams, s, execId)
      }
    } else {
      log.warn(s"ignoring query $req before initialization")
    }
  }

  protected def onCompletionRequest(execId: Option[String], cp: CompletionParams) = {
    if (initialized) {
      try {
        Option(EvaluateTask.lastEvaluatedState.get) match {
          case Some(sstate) =>
            import sbt.protocol.codec.JsonProtocol._
            def completionItems(s: State) = {
              Parser
                .completions(s.combinedParser, cp.query, cp.level.getOrElse(9))
                .get
                .flatMap { c =>
                  if (!c.isEmpty) Some(c.append.replaceAll("\n", " "))
                  else None
                }
                .map(c => cp.query + c)
            }
            val (items, cachedMainClassNames, cachedTestNames) = StandardMain.exchange.withState {
              s =>
                val scopedKeyParser: Parser[Seq[Def.ScopedKey[_]]] =
                  Act.aggregatedKeyParser(s) <~ Parsers.any.*
                Parser.parse(cp.query, scopedKeyParser) match {
                  case Right(keys) =>
                    val testKeys =
                      keys.filter(k => k.key.label == "testOnly" || k.key.label == "testQuick")
                    val (testState, cachedTestNames) = testKeys.foldLeft((s, true)) {
                      case ((st, allCached), k) =>
                        SessionVar.loadAndSet(sbt.Keys.definedTestNames in k.scope, st, true) match {
                          case (nst, d) => (nst, allCached && d.isDefined)
                        }
                    }
                    val runKeys = keys.filter(_.key.label == "runMain")
                    val (runState, cachedMainClassNames) = runKeys.foldLeft((testState, true)) {
                      case ((st, allCached), k) =>
                        SessionVar.loadAndSet(sbt.Keys.discoveredMainClasses in k.scope, st, true) match {
                          case (nst, d) => (nst, allCached && d.isDefined)
                        }
                    }
                    (completionItems(runState), cachedMainClassNames, cachedTestNames)
                  case _ => (completionItems(s), true, true)
                }
            }
            respondResult(
              CompletionResponse(
                items = items.toVector,
                cachedMainClassNames = cachedMainClassNames,
                cachedTestNames = cachedTestNames
              ),
              execId
            )
          case _ =>
            respondError(
              ErrorCodes.UnknownError,
              "No available sbt state",
              execId
            )
        }
      } catch {
        case NonFatal(_) =>
          respondError(
            ErrorCodes.UnknownError,
            "Completions request failed",
            execId
          )
      }
    } else {
      log.warn(s"ignoring completion request $cp before initialization")
    }
  }

  protected def onCancellationRequest(execId: Option[String], crp: CancelRequestParams) = {
    if (initialized) {

      def errorRespond(msg: String) = respondError(
        ErrorCodes.RequestCancelled,
        msg,
        execId
      )

      try {
        Option(EvaluateTask.currentlyRunningEngine.get) match {
          case Some((state, runningEngine)) =>
            val runningExecId = state.currentExecId.getOrElse("")
            val expected = StandardMain.exchange.withState(
              _.get(BasicCommands.execMap)
                .flatMap(s => s.get(crp.id) orElse s.get("\u2668" + crp.id))
                .getOrElse(crp.id)
            )

            def checkId(): Boolean = {
              if (runningExecId.startsWith("\u2668")) {
                (
                  Try { crp.id.toLong }.toOption,
                  Try { runningExecId.substring(1).toLong }.toOption
                ) match {
                  case (Some(id), Some(eid)) => id == eid
                  case _                     => false
                }
              } else runningExecId == expected
            }

            // direct comparison on strings and
            // remove hotspring unicode added character for numbers
            if (checkId || (crp.id == "__all" && StandardMain.exchange.currentExec.exists(
                  _.source.exists(_.channelName == name)
                ))) {
              runningEngine.cancelAndShutdown()

              import sbt.protocol.codec.JsonProtocol._
              respondResult(
                ExecStatusEvent(
                  "Task cancelled",
                  Some(name),
                  Some(runningExecId),
                  Vector(),
                  None,
                ),
                execId
              )
            } else {
              errorRespond("Task ID not matched")
            }

          case None =>
            errorRespond("No tasks under execution")
        }
      } catch {
        case NonFatal(_) =>
          errorRespond("Cancel request failed")
      }
    } else {
      log.warn(s"ignoring cancellation request $crp before initialization")
    }
  }

  @deprecated("Use variant that takes logShutdown parameter", "1.4.0")
  override def shutdown(): Unit = {
    shutdown(true)
  }
  import sjsonnew.BasicJsonProtocol.BooleanJsonFormat
  override def shutdown(logShutdown: Boolean): Unit = {
    terminal.close()
    StandardMain.exchange.removeChannel(this)
    super.shutdown(logShutdown)
    Terminal.consoleLog(s"shutting down client connection $name")
    pendingTerminalProperties.values.forEach { p =>
      Util.ignoreResult(p.offer(TerminalPropertiesResponse(0, 0, false, false, false, false)))
    }
    try jsonRpcNotify("shutdown", logShutdown)
    catch { case _: IOException => }
    running.set(false)
    out.close()
    thread.interrupt()
    writeThread.interrupt()
  }

  /**
   * This reacts to various events that happens inside sbt, sometime
   * in response to the previous requests.
   * The type information has been erased because it went through logging.
   */
  protected def onObjectEvent(event: ObjectEvent[_]): Unit = {
    // import sbt.internal.langserver.codec.JsonProtocol._

    val msgContentType = event.contentType
    msgContentType match {
      // LanguageServerReporter sends PublishDiagnosticsParams
      case "sbt.internal.langserver.PublishDiagnosticsParams" =>
      // val p = event.message.asInstanceOf[PublishDiagnosticsParams]
      // jsonRpcNotify("textDocument/publishDiagnostics", p)
      case "xsbti.Problem" =>
        () // ignore
      case _ =>
        // log.debug(event)
        ()
    }
  }

  /** Respond back to Language Server's client. */
  private[sbt] def jsonRpcRespond[A: JsonFormat](event: A, execId: String): Unit = {
    val m =
      JsonRpcResponseMessage("2.0", execId, Option(Converter.toJson[A](event).get), None)
    val bytes = Serialization.serializeResponseMessage(m)
    publishBytes(bytes)
  }

  /** Respond back to Language Server's client. */
  private[sbt] def jsonRpcRespondError[A: JsonFormat](
      execId: String,
      code: Long,
      message: String,
      data: A,
  ): Unit = {
    val err = JsonRpcResponseError(code, message, Converter.toJson[A](data).get)
    jsonRpcRespondError(execId, err)
  }

  private[sbt] def jsonRpcRespondError(
      execId: String,
      err: JsonRpcResponseError
  ): Unit = {
    val m = JsonRpcResponseMessage("2.0", execId, None, Option(err))
    val bytes = Serialization.serializeResponseMessage(m)
    publishBytes(bytes)
  }

  /** Notify to Language Server's client. */
  private[sbt] def jsonRpcNotify[A: JsonFormat](method: String, params: A): Unit = {
    val m =
      JsonRpcNotificationMessage("2.0", method, Option(Converter.toJson[A](params).get))
    log.debug(s"jsonRpcNotify: $m")
    val bytes = Serialization.serializeNotificationMessage(m)
    publishBytes(bytes)
  }

  def logMessage(level: String, message: String): Unit = {
    import sbt.internal.langserver.codec.JsonProtocol._
    jsonRpcNotify(
      "build/logMessage",
      LogMessageParams(MessageType.fromLevelString(level), message)
    )
  }
  private[this] lazy val pendingTerminalProperties =
    new ConcurrentHashMap[String, ArrayBlockingQueue[TerminalPropertiesResponse]]()
  private[this] lazy val pendingTerminalCapability =
    new ConcurrentHashMap[String, ArrayBlockingQueue[TerminalCapabilitiesResponse]]
  private[this] lazy val inputStream: InputStream = new InputStream {
    override def read(): Int = {
      try {
        inputBuffer.take & 0xFF match {
          case -1 => throw new ClosedChannelException()
          case b  => b
        }
      } catch { case _: IOException => -1 }
    }
    override def available(): Int = inputBuffer.size
  }
  import sjsonnew.BasicJsonProtocol._

  import scala.collection.JavaConverters._
  private[this] lazy val outputStream: OutputStream = new OutputStream {
    private[this] val buffer = new LinkedBlockingQueue[Byte]()
    override def write(b: Int): Unit = buffer.put(b.toByte)
    override def flush(): Unit = {
      jsonRpcNotify(Serialization.systemOut, buffer.asScala)
      buffer.clear()
    }
    override def write(b: Array[Byte]): Unit = write(b, 0, b.length)
    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      var i = off
      while (i < len) {
        buffer.put(b(i))
        i += 1
      }
    }
  }
  private class NetworkTerminal extends TerminalImpl(inputStream, outputStream, name) {
    private[this] val pending = new AtomicBoolean(false)
    private[this] val closed = new AtomicBoolean(false)
    private[this] val properties = new AtomicReference[TerminalPropertiesResponse]
    private[this] val lastUpdate = new AtomicReference[Deadline]
    private def empty = TerminalPropertiesResponse(0, 0, false, false, false, false)
    def getProperties(block: Boolean): Unit = {
      if (alive.get) {
        if (!pending.get && Option(lastUpdate.get).fold(true)(d => (d + 1.second).isOverdue)) {
          pending.set(true)
          val id = UUID.randomUUID.toString
          val queue = new ArrayBlockingQueue[TerminalPropertiesResponse](1)
          import sbt.protocol.codec.JsonProtocol._
          pendingTerminalProperties.put(id, queue)
          jsonRpcNotify(terminalPropertiesQuery, id)
          val update: Runnable = () => {
            queue.poll(5, java.util.concurrent.TimeUnit.SECONDS) match {
              case null =>
              case t    => properties.set(t)
            }
            pending.synchronized {
              lastUpdate.set(Deadline.now)
              pending.set(false)
              pending.notifyAll()
            }
          }
          new Thread(update, s"network-terminal-$name-update") {
            setDaemon(true)
          }.start()
        }
        while (block && properties.get == null) pending.synchronized(pending.wait())
        ()
      } else throw new InterruptedException
    }
    private def withThread[R](f: => R, default: R): R = {
      val t = Thread.currentThread
      try {
        blockedThreads.synchronized(blockedThreads.add(t))
        f
      } catch { case _: InterruptedException => default } finally {
        Util.ignoreResult(blockedThreads.synchronized(blockedThreads.remove(t)))
      }
    }
    def getProperty[T](f: TerminalPropertiesResponse => T, default: T): Option[T] = {
      if (closed.get) None
      else
        withThread({
          getProperties(true);
          Some(f(Option(properties.get).getOrElse(empty)))
        }, None)
    }
    private[this] def waitForPending(f: TerminalPropertiesResponse => Boolean): Boolean = {
      if (closed.get) false
      withThread(
        {
          if (pending.get) pending.synchronized(pending.wait())
          f(properties.get)
        },
        false
      )
    }
    private[this] val blockedThreads = ConcurrentHashMap.newKeySet[Thread]
    override def getWidth: Int = getProperty(_.width, 0).getOrElse(0)
    override def getHeight: Int = getProperty(_.height, 0).getOrElse(0)
    override def isAnsiSupported: Boolean = getProperty(_.isAnsiSupported, false).getOrElse(false)
    override def isEchoEnabled: Boolean = waitForPending(_.isEchoEnabled)
    override def isSuccessEnabled: Boolean =
      prompt != Prompt.Batch || ContinuousCommands.isInWatch(NetworkChannel.this)
    override lazy val isColorEnabled: Boolean = waitForPending(_.isColorEnabled)
    override lazy val isSupershellEnabled: Boolean = waitForPending(_.isSupershellEnabled)
    getProperties(false)
    private def getCapability[T](
        capability: String,
        query: String => TerminalCapabilitiesQuery,
        result: TerminalCapabilitiesResponse => T
    ): Option[T] = {
      if (closed.get) None
      else {
        val id = UUID.randomUUID.toString
        val queue = new ArrayBlockingQueue[TerminalCapabilitiesResponse](1)
        import sbt.protocol.codec.JsonProtocol._
        pendingTerminalCapability.put(id, queue)
        jsonRpcNotify(terminalCapabilities, query(id))
        Some(result(queue.take))
      }
    }
    override def getBooleanCapability(capability: String): Boolean =
      getCapability(
        capability,
        TerminalCapabilitiesQuery(_, boolean = Some(capability), numeric = None, string = None),
        _.boolean.getOrElse(false)
      ).getOrElse(false)
    override def getNumericCapability(capability: String): Int =
      getCapability(
        capability,
        TerminalCapabilitiesQuery(_, boolean = None, numeric = Some(capability), string = None),
        _.numeric.getOrElse(-1)
      ).getOrElse(-1)
    override def getStringCapability(capability: String): String =
      getCapability(
        capability,
        TerminalCapabilitiesQuery(_, boolean = None, numeric = None, string = Some(capability)),
        _.string.flatMap {
          case "null" => None
          case s      => Some(s)
        }.orNull
      ).getOrElse("")

    override def toString: String = s"NetworkTerminal($name)"
    override def close(): Unit = if (closed.compareAndSet(false, true)) {
      val threads = blockedThreads.synchronized {
        val t = blockedThreads.asScala.toVector
        blockedThreads.clear()
        t
      }
      threads.foreach(_.interrupt())
      super.close()
    }
  }
  private[sbt] def isAttached: Boolean = attached.get
}

object NetworkChannel {
  sealed trait ChannelState
  case object SingleLine extends ChannelState
  case object InHeader extends ChannelState
  case object InBody extends ChannelState
  private[sbt] def cancel(
      execID: Option[String],
      id: String
  ): Either[String, String] = {

    Option(EvaluateTask.currentlyRunningEngine.get) match {
      case Some((state, runningEngine)) =>
        val runningExecId = state.currentExecId.getOrElse("")

        def checkId(): Boolean = {
          if (runningExecId.startsWith("\u2668")) {
            (
              Try { id.toLong }.toOption,
              Try { runningExecId.substring(1).toLong }.toOption
            ) match {
              case (Some(id), Some(eid)) => id == eid
              case _                     => false
            }
          } else runningExecId == id
        }

        // direct comparison on strings and
        // remove hotspring unicode added character for numbers
        if (checkId) {
          runningEngine.cancelAndShutdown()
          Right(runningExecId)
        } else {
          Left("Task ID not matched")
        }

      case None =>
        Left("No tasks under execution")
    }
  }

  private[sbt] val disconnect: Command =
    Command.arb { s =>
      val dncParser: Parser[String] = BasicCommandStrings.DisconnectNetworkChannel
      dncParser.examples() ~> Parsers.Space.examples() ~> Parsers.any.*.examples()
    } { (st, channel) =>
      StandardMain.exchange.killChannel(channel.mkString)
      st
    }
}
