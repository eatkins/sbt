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
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ ArrayBlockingQueue, ConcurrentHashMap, LinkedBlockingQueue }

import sbt.internal.langserver.{ CancelRequestParams, ErrorCodes }
import sbt.internal.util.{ ManagedLogger, ObjectEvent, StringEvent, Terminal }
import sbt.internal.util.complete.Parser
import sbt.internal.util.codec.JValueFormats
import sbt.internal.protocol.{
  JsonRpcResponseError,
  JsonRpcRequestMessage,
  JsonRpcNotificationMessage
}
import sbt.internal.ui.UserThread
import sbt.internal.util.Terminal.TerminalImpl
import sbt.internal.util.codec.JValueFormats
import sbt.internal.util.complete.Parser
import sbt.internal.util.{ ObjectEvent, StringEvent, Terminal }
import sbt.protocol._
import sbt.util.{ Level, Logger }
import sjsonnew._
import sjsonnew.support.scalajson.unsafe.Converter

import scala.annotation.tailrec
import scala.util.Try
import scala.util.control.NonFatal

final class NetworkChannel(
    val name: String,
    connection: Socket,
    structure: BuildStructure,
    auth: Set[ServerAuthentication],
    instance: ServerInstance,
    handlers: Seq[ServerHandler],
) extends CommandChannel
    with LanguageServerProtocol {
  def this(
      name: String,
      connection: Socket,
      structure: BuildStructure,
      auth: Set[ServerAuthentication],
      instance: ServerInstance,
      handlers: Seq[ServerHandler],
      log: Logger
  ) = this(name, connection, structure, auth, instance, handlers)
  import NetworkChannel._

  private[this] val inputBuffer = new LinkedBlockingQueue[Byte]()
  private[this] val attached = new AtomicBoolean(false)
  private val running = new AtomicBoolean(true)
  private val delimiter: Byte = '\n'.toByte
  private val RetByte = '\r'.toByte
  private val out = connection.getOutputStream
  private var initialized = false
  private val Curly = '{'.toByte
  private val ContentLength = """^Content\-Length\:\s*(\d+)""".r
  private val ContentType = """^Content\-Type\:\s*(.+)""".r
  private var _contentType: String = ""
  private val SbtX1Protocol = "application/sbt-x1"
  private val VsCode = sbt.protocol.Serialization.VsCode
  private val VsCodeOld = "application/vscode-jsonrpc; charset=utf8"
  private lazy val jsonFormat = new sjsonnew.BasicJsonProtocol with JValueFormats {}
  private[this] val alive = new AtomicBoolean(true)

  override def log: Logger = new Logger {
    override def trace(t: => Throwable): Unit = {}
    override def success(message: => String): Unit = {}
    override def log(level: Level.Value, message: => String): Unit = {}
  }
  override val terminal: Terminal = new NetworkTerminal

  def setContentType(ct: String): Unit = synchronized { _contentType = ct }
  def contentType: String = _contentType

  protected def authenticate(token: String): Boolean = instance.authenticate(token)

  protected def setInitialized(value: Boolean): Unit = initialized = value

  protected def authOptions: Set[ServerAuthentication] = auth

  val thread = new Thread(s"sbt-networkchannel-${connection.getPort}") {
    var contentLength: Int = 0
    var state: ChannelState = SingleLine

    override def run(): Unit = {
      try {
        val readBuffer = new Array[Byte](4096)
        val in = connection.getInputStream
        connection.setSoTimeout(5000)
        var buffer: Vector[Byte] = Vector.empty
        var bytesRead = 0
        def resetChannelState(): Unit = {
          contentLength = 0
          state = SingleLine
        }
        def tillEndOfLine: Option[Vector[Byte]] = {
          val delimPos = buffer.indexOf(delimiter)
          if (delimPos > 0) {
            val chunk0 = buffer.take(delimPos)
            buffer = buffer.drop(delimPos + 1)
            // remove \r at the end of line.
            if (chunk0.size > 0 && chunk0.indexOf(RetByte) == chunk0.size - 1)
              Some(chunk0.dropRight(1))
            else Some(chunk0)
          } else None // no EOL yet, so skip this turn.
        }

        def tillContentLength: Option[Vector[Byte]] = {
          if (contentLength <= buffer.size) {
            val chunk = buffer.take(contentLength)
            buffer = buffer.drop(contentLength)
            resetChannelState()
            Some(chunk)
          } else None // have not read enough yet, so skip this turn.
        }

        @tailrec def process(): Unit = {
          // handle un-framing
          state match {
            case SingleLine =>
              tillEndOfLine match {
                case Some(chunk) =>
                  chunk.headOption match {
                    case None        => // ignore blank line
                    case Some(Curly) =>
                      // When Content-Length header is not found, interpret the line as JSON message.
                      handleBody(chunk)
                      process()
                    case Some(_) =>
                      val str = (new String(chunk.toArray, "UTF-8")).trim
                      handleHeader(str) match {
                        case Some(_) =>
                          state = InHeader
                          process()
                        case _ => log.error("Got invalid chunk from client: " + str)
                      }
                  }
                case _ => ()
              }
            case InHeader =>
              tillEndOfLine match {
                case Some(chunk) =>
                  val str = (new String(chunk.toArray, "UTF-8")).trim
                  if (str == "") {
                    state = InBody
                    process()
                  } else
                    handleHeader(str) match {
                      case Some(_) => process()
                      case _ =>
                        log.error("Got invalid header from client: " + str)
                        resetChannelState()
                    }
                case _ => ()
              }
            case InBody =>
              tillContentLength match {
                case Some(chunk) =>
                  handleBody(chunk)
                  process()
                case _ => ()
              }
          }
        }

        // keep going unless the socket has closed
        while (bytesRead != -1 && running.get) {
          try {
            bytesRead = in.read(readBuffer)
            // log.debug(s"bytesRead: $bytesRead")
            if (bytesRead > 0) {
              buffer = buffer ++ readBuffer.toVector.take(bytesRead)
            }
            process()
          } catch {
            case _: SocketTimeoutException => // its ok
          }
        } // while
      } finally {
        shutdown(false)
      }
    }

    private lazy val intents = {
      val cb = callbackImpl
      handlers.toVector map { h =>
        h.handler(cb)
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

    def handleBody(chunk: Vector[Byte]): Unit = {
      if (isLanguageServerProtocol) {
        Serialization.deserializeJsonMessage(chunk) match {
          case Right(req: JsonRpcRequestMessage) =>
            req.method match {
              case "inputStream" =>
                import sjsonnew.BasicJsonProtocol._
                val byte = req.params.flatMap(Converter.fromJson[Byte](_).toOption)
                byte.foreach(inputBuffer.put)
              case "sbt/terminalpropsresponse" =>
                import sbt.protocol.codec.JsonProtocol._
                val response =
                  req.params.flatMap(Converter.fromJson[TerminalPropertiesResponse](_).toOption)
                pendingTerminalProperties.remove(req.id) match {
                  case null   =>
                  case buffer => response.foreach(buffer.put)
                }
              case "sbt/terminalcapresponse" =>
                import sbt.protocol.codec.JsonProtocol._
                val response =
                  req.params.flatMap(Converter.fromJson[TerminalCapabilitiesResponse](_).toOption)
                pendingTerminalCapability.remove(req.id) match {
                  case null =>
                  case buffer =>
                    buffer.put(
                      response.getOrElse(TerminalCapabilitiesResponse("", None, None, None))
                    )
                }
              case "attach" =>
                attached.set(true)
                initiateMaintenance("attach")
              case _ =>
                try {
                  onRequestMessage(req)
                } catch {
                  case LangServerError(code, message) =>
                    log.debug(s"sending error: $code: $message")
                    jsonRpcRespondError(Option(req.id), code, message)
                }
            }
          case Right(ntf: JsonRpcNotificationMessage) =>
            try {
              onNotification(ntf)
            } catch {
              case LangServerError(code, message) =>
                log.debug(s"sending error: $code: $message")
                jsonRpcRespondError(None, code, message) // new id?
            }
          case Right(msg) =>
            log.debug(s"Unhandled message: $msg")
          case Left(errorDesc) =>
            val msg = s"Got invalid chunk from client (${new String(chunk.toArray, "UTF-8")}): " + errorDesc
            jsonRpcRespondError(None, ErrorCodes.ParseError, msg)
        }
      } else {
        contentType match {
          case SbtX1Protocol =>
            Serialization
              .deserializeCommand(chunk)
              .fold(
                errorDesc =>
                  log.error(
                    s"Got invalid chunk from client (${new String(chunk.toArray, "UTF-8")}): " + errorDesc
                  ),
                onCommand
              )
          case _ =>
            log.error(s"Unknown Content-Type: $contentType")
        }
      } // if-else
    }

    def handleHeader(str: String): Option[Unit] = {
      str match {
        case ContentLength(len) =>
          contentLength = len.toInt
          Some(())
        case ContentType(ct) =>
          setContentType(ct)
          Some(())
        case _ => None
      }
    }
  }
  thread.start()

  private[sbt] def isLanguageServerProtocol: Boolean = {
    contentType match {
      case "" | VsCode | VsCodeOld => true
      case _                       => false
    }
  }

  private[sbt] def respondError(
      err: JsonRpcResponseError,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = jsonRpcRespondError(execId, err)

  private[sbt] def respondError(
      code: Long,
      message: String,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = jsonRpcRespondError(execId, code, message)

  private[sbt] def respondEvent[A: JsonFormat](
      event: A,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = jsonRpcRespond(event, execId)

  private[sbt] def notifyEvent[A: JsonFormat](method: String, params: A): Unit = {
    if (isLanguageServerProtocol) {
      jsonRpcNotify(method, params)
    } else {
      ()
    }
  }

  def publishEvent[A: JsonFormat](event: A, execId: Option[String]): Unit = if (alive.get) {
    if (isLanguageServerProtocol) {
      event match {
        case entry: StringEvent => logMessage(entry.level, entry.message)
        case entry: ExecStatusEvent =>
          entry.exitCode match {
            case None    => jsonRpcRespond(event, entry.execId)
            case Some(0) => jsonRpcRespond(event, entry.execId)
            case Some(exitCode) =>
              jsonRpcRespondError(entry.execId, exitCode, entry.message.getOrElse(""))
          }
        case _ => jsonRpcRespond(event, execId)
      }
    } else {
      contentType match {
        case SbtX1Protocol =>
          val bytes = Serialization.serializeEvent(event)
          publishBytes(bytes, true)
        case _ =>
      }
    }
  }

  def publishEventMessage(event: EventMessage): Unit = if (alive.get) {
    event match {
      case ConsolePromptEvent(state) if isAttached => reset(state, UserThread.Ready)
      case ConsoleUnpromptEvent(lastSource, state) =>
        terminal.progressState.reset()
        if (lastSource.fold(true)(_.channelName != name))
          reset(state, UserThread.Blocked)
      case _ if isLanguageServerProtocol =>
        event match {
          case entry: LogEvent        => logMessage(entry.level, entry.message)
          case entry: ExecStatusEvent => logMessage("debug", entry.status)
          case _                      => ()
        }
      case _ =>
        contentType match {
          case SbtX1Protocol =>
            val bytes = Serialization.serializeEventMessage(event)
            publishBytes(bytes, true)
          case _ => ()
        }
    }
  }

  /**
   * This publishes object events. The type information has been
   * erased because it went through logging.
   */
  private[sbt] def publishObjectEvent(event: ObjectEvent[_]): Unit = if (alive.get) {
    import sjsonnew.shaded.scalajson.ast.unsafe._
    if (isLanguageServerProtocol) onObjectEvent(event)
    else {
      import jsonFormat._
      val json: JValue = JObject(
        JField("type", JString(event.contentType)),
        (Vector(JField("message", event.json), JField("level", JString(event.level.toString))) ++
          (event.channelName.toVector map { channelName =>
            JField("channelName", JString(channelName))
          }) ++
          (event.execId.toVector map { execId =>
            JField("execId", JString(execId))
          })): _*
      )
      publishEvent(json)
    }
  }

  def publishBytes(event: Array[Byte]): Unit = publishBytes(event, false)

  def publishBytes(event: Array[Byte], delimit: Boolean): Unit =
    if (alive.get)
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
      }

  def onCommand(command: CommandMessage): Unit = {
    command match {
      case x: InitCommand             => onInitCommand(x)
      case x: ExecCommand             => onExecCommand(x)
      case x: SettingQuery            => onSettingQuery(None, x)
      case _: Attach                  => attached.set(true)
      case x: TerminalPropertiesQuery =>
    }
  }

  private def onInitCommand(cmd: InitCommand): Unit = {
    if (auth(ServerAuthentication.Token)) {
      cmd.token match {
        case Some(x) =>
          authenticate(x) match {
            case true =>
              initialized = true
              publishEventMessage(ChannelAcceptedEvent(name))
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
        case Right(x) => jsonRpcRespond(x, execId)
        case Left(s)  => jsonRpcRespondError(execId, ErrorCodes.InvalidParams, s)
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
            val completionItems =
              Parser
                .completions(sstate.combinedParser, cp.query, cp.level.getOrElse(9))
                .get
                .flatMap(c => {
                  if (!c.isEmpty) Some(cp.query + c.append.replaceAll("\n", " "))
                  else None
                })
            import sbt.protocol.codec.JsonProtocol._
            jsonRpcRespond(CompletionResponse(items = completionItems.toVector), execId)
          case _ =>
            jsonRpcRespondError(
              execId,
              ErrorCodes.UnknownError,
              "No available sbt state"
            )
        }
      } catch {
        case NonFatal(e) =>
          jsonRpcRespondError(
            execId,
            ErrorCodes.UnknownError,
            "Completions request failed"
          )
      }
    } else {
      log.warn(s"ignoring completion request $cp before initialization")
    }
  }

  protected def onCancellationRequest(execId: Option[String], crp: CancelRequestParams) = {
    if (initialized) {

      def errorRespond(msg: String) = jsonRpcRespondError(
        execId,
        ErrorCodes.RequestCancelled,
        msg
      )

      try {
        NetworkChannel.cancel(execId, crp.id) match {
          case Left(msg) => errorRespond(msg)
          case Right(runningExecId) =>
            import sbt.protocol.codec.JsonProtocol._
            jsonRpcRespond(
              ExecStatusEvent(
                "Task cancelled",
                Some(name),
                Some(runningExecId),
                Vector(),
                None,
              ),
              execId
            )
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
    super.shutdown()
    shutdown()
  }
  import sjsonnew.BasicJsonProtocol.BooleanJsonFormat
  override def shutdown(logShutdown: Boolean): Unit = {
    log.info(s"Shutting down client connection $name")
    try jsonRpcNotify("shutdown", logShutdown)
    catch { case _: IOException => }
    running.set(false)
    out.close()
  }
  private[this] lazy val pendingTerminalProperties =
    new ConcurrentHashMap[String, ArrayBlockingQueue[TerminalPropertiesResponse]]()
  private[this] lazy val pendingTerminalCapability =
    new ConcurrentHashMap[String, ArrayBlockingQueue[TerminalCapabilitiesResponse]]
  private[this] lazy val inputStream: InputStream = new InputStream {
    override def read(): Int = {
      try {
        // if (askUserThread != null) jsonRpcNotify("readInput", true) TODO -- fix condition
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
      jsonRpcNotify("systemOut", buffer.asScala)
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
    def getProperties: TerminalPropertiesResponse = {
      val id = UUID.randomUUID.toString
      val queue = new ArrayBlockingQueue[TerminalPropertiesResponse](1)
      import sbt.protocol.codec.JsonProtocol._
      pendingTerminalProperties.put(id, queue)
      jsonRpcNotify("sbt/terminalprops", id)
      queue.take
    }
    override def getWidth: Int = getProperties.width
    override def getHeight: Int = getProperties.height
    override def isAnsiSupported: Boolean = getProperties.isAnsiSupported
    override def isEchoEnabled: Boolean = getProperties.isEchoEnabled
    override def isColorEnabled: Boolean = getProperties.isColorEnabled
    override def isSupershellEnabled: Boolean = getProperties.isSupershellEnabled
    private def getCapability[T](
        capability: String,
        query: String => TerminalCapabilitiesQuery,
        result: TerminalCapabilitiesResponse => T
    ): T = {
      val id = UUID.randomUUID.toString
      val queue = new ArrayBlockingQueue[TerminalCapabilitiesResponse](1)
      import sbt.protocol.codec.JsonProtocol._
      pendingTerminalCapability.put(id, queue)
      jsonRpcNotify("sbt/terminalcap", query(id))
      result(queue.take)
    }
    override def getBooleanCapability(capability: String): Boolean = getCapability(
      capability,
      TerminalCapabilitiesQuery(_, boolean = Some(capability), numeric = None, string = None),
      _.boolean.getOrElse(false)
    )
    override def getNumericCapability(capability: String): Int = getCapability(
      capability,
      TerminalCapabilitiesQuery(_, boolean = None, numeric = Some(capability), string = None),
      _.numeric.getOrElse(-1)
    )
    override def getStringCapability(capability: String): String =
      getCapability(
        capability,
        TerminalCapabilitiesQuery(_, boolean = None, numeric = None, string = Some(capability)),
        _.string.orNull
      )
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
}
