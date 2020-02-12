/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.io.IOException
import java.net.Socket
import java.util.concurrent.atomic._
import java.util.concurrent.{ LinkedBlockingQueue, TimeUnit }

import sbt.BasicKeys._
import sbt.nio.Watch.NullLogger
import sbt.internal.protocol.JsonRpcResponseError
import sbt.internal.langserver.{ LogMessageParams, MessageType }
import sbt.internal.server._
import sbt.internal.ui.{ AskUserTask, UITask }
import sbt.internal.util._
import sbt.internal.util.codec.JValueFormats
import sbt.io.syntax._
import sbt.io.{ Hash, IO }
import sbt.nio.Watch.NullLogger
import sbt.protocol.{ EventMessage, ExecStatusEvent }
import sbt.util.Logger
import sjsonnew.JsonFormat
import sjsonnew.shaded.scalajson.ast.unsafe._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

/**
 * The command exchange merges multiple command channels (e.g. network and console),
 * and acts as the central multiplexing point.
 * Instead of blocking on JLine.readLine, the server command will block on
 * this exchange, which could serve command request from either of the channel.
 */
private[sbt] final class CommandExchange {
  private val autoStartServerSysProp =
    sys.props get "sbt.server.autostart" forall (_.toLowerCase == "true")
  private var server: Option[ServerInstance] = None
  private val firstInstance: AtomicBoolean = new AtomicBoolean(true)
  private var consoleChannel: Option[ConsoleChannel] = None
  private val commandQueue: LinkedBlockingQueue[Exec] = new LinkedBlockingQueue[Exec]
  private val channelBuffer: ListBuffer[CommandChannel] = new ListBuffer()
  private val channelBufferLock = new AnyRef {}
  private val maintenanceChannelQueue = new LinkedBlockingQueue[MaintenanceTask]
  private val nextChannelId: AtomicInteger = new AtomicInteger(0)
  private lazy val jsonFormat = new sjsonnew.BasicJsonProtocol with JValueFormats {}
  private[this] val lastState = new AtomicReference[State]
  private[this] val currentExec = new AtomicReference[Exec]
  private[this] val lastProgressEvent = new AtomicReference[ProgressEvent]

  def channels: List[CommandChannel] = channelBuffer.toList
  private[this] def removeChannels(toDel: List[CommandChannel]): Unit = {
    toDel match {
      case Nil => // do nothing
      case xs =>
        channelBufferLock.synchronized {
          channelBuffer --= xs
          ()
        }
    }
  }

  def subscribe(c: CommandChannel): Unit = channelBufferLock.synchronized {
    channelBuffer.append(c)
    c.register(commandQueue, maintenanceChannelQueue)
  }

  private[sbt] def withState[T](f: State => T): T = f(lastState.get)
  def blockUntilNextExec: Exec = blockUntilNextExec(Duration.Inf, NullLogger)
  // periodically move all messages from all the channels
  private[sbt] def blockUntilNextExec(interval: Duration, logger: Logger): Exec =
    blockUntilNextExec(interval, None, logger)
  private[sbt] def blockUntilNextExec(
      interval: Duration,
      state: Option[State],
      logger: Logger
  ): Exec = {
    state.foreach(lastState.set)
    @tailrec def impl(deadline: Option[Deadline]): Exec = {
      state.foreach(s => channels.foreach(_.publishEventMessage(ConsolePromptEvent(s))))
      def poll: Option[Exec] =
        Option(deadline match {
          case Some(d: Deadline) => commandQueue.poll(d.timeLeft.toMillis, TimeUnit.MILLISECONDS)
          case _                 => commandQueue.take
        })
      poll match {
        case Some(exec) =>
          exec.commandLine match {
            case "shutdown" => exec.withCommandLine("exit")
            case "exit" if exec.source.fold(false)(_.channelName.startsWith("network")) =>
              channels.collectFirst {
                case c: NetworkChannel if exec.source.fold(false)(_.channelName == c.name) => c
              } match {
                case Some(c) if c.isAttached =>
                  c.shutdown(false)
                  impl(deadline)
                case _ => exec
              }
            case _ => exec
          }
        case None =>
          val newDeadline = if (deadline.fold(false)(_.isOverdue())) {
            GCUtil.forceGcWithInterval(interval, logger)
            None
          } else deadline
          impl(newDeadline)
      }
    }
    // Do not manually run GC until the user has been idling for at least the min gc interval.
    val res = impl(interval match {
      case d: FiniteDuration => Some(d.fromNow)
      case _                 => None
    })
    currentExec.set(res)
    res
  }

  def run(s: State): State = {
    if (consoleChannel.isEmpty) {
      val name = "console0"
      val console0 = new ConsoleChannel(name, mkAskUser(name))
      consoleChannel = Some(console0)
      subscribe(console0)
    }
    val autoStartServerAttr = s get autoStartServer match {
      case Some(bool) => bool
      case None       => true
    }
    if (autoStartServerSysProp && autoStartServerAttr) runServer(s)
    else s
  }
  private[sbt] def setState(s: State): Unit = lastState.set(s)

  private def newNetworkName: String = s"network-${nextChannelId.incrementAndGet()}"

  private[sbt] def removeChannel(c: CommandChannel): Unit = channelBufferLock.synchronized {
    Util.ignoreResult(channelBuffer -= c)
  }

  private[this] def mkAskUser(
      name: String,
  ): (State, CommandChannel) => UITask = { (state, channel) =>
    ContinuousCommands.watchUIThreadFor(channel).getOrElse(new AskUserTask(state, channel))
  }

  /**
   * Check if a server instance is running already, and start one if it isn't.
   */
  private[sbt] def runServer(s: State): State = {
    lazy val port = s.get(serverPort).getOrElse(5001)
    lazy val host = s.get(serverHost).getOrElse("127.0.0.1")
    lazy val auth: Set[ServerAuthentication] =
      s.get(serverAuthentication).getOrElse(Set(ServerAuthentication.Token))
    lazy val connectionType = s.get(serverConnectionType).getOrElse(ConnectionType.Tcp)
    lazy val handlers = s.get(fullServerHandlers).getOrElse(Nil)

    def onIncomingSocket(socket: Socket, instance: ServerInstance): Unit = {
      val name = newNetworkName
      s.log.debug(s"new client connected: $name")
      val channel =
        new NetworkChannel(
          name,
          socket,
          Project structure s,
          auth,
          instance,
          handlers,
          mkAskUser(name)
        )
      subscribe(channel)
    }
    if (server.isEmpty && firstInstance.get) {
      val portfile = s.baseDir / "project" / "target" / "active.json"
      val h = Hash.halfHashString(IO.toURI(portfile).toString)
      val serverDir =
        sys.env get "SBT_GLOBAL_SERVER_DIR" map file getOrElse BuildPaths.getGlobalBase(s) / "server"
      val tokenfile = serverDir / h / "token.json"
      val socketfile = serverDir / h / "sock"
      val pipeName = "sbt-server-" + h
      val connection = ServerConnection(
        connectionType,
        host,
        port,
        auth,
        portfile,
        tokenfile,
        socketfile,
        pipeName,
      )
      val serverInstance = Server.start(connection, onIncomingSocket, s.log)
      // don't throw exception when it times out
      val d = "10s"
      Try(Await.ready(serverInstance.ready, Duration(d)))
      serverInstance.ready.value match {
        case Some(Success(())) =>
          // remember to shutdown only when the server comes up
          server = Some(serverInstance)
        case Some(Failure(_: AlreadyRunningException)) =>
          s.log.warn(
            "sbt server could not start because there's another instance of sbt running on this build."
          )
          s.log.warn("Running multiple instances is unsupported")
          server = None
          firstInstance.set(false)
        case Some(Failure(e)) =>
          s.log.error(e.toString)
          server = None
        case None =>
          s.log.warn(s"sbt server could not start in $d")
          server = None
          firstInstance.set(false)
      }
      Terminal.close()
    }
    s
  }

  def shutdown(): Unit = {
    maintenanceThread.close()
    channels foreach (_.shutdown(true))
    // interrupt and kill the thread
    server.foreach(_.shutdown())
    server = None
  }

  // This is an interface to directly respond events.
  private[sbt] def respondError(
      code: Long,
      message: String,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = {
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty
    channels.foreach {
      case _: ConsoleChannel =>
      case c: NetworkChannel =>
        try {
          // broadcast to all network channels
          c.respondError(code, message, execId, source)
        } catch {
          case _: IOException =>
            toDel += c
        }
    }
    removeChannels(toDel.toList)
  }

  private[sbt] def respondError(
      err: JsonRpcResponseError,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = {
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty
    channels.foreach {
      case _: ConsoleChannel =>
      case c: NetworkChannel =>
        try {
          // broadcast to all network channels
          c.respondError(err, execId, source)
        } catch {
          case _: IOException =>
            toDel += c
        }
    }
    removeChannels(toDel.toList)
  }

  // This is an interface to directly respond events.
  private[sbt] def respondEvent[A: JsonFormat](
      event: A,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = {
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty
    channels.foreach {
      case _: ConsoleChannel =>
      case c: NetworkChannel =>
        try {
          // broadcast to all network channels
          c.respondEvent(event, execId, source)
        } catch {
          case _: IOException =>
            toDel += c
        }
    }
    removeChannels(toDel.toList)
  }

  private[sbt] def getLoggerFor(exec: Exec): Option[ManagedLogger] =
    channels.find(c => exec.source.map(_.channelName).contains(c.name)) match {
      case Some(c) =>
        println(s"fuck found $c"); /*Some(c.logger) */
        None
      case None => println(s"no channel for $exec"); None
    }

  // This is an interface to directly notify events.
  private[sbt] def notifyEvent[A: JsonFormat](method: String, params: A): Unit = {
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty
    channels.foreach {
      case _: ConsoleChannel =>
      // c.publishEvent(event)
      case c: NetworkChannel =>
        try {
          c.notifyEvent(method, params)
        } catch {
          case _: IOException =>
            toDel += c
        }
    }
    removeChannels(toDel.toList)
  }

  private[sbt] def channelForName(channelName: String): Option[CommandChannel] =
    channels.find(_.name == channelName)

  private def tryTo(x: => Unit, c: CommandChannel, toDel: ListBuffer[CommandChannel]): Unit =
    try x
    catch { case _: IOException => toDel += c }

  def publishEvent[A: JsonFormat](event: A): Unit = {
    val broadcastStringMessage = true
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty

    event match {
      case entry: StringEvent =>
        val params = toLogMessageParams(entry)
        channels collect {
          case c: ConsoleChannel =>
            if (broadcastStringMessage || (entry.channelName forall (_ == c.name)))
              c.publishEvent(event)
          case c: NetworkChannel =>
            tryTo(
              {
                // Note that language server's LogMessageParams does not hold the execid,
                // so this is weaker than the StringMessage. We might want to double-send
                // in case we have a better client that can utilize the knowledge.
                import sbt.internal.langserver.codec.JsonProtocol._
                if (entry.channelName.contains(c.name))
                  c.jsonRpcNotify("window/logMessage", params)
              },
              c,
              toDel
            )
        }
      case entry: ExecStatusEvent =>
        channels collect {
          case c: ConsoleChannel =>
            if (entry.channelName forall (_ == c.name)) c.publishEvent(event)
          case c: NetworkChannel =>
            if (entry.channelName contains c.name) tryTo(c.publishEvent(event), c, toDel)
        }
      case _ =>
        channels foreach {
          case c: ConsoleChannel => c.publishEvent(event)
          case c: NetworkChannel =>
            tryTo(c.publishEvent(event), c, toDel)
        }
    }
    removeChannels(toDel.toList)
  }

  private[sbt] def toLogMessageParams(event: StringEvent): LogMessageParams = {
    LogMessageParams(MessageType.fromLevelString(event.level), event.message)
  }

  /**
   * This publishes object events. The type information has been
   * erased because it went through logging.
   */
  private[sbt] def publishObjectEvent(event: ObjectEvent[_]): Unit = {
    import jsonFormat._
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty
    def json: JValue = JObject(
      JField("type", JString(event.contentType)),
      Vector(JField("message", event.json), JField("level", JString(event.level.toString))) ++
        (event.channelName.toVector map { channelName =>
          JField("channelName", JString(channelName))
        }) ++
        (event.execId.toVector map { execId =>
          JField("execId", JString(execId))
        }): _*
    )
    channels collect {
      case c: ConsoleChannel =>
        c.publishEvent(json)
      case c: NetworkChannel =>
        try {
          c.publishObjectEvent(event)
        } catch {
          case _: IOException =>
            toDel += c
        }
    }
    removeChannels(toDel.toList)
  }

  // fanout publishEvent
  def publishEventMessage(event: EventMessage): Unit = {
    val toDel: ListBuffer[CommandChannel] = ListBuffer.empty

    event match {
      // Special treatment for ConsolePromptEvent since it's hand coded without codec.
      case entry: ConsolePromptEvent =>
        channels collect {
          case c @ (_: ConsoleChannel | _: NetworkChannel) => c.publishEventMessage(entry)
        }
      case entry: ExecStatusEvent =>
        channels collect {
          case c: ConsoleChannel =>
            if (entry.channelName forall (_ == c.name)) c.publishEventMessage(event)
          case c: NetworkChannel =>
            if (entry.channelName contains c.name) tryTo(c.publishEventMessage(event), c, toDel)
        }
      case _ =>
        channels collect {
          case c: ConsoleChannel => c.publishEventMessage(event)
          case c: NetworkChannel => tryTo(c.publishEventMessage(event), c, toDel)
        }
    }

    removeChannels(toDel.toList)
  }
  private[sbt] def updateProgress(pe: ProgressEvent): Unit = {
    lastProgressEvent.set(pe)
    channels.foreach(c => ProgressState.updateProgressState(pe, c.terminal))
  }
  private[this] class MaintenanceThread
      extends Thread("sbt-command-exchange-maintenance")
      with AutoCloseable {
    setDaemon(true)
    start()
    private[this] val isStopped = new AtomicBoolean(false)
    private[this] def cancel(e: Exec): Unit = {
      if (e.commandLine.startsWith("console")) {
        val terminal = Terminal.get
        terminal.write(13, 4)
        terminal.printStream.println("\nconsole session killed by remote sbt client")
      } else {
        Util.ignoreResult(NetworkChannel.cancel(e.execId, e.execId.getOrElse("0")))
      }
    }
    override def run(): Unit = {
      @tailrec def impl(): Unit = {
        maintenanceChannelQueue.take match {
          case null =>
          case mt: MaintenanceTask =>
            mt.task match {
              case "attach"                 => mt.channel.publishEventMessage(ConsolePromptEvent(lastState.get))
              case k if k == "kill channel" => mt.channel.shutdown(false)
              case k if k.startsWith("kill") =>
                val name = k.split("kill[ ]+").lastOption
                Option(currentExec.get).filter(e => name.contains(e.commandLine)).foreach(cancel)
              case "exit" => mt.channel.shutdown(false)
              case "shutdown" =>
                Option(currentExec.get).foreach(cancel)
                commandQueue.clear()
                val exit =
                  Exec("shutdown", Some(Exec.newExecId), Some(CommandSource(mt.channel.name)))
                commandQueue.add(exit)
              case _ =>
            }
        }
        if (!isStopped.get) impl()
      }
      try impl()
      catch { case _: InterruptedException => }
    }
    override def close(): Unit = if (isStopped.compareAndSet(false, true)) {
      interrupt()
    }
  }
  private[this] val maintenanceThread = new MaintenanceThread
}
