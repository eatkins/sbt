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
import sbt.internal.server._
import sbt.internal.util.{ ObjectEvent, Terminal }
import sbt.io.syntax._
import sbt.io.{ Hash, IO }
import sbt.protocol.{ ExecStatusEvent, LogEvent }
import sbt.util.Logger
import sbt.internal.ui.{ AskUserTask, UITask }
import sbt.internal.util._
import sbt.io.syntax._
import sbt.io.{ Hash, IO }
import sbt.nio.Watch.NullLogger
import sbt.protocol.ExecStatusEvent
import sbt.util.Logger
import sjsonnew.JsonFormat

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
  private[this] val lastState = new AtomicReference[State]
  private[this] val currentExec = new AtomicReference[Exec]
  private[this] val lastProgressEvent = new AtomicReference[ProgressEvent]

  def channels: List[CommandChannel] = channelBuffer.toList

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
      state.foreach(s => channels.foreach(_.prompt(ConsolePromptEvent(s))))
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

  def run(s: State): State = run(s, s.get(autoStartServer).getOrElse(true))
  def run(s: State, autoStart: Boolean): State = {
    if (consoleChannel.isEmpty) {
      val name = "console0"
      val console0 = new ConsoleChannel(name, mkAskUser(name))
      consoleChannel = Some(console0)
      subscribe(console0)
    }
    if (autoStartServerSysProp && autoStart) runServer(s)
    else s
  }
  private[sbt] def setState(s: State): Unit = lastState.set(s)

  private def newNetworkName: String = s"network-${nextChannelId.incrementAndGet()}"

  private[sbt] def removeChannel(c: CommandChannel): Unit = channelBufferLock.synchronized {
    Util.ignoreResult(channelBuffer -= c)
  }

  private[sbt] def addBatchChannel(useSuperShell: Boolean): Unit = {
    if (!channels.exists(_.name == "anonymous"))
      subscribe(new BatchCommandChannel(mkAskUser("anonymous"), useSuperShell))
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
    respondError(JsonRpcResponseError(code, message), execId, source)
  }

  private[sbt] def respondError(
      err: JsonRpcResponseError,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = {
    for {
      source <- source.map(_.channelName)
      channel <- channels.collectFirst {
        // broadcast to the source channel only
        case c: NetworkChannel if c.name == source => c
      }
    } tryTo(_.respondError(err, execId))(channel)
  }

  // This is an interface to directly respond events.
  private[sbt] def respondEvent[A: JsonFormat](
      event: A,
      execId: Option[String],
      source: Option[CommandSource]
  ): Unit = {
    for {
      source <- source.map(_.channelName)
      channel <- channels.collectFirst {
        // broadcast to the source channel only
        case c: NetworkChannel if c.name == source => c
      }
    } tryTo(_.respondResult(event, execId))(channel)
  }

  // This is an interface to directly notify events.
  private[sbt] def notifyEvent[A: JsonFormat](method: String, params: A): Unit = {
    channels.foreach {
      case c: NetworkChannel => tryTo(_.notifyEvent(method, params))(c)
      case _                 =>
    }
  }

  private def tryTo(f: NetworkChannel => Unit)(
      channel: NetworkChannel
  ): Unit =
    try f(channel)
    catch { case _: IOException => removeChannel(channel) }

  def respondStatus(event: ExecStatusEvent): Unit = {
    import sbt.protocol.codec.JsonProtocol._
    for {
      source <- event.channelName
      channel <- channels.collectFirst {
        case c: NetworkChannel if c.name == source => c
      }
    } {
      if (event.execId.isEmpty) {
        tryTo(_.notifyEvent(event))(channel)
      } else {
        event.exitCode match {
          case None | Some(0) =>
            tryTo(_.respondResult(event, event.execId))(channel)
          case Some(code) =>
            tryTo(_.respondError(code, event.message.getOrElse(""), event.execId))(channel)
        }
      }

      tryTo(_.respond(event, event.execId))(channel)
    }
  }

  /**
   * This publishes object events. The type information has been
   * erased because it went through logging.
   */
  private[sbt] def respondObjectEvent(event: ObjectEvent[_]): Unit = {
    for {
      source <- event.channelName
      channel <- channels.collectFirst {
        case c: NetworkChannel if c.name == source => c
      }
    } tryTo(_.respond(event))(channel)
  }

  def prompt(event: ConsolePromptEvent): Unit = channels.foreach(_.prompt(event))
  def unprompt(event: ConsoleUnpromptEvent): Unit = channels.foreach(_.unprompt(event))

  def logMessage(event: LogEvent): Unit = {
    channels.foreach {
      case c: NetworkChannel => tryTo(_.notifyEvent(event))(c)
      case _                 =>
    }
  }

  def notifyStatus(event: ExecStatusEvent): Unit = {
    for {
      source <- event.channelName
      channel <- channels.collectFirst {
        case c: NetworkChannel if c.name == source => c
      }
    } tryTo(_.notifyEvent(event))(channel)
  }

  private[sbt] def killChannel(channel: String): Unit = {
    channels.find(_.name == channel).foreach(_.shutdown(false))
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
      def shutdown(mt: MaintenanceTask): Unit = {
        Option(currentExec.get).foreach(cancel)
        commandQueue.clear()
        val exit =
          Exec("shutdown", Some(Exec.newExecId), Some(CommandSource(mt.channel.name)))
        commandQueue.add(exit)
        ()
      }
      @tailrec def impl(): Unit = {
        maintenanceChannelQueue.take match {
          case null =>
          case mt: MaintenanceTask =>
            mt.task match {
              case "attach"                 => mt.channel.prompt(ConsolePromptEvent(lastState.get))
              case k if k == "kill channel" => mt.channel.shutdown(false)
              case k if k.startsWith("kill") =>
                val name = k.split("kill[ ]+").lastOption
                Option(currentExec.get).filter(e => name.contains(e.commandLine)).foreach(cancel)
              case "exit" =>
                mt.channel.shutdown(false)
                if (mt.channel.name.contains("console")) shutdown(mt)
              case "shutdown" => shutdown(mt)
              case _          =>
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
  private[sbt] def channelForName(channelName: String): Option[CommandChannel] =
    channels.find(_.name == channelName)
  private[this] val maintenanceThread = new MaintenanceThread
}
