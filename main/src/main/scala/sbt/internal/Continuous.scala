/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.io.{ ByteArrayInputStream, IOException, InputStream, File => _ }
import java.nio.file.Path
import java.util.concurrent.{
  Callable,
  ConcurrentHashMap,
  ExecutorService,
  Executors,
  Future,
  LinkedBlockingQueue
}
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger, AtomicReference }

import sbt.BasicCommandStrings._
import sbt.Def._
import sbt.Keys._
import sbt.internal.Continuous.{ ContinuousState, FileStampRepository }
import sbt.internal.LabeledFunctions._
import sbt.internal.io.WatchState
import sbt.internal.nio._
import sbt.internal.ui.UITask
import sbt.internal.util.complete.DefaultParsers.{ Space, matched }
import sbt.internal.util.complete.Parser._
import sbt.internal.util.complete.{ Parser, Parsers }
import sbt.internal.util._
import sbt.nio.Keys.{ fileInputs, _ }
import sbt.nio.Watch.{ Creation, Deletion, ShowOptions, Update }
import sbt.nio.file.{ FileAttributes, Glob }
import sbt.nio.{ FileStamp, FileStamper, Watch }
import sbt.util.{ Level, _ }

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration.FiniteDurationIsOrdered
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

/**
 * Provides the implementation of the `~` command and `watch` task. The implementation is quite
 * complex because we have to parse the command string to figure out which tasks we want to run.
 * Using the tasks, we then have to extract all of the settings for the continuous build. Finally
 * we have to aggregate the settings for each task into an aggregated watch config that will
 * sanely watch multiple tasks and respond to file updates and user input in a way that makes
 * sense for each of the tasks that are being monitored.
 *
 * The behavior, on the other hand, should be fairly straightforward. For example, if a user
 * wants to continuously run the compile task for projects a and b, then we create FileEventMonitor
 * instances for each product and watch all of the directories that contain compile sources
 * (as well as the source directories of transitive inter-project classpath dependencies). If
 * a change is detected in project a, then we should trigger a build for both projects a and b.
 *
 * The semantics are flexible and may be adapted. For example, a user may want to watch two
 * unrelated tasks and only rebuild the task with sources that have been changed. This could be
 * handled at the `~` level, but it probably makes more sense to build a better task caching
 * system so that we don't rerun tasks if their inputs have not changed. As of 1.3.0, the
 * semantics match previous sbt versions as closely as possible while allowing the user more
 * freedom to adjust the behavior to best suit their use cases.
 *
 * For now Continuous extends DeprecatedContinuous to minimize the number of deprecation warnings
 * produced by this file. In sbt 2.0, the DeprecatedContinuous mixin should be eliminated and
 * the deprecated apis should no longer be supported.
 *
 */
private[sbt] object Continuous extends DeprecatedContinuous {
  private type Event = FileEvent[FileAttributes]

  /**
   * Provides the dynamic inputs to the continuous build callbacks that cannot be stored as
   * settings. This wouldn't need to exist if there was a notion of a lazy setting in sbt.
   *
   * @param logger the Logger
   * @param inputs the transitive task inputs
   */
  private[sbt] final class Arguments private[Continuous] (
      val logger: Logger,
      val inputs: Seq[DynamicInput]
  )

  /**
   * Create a function from InputStream => [[Watch.Action]] from a [[Parser]]. This is intended
   * to be used to set the watchInputHandler setting for a task.
   *
   * @param parser the parser
   * @return the function
   */
  private def defaultInputHandler(parser: Parser[Watch.Action]): InputStream => Watch.Action = {
    val builder = new StringBuilder
    val any = matched(Parsers.any.*)
    val fullParser = any ~> parser ~ any
    ((inputStream: InputStream) => parse(inputStream, builder, fullParser))
      .label("Continuous.defaultInputHandler")
  }

  /**
   * Implements continuous execution. It works by first parsing the command and generating a task to
   * run with each build. It can run multiple commands that are separated by ";" in the command
   * input. If any of these commands are invalid, the watch will immediately exit.
   *
   * @return a Command that can be used by sbt to implement continuous builds.
   */
  private[sbt] def continuous: Command =
    Command(ContinuousExecutePrefix, continuousBriefHelp, continuousDetail)(continuousParser) {
      case (s, (initialCount, commands)) =>
        val channel = s.currentCommand.flatMap(_.source.map(_.channelName)).getOrElse("anonymous")
        ContinuousCommands.setupWatchState(channel, initialCount, commands, s)
        s"${ContinuousCommands.runWatch} $channel" :: s
    }

  /**
   * The task implementation is quite similar to the command implementation. The tricky part is that
   * we have to modify the Task.info to apply the state transformation after the task completes.
   *
   * @return the [[InputTask]]
   */
  private[sbt] def continuousTask: Def.Initialize[InputTask[StateTransform]] =
    Def.inputTask {
      val (initialCount, commands) = continuousParser.parsed
      val s = state.value
      val channel = s.currentCommand.flatMap(_.source.map(_.channelName)).getOrElse("anonymous")
      ContinuousCommands.setupWatchState(channel, initialCount, commands, s)
      StateTransform(s => s"${ContinuousCommands.runWatch} $channel" :: s)
    }

  private[sbt] val dynamicInputs = taskKey[Option[mutable.Set[DynamicInput]]](
    "The input globs found during task evaluation that are used in watch."
  )

  private[sbt] def dynamicInputsImpl: Def.Initialize[Task[Option[mutable.Set[DynamicInput]]]] =
    Def.task(Keys.state.value.get(DynamicInputs))

  private[sbt] val DynamicInputs =
    AttributeKey[mutable.Set[DynamicInput]](
      "dynamic-inputs",
      "Stores the inputs (dynamic and regular) for a task",
      10000
    )

  private[this] val continuousParser: State => Parser[(Int, Seq[String])] = {
    def toInt(s: String): Int = Try(s.toInt).getOrElse(0)

    // This allows us to re-enter the watch with the previous count.
    val digitParser: Parser[Int] =
      (Parsers.Space.* ~> matched(Parsers.Digit.+) <~ Parsers.Space.*).map(toInt)
    state =>
      val ocp = BasicCommands.multiParserImpl(Some(state)) |
        BasicCommands.otherCommandParser(state).map(_ :: Nil)
      (digitParser.? ~ ocp).flatMap {
        case (i, commands) if commands.exists(_.nonEmpty) =>
          Parser.success((i.getOrElse(0), commands.filter(_.nonEmpty)))
        case (_, _) => Parser.failure("Couldn't parse any commands")
      }
  }

  /**
   * Gets the [[Config]] necessary to watch a task. It will extract the internal dependency
   * configurations for the task (these are the classpath dependencies specified by
   * [[Project.dependsOn]]). Using these configurations and the settings map, it walks the
   * dependency graph for the key and extracts all of the transitive globs specified by the
   * inputs and triggers keys. It also extracts the legacy globs specified by the watchSources key.
   *
   * @param state the current [[State]] instance.
   * @param scopedKey the [[ScopedKey]] instance corresponding to the task we're monitoring
   * @param compiledMap the map of all of the build settings
   * @param extracted the [[Extracted]] instance for the build
   * @param logger a logger that can be used while generating the [[Config]]
   * @return the [[Config]] instance
   */
  private def getConfig(
      state: State,
      scopedKey: ScopedKey[_],
      compiledMap: CompiledMap,
  )(implicit extracted: Extracted, logger: Logger): Config = {

    // Extract all of the globs that we will monitor during the continuous build.
    val inputs = {
      val configs = scopedKey.get(internalDependencyConfigurations).getOrElse(Nil)
      val args =
        new SettingsGraph.Arguments(scopedKey, extracted, compiledMap, logger, configs, state)
      SettingsGraph.transitiveDynamicInputs(args)
    }

    val repository = getRepository(state)
    val dynamicInputs = state
      .get(DynamicInputs)
      .getOrElse {
        val msg = "Uninitialized dynamic inputs in continuous build (should be unreachable!)"
        throw new IllegalStateException(msg)
      }
    dynamicInputs ++= inputs
    logger.debug(s"[watch] [${scopedKey.show}] Found inputs: ${inputs.map(_.glob).mkString(",")}")
    inputs.foreach(i => repository.register(i.glob))
    val watchSettings = new WatchSettings(scopedKey)
    new Config(
      scopedKey.show,
      () => dynamicInputs.toSeq.sorted,
      watchSettings
    )
  }

  private def getRepository(state: State): FileTreeRepository[FileAttributes] = {
    lazy val exception =
      new IllegalStateException("Tried to access FileTreeRepository for uninitialized state")
    state
      .get(globalFileTreeRepository)
      .getOrElse(throw exception)
  }

  private[sbt] def setup[R](state: State, commands: Seq[String])(
      f: (State, Seq[(String, State, () => Boolean)], Seq[String]) => R
  ): R = {
    // First set up the state so that we can capture whether or not a task completed successfully
    // or if it threw an Exception (we lose the actual exception, but that should still be printed
    // to the console anyway).
    val failureCommandName = "SbtContinuousWatchOnFail"
    val onFail = Command.command(failureCommandName)(identity)
    // This adds the "SbtContinuousWatchOnFail" onFailure handler which allows us to determine
    // whether or not the last task successfully ran. It is used in the makeTask method below.
    val s = state.copy(
      onFailure = Some(Exec(failureCommandName, None)),
      definedCommands = state.definedCommands :+ onFail
    )

    /*
     * Takes a task string and converts it to an EitherTask. We cannot preserve either
     * the value returned by the task or any exception thrown by the task, but we can determine
     * whether or not the task ran successfully using the onFail command defined above. Each
     * task gets its own state with its own file tree repository. This is so that we can keep
     * track of what globs are actually used by the task to ensure that we monitor them, even
     * if they are not visible in the input graph due to the use of Def.taskDyn.
     */
    def makeTask(cmd: String): (String, State, () => Boolean) = {
      val newState = s
        .put(DynamicInputs, mutable.Set.empty[DynamicInput])
        .copy(remainingCommands = Exec(cmd, None, None) :: Exec(FailureWall, None, None) :: Nil)
      (
        cmd,
        newState,
        () => {
          @tailrec
          def impl(s: State): Boolean = {
            s.remainingCommands match {
              case exec :: rest =>
                val updatedState = MainLoop.processCommand(exec, s.copy(remainingCommands = rest))
                val remaining =
                  updatedState.remainingCommands.takeWhile(_.commandLine != failureCommandName)
                remaining match {
                  case Nil =>
                    updatedState.remainingCommands.forall(_.commandLine != failureCommandName)
                  case _ => impl(updatedState)
                }
              case Nil => true
            }
          }
          impl(newState)
        }
      )
    }

    // Convert the command strings to runnable tasks, which are represented by
    // () => Try[Boolean].
    val taskParser = s.combinedParser
    // This specified either the task corresponding to a command or the command itself if the
    // the command cannot be converted to a task.
    val (invalid, valid) =
      commands.foldLeft((Nil: Seq[String], Nil: Seq[(String, State, () => Boolean)])) {
        case ((i, v), cmd) =>
          Parser.parse(cmd, taskParser) match {
            case Right(_) => (i, v :+ makeTask(cmd))
            case Left(c)  => (i :+ c, v)
          }
      }
    f(s, valid, invalid)
  }

  // This is defined so we can assign a task key to a command to parse the WatchSettings.
  private[this] val globalWatchSettingKey =
    taskKey[Unit]("Internal task key. Not actually used.").withRank(KeyRanks.Invisible)
  private def parseCommand(command: String, state: State): Seq[ScopedKey[_]] = {
    // Collect all of the scoped keys that are used to delegate the multi commands. These are
    // necessary to extract all of the transitive globs that we need to monitor during watch.
    // We have to add the <~ Parsers.any.* to ensure that we're able to extract the input key
    // from input tasks.
    val scopedKeyParser: Parser[Seq[ScopedKey[_]]] = Act.aggregatedKeyParser(state) <~ Parsers.any.*
    @tailrec def impl(current: String): Seq[ScopedKey[_]] = {
      Parser.parse(current, scopedKeyParser) match {
        case Right(scopedKeys: Seq[ScopedKey[_]]) => scopedKeys
        case Left(e) =>
          val aliases = BasicCommands.allAliases(state)
          aliases.collectFirst { case (`command`, aliased) => aliased } match {
            case Some(aliased) => impl(aliased)
            case None =>
              Parser.parse(command, state.combinedParser) match {
                case Right(_) => globalWatchSettingKey.scopedKey :: Nil
                case _ =>
                  val msg = s"Error attempting to extract scope from $command: $e."
                  throw new IllegalStateException(msg)
              }
          }
        case _ => Nil: Seq[ScopedKey[_]]
      }
    }
    impl(command)
  }

  private def getAllConfigs(
      inputs: Seq[(String, State)]
  )(implicit extracted: Extracted, logger: Logger): Seq[Config] = {
    val commandKeys = inputs.map { case (c, s) => s -> parseCommand(c, s) }
    val compiledMap = SettingsGraph.compile(extracted.structure)
    commandKeys.flatMap {
      case (s, scopedKeys) => scopedKeys.map(getConfig(s, _, compiledMap))
    }
  }

  private[sbt] class Callbacks(
      val nextEvent: Int => Watch.Action,
      val beforeCommand: () => Unit,
      val onExit: () => Unit,
      val onStart: Int => Watch.Action,
      val onTermination: (Watch.Action, String, Int, State) => State
  )

  private[sbt] def getCallbacks(
      s: State,
      channelName: String,
      commands: Seq[String],
      terminal: Terminal,
      fileStampCache: FileStamp.Cache
  ): Callbacks = {
    implicit val extracted: Extracted = Project.extract(s)
    implicit val logger: Logger = LogExchange.logger(channelName + "-watch")
    setup(s, commands) { (_, valid, invalid) =>
      if (invalid.isEmpty) {
        val configs = getAllConfigs(valid.map(v => v._1 -> v._2))
        val appender = ConsoleAppender(channelName + "-watch", terminal)
        val level = configs.minBy(_.watchSettings.logLevel).watchSettings.logLevel
        LogExchange.bindLoggerAppenders(channelName + "-watch", (appender -> level) :: Nil)
        aggregate(
          configs,
          logger,
          terminal,
          s,
          isCommand = true,
          commands,
          fileStampCache
        )
      } else {
        val msg = s"Invalid commands: ${invalid.mkString("'", "', '", ",")}"
        throw new IllegalArgumentException(msg)
      }
    }
  }

  /**
   * Aggregates a collection of [[Config]] instances into a single instance of [[Callbacks]].
   * This allows us to monitor and respond to changes for all of
   * the inputs and triggers for each of the tasks that we are monitoring in the continuous build.
   * To monitor all of the inputs and triggers, it creates a monitor for each task
   * and then aggregates each of the individual monitor instances into an aggregated
   * instance. It aggregates all of the event callbacks into a single callback that delegates
   * to each of the individual callbacks. For the callbacks that return a [[Watch.Action]],
   * the aggregated callback will select the minimum [[Watch.Action]] returned where the ordering
   * is such that the highest priority [[Watch.Action]] have the lowest values. Finally, to
   * handle user input, we read from the provided input stream and buffer the result. Each
   * task's input parser is then applied to the buffered result and, again, we return the minimum
   * [[Watch.Action]] returned by the parsers (when the parsers fail, they just return
   * [[Watch.Ignore]], which is the lowest priority [[Watch.Action]].
   *
   * @param configs the [[Config]] instances
   * @param logger the default sbt logger instance
   * @param state the current state
   * @param extracted the [[Extracted]] instance for the current build
   * @return the [[Callbacks]] to pass into [[Watch.apply]]
   */
  private def aggregate(
      configs: Seq[Config],
      logger: Logger,
      terminal: Terminal,
      state: State,
      isCommand: Boolean,
      commands: Seq[String],
      fileStampCache: FileStamp.Cache
  )(
      implicit extracted: Extracted
  ): Callbacks = {
    val project = extracted.currentRef
    val beforeCommand = () => configs.foreach(_.watchSettings.beforeCommand())
    val onStart: Int => Watch.Action =
      getOnStart(project, commands, configs, logger, extracted)
    val (message, parser, altParser) = getWatchInputOptions(configs, extracted)
    val nextInputEvent: ExecutorService => Option[Watch.Action] = {
      val poll = !Util.isNonCygwinWindows && configs.exists(_.watchSettings.pollSystemIn)
      parseInputEvents(parser, altParser, state, terminal, logger, poll)
    }
    val (nextFileEvent, cleanupFileMonitor): (
        Int => Option[(Watch.Event, Watch.Action)],
        () => Unit
    ) = getFileEvents(configs, logger, state, commands, fileStampCache)
    val executor = Executors.newFixedThreadPool(3, {
      val count = new AtomicInteger(0)
      r => new Thread(r, s"sbt-watch-event-thread-${count.incrementAndGet}")
    })
    val nextEvent: Int => Watch.Action =
      combineInputAndFileEvents(nextInputEvent, nextFileEvent, message, logger, logger, executor)
    val onExit = () => {
      cleanupFileMonitor()
      executor.shutdown()
    }
    val onTermination = getOnTermination(configs, isCommand)
    new Callbacks(nextEvent, beforeCommand, onExit, onStart, onTermination)
  }

  private def getOnTermination(
      configs: Seq[Config],
      isCommand: Boolean
  ): (Watch.Action, String, Int, State) => State = {
    configs.flatMap(_.watchSettings.onTermination).distinct match {
      case Seq(head, tail @ _*) =>
        tail.foldLeft(head) {
          case (onTermination, configOnTermination) =>
            (action, cmd, count, state) =>
              configOnTermination(action, cmd, count, onTermination(action, cmd, count, state))
        }
      case _ =>
        if (isCommand) Watch.defaultCommandOnTermination else Watch.defaultTaskOnTermination
    }
  }

  private def getWatchInputOptions(
      configs: Seq[Config],
      extracted: Extracted
  ): (String, Parser[Watch.Action], Option[(TaskKey[InputStream], InputStream => Watch.Action)]) = {
    configs match {
      case Seq(h) =>
        val settings = h.watchSettings
        val parser = settings.inputParser
        val alt = settings.inputStream.map { k =>
          k -> settings.inputHandler.getOrElse(defaultInputHandler(parser))
        }
        (settings.inputOptionsMessage, parser, alt)
      case _ =>
        val options =
          extracted.getOpt(watchInputOptions in ThisBuild).getOrElse(Watch.defaultInputOptions)
        val message = extracted
          .getOpt(watchInputOptionsMessage in ThisBuild)
          .getOrElse(Watch.defaultInputOptionsMessage(options))
        val parser = extracted
          .getOpt(watchInputParser in ThisBuild)
          .getOrElse(Watch.defaultInputParser(options))
        val alt = extracted
          .getOpt(watchInputStream in ThisBuild)
          .map { _ =>
            (watchInputStream in ThisBuild) -> extracted
              .getOpt(watchInputHandler in ThisBuild)
              .getOrElse(defaultInputHandler(parser))
          }
        (message, parser, alt)
    }
  }

  private def getOnStart(
      project: ProjectRef,
      commands: Seq[String],
      configs: Seq[Config],
      logger: Logger,
      extracted: Extracted,
  ): Int => Watch.Action = {
    val f: Int => Seq[Watch.Action] = count => {
      configs.map { params =>
        val ws = params.watchSettings
        ws.onIteration.map(_(count, project, commands)).getOrElse {
          if (configs.size == 1) { // Only allow custom start messages for single tasks
            ws.startMessage match {
              case Some(Left(sm))  => logger.info(sm(params.watchState(count)))
              case Some(Right(sm)) => sm(count, project, commands).foreach(logger.info(_))
              case None =>
                Watch.defaultStartWatch(count, project, commands).foreach(logger.info(_))
            }
          }
          Watch.Ignore
        }
      }
    }
    count => {
      val res = f(count).min
      // Print the default watch message if there are multiple tasks
      if (configs.size > 1) {
        val onStartWatch =
          extracted.getOpt(watchStartMessage in project).getOrElse(Watch.defaultStartWatch)
        onStartWatch(count, project, commands).foreach(logger.info(_))
      }
      res
    }
  }

  private def getFileEvents(
      configs: Seq[Config],
      logger: Logger,
      state: State,
      commands: Seq[String],
      fileStampCache: FileStamp.Cache
  )(implicit extracted: Extracted): (Int => Option[(Watch.Event, Watch.Action)], () => Unit) = {
    val trackMetaBuild = configs.forall(_.watchSettings.trackMetaBuild)
    val buildGlobs =
      if (trackMetaBuild) extracted.getOpt(fileInputs in checkBuildSources).getOrElse(Nil)
      else Nil

    val retentionPeriod = configs.map(_.watchSettings.antiEntropyRetentionPeriod).max
    val quarantinePeriod = configs.map(_.watchSettings.deletionQuarantinePeriod).max
    val monitor: FileEventMonitor[Event] = new FileEventMonitor[Event] {

      private implicit class WatchLogger(val l: Logger) extends sbt.internal.nio.WatchLogger {
        override def debug(msg: Any): Unit = l.debug(msg.toString)
      }

      private[this] val observers: Observers[Event] = new Observers
      private[this] val repo = getRepository(state)
      private[this] val handle = repo.addObserver(observers)
      private[this] val eventMonitorObservers = new Observers[Event]
      private[this] val configHandle: AutoCloseable =
        observers.addObserver { e =>
          // We only want to create one event per actual source file event. It doesn't matter
          // which of the config inputs triggers the event because they all will be used in
          // the onEvent callback above.
          configs.find(_.inputs().exists(_.glob.matches(e.path))) match {
            case Some(config) =>
              val configLogger = logger.withPrefix(config.command)
              configLogger.debug(s"Accepted event for ${e.path}")
              eventMonitorObservers.onNext(e)
            case None =>
          }
          if (trackMetaBuild && buildGlobs.exists(_.matches(e.path))) {
            val metaLogger = logger.withPrefix("build")
            metaLogger.debug(s"Accepted event for ${e.path}")
            eventMonitorObservers.onNext(e)
          }
        }
      if (trackMetaBuild) buildGlobs.foreach(repo.register)

      private[this] val monitor = FileEventMonitor.antiEntropy(
        eventMonitorObservers,
        configs.map(_.watchSettings.antiEntropy).max,
        logger,
        quarantinePeriod,
        retentionPeriod
      )

      override def poll(duration: Duration, filter: Event => Boolean): Seq[Event] =
        monitor.poll(duration, filter)

      override def close(): Unit = {
        configHandle.close()
        handle.close()
      }
    }
    val watchLogger: WatchLogger = msg => logger.debug(msg.toString)
    val antiEntropy = configs.map(_.watchSettings.antiEntropy).max
    val antiEntropyMonitor = FileEventMonitor.antiEntropy(
      monitor,
      antiEntropy,
      watchLogger,
      quarantinePeriod,
      retentionPeriod
    )

    val onEvent: (Int, Event) => Seq[(Watch.Event, Watch.Action)] = (count, event) => {
      val path = event.path

      def getWatchEvent(forceTrigger: Boolean): Option[Watch.Event] = {
        if (!event.exists) {
          Some(Deletion(event))
          fileStampCache.remove(event.path) match {
            case null => None
            case _    => Some(Deletion(event))
          }
        } else {
          fileStampCache.update(path, FileStamper.Hash) match {
            case (None, Some(_)) => Some(Creation(event))
            case (Some(_), None) => Some(Deletion(event))
            case (Some(p), Some(c)) =>
              if (forceTrigger) {
                val msg =
                  s"Creating forced update event for path $path (previous stamp: $p, current stamp: $c)"
                logger.debug(msg)
                Some(Update(event))
              } else if (p == c) {
                logger.debug(s"Dropping event for unmodified path $path")
                None
              } else {
                val msg =
                  s"Creating update event for modified $path (previous stamp: $p, current stamp: $c)"
                logger.debug(msg)
                Some(Update(event))
              }
            case _ => None
          }
        }
      }

      if (buildGlobs.exists(_.matches(path))) {
        getWatchEvent(forceTrigger = false).map(e => e -> Watch.Reload).toSeq
      } else {
        val acceptedConfigParameters = configs.flatMap { config =>
          config.inputs().flatMap {
            case i if i.glob.matches(path) =>
              Some((i.forceTrigger, i.fileStamper, config.watchSettings.onFileInputEvent))
            case _ => None
          }
        }
        if (acceptedConfigParameters.nonEmpty) {
          val useHash = acceptedConfigParameters.exists(_._2 == FileStamper.Hash)
          val forceTrigger = acceptedConfigParameters.exists(_._1)
          val watchEvent =
            if (useHash) getWatchEvent(forceTrigger)
            else {
              logger.debug(s"Trigger path detected $path")
              Some(
                if (!event.exists) Deletion(event)
                else if (fileStampCache.get(path).isDefined) Creation(event)
                else Update(event)
              )
            }
          acceptedConfigParameters.flatMap {
            case (_, _, callback) =>
              watchEvent.map(e => e -> callback(count, e))
          }
        } else Nil
      }
    }
    /*
     * This is a callback that will be invoked whenever onEvent returns a Trigger action. The
     * motivation is to allow the user to specify this callback via setting so that, for example,
     * they can clear the screen when the build triggers.
     */
    val onTrigger: (Int, Watch.Event) => Unit = { (count: Int, event: Watch.Event) =>
      if (configs.size == 1) {
        val config = configs.head
        config.watchSettings.triggerMessage match {
          case Left(tm)  => logger.info(tm(config.watchState(count)))
          case Right(tm) => tm(count, event.path, commands).foreach(logger.info(_))
        }
      } else {
        Watch.defaultOnTriggerMessage(count, event.path, commands).foreach(logger.info(_))
      }
    }

    ((count: Int) => {
      val interrupted = new AtomicBoolean(false)
      def getEvent: Option[(Watch.Event, Watch.Action)] = {
        val events =
          try antiEntropyMonitor.poll(Duration.Inf)
          catch { case _: InterruptedException => interrupted.set(true); Nil }
        val actions = events.flatMap(onEvent(count, _))
        if (actions.exists(_._2 != Watch.Ignore)) {
          val builder = new StringBuilder
          val min = actions.minBy {
            case (e, a) =>
              if (builder.nonEmpty) builder.append(", ")
              val path = e.path
              builder.append(path)
              builder.append(" -> ")
              builder.append(a.toString)
              a
          }
          logger.debug(s"Received file event actions: $builder. Returning: $min")
          if (min._2 == Watch.Trigger) onTrigger(count, min._1)
          if (min._2 == Watch.ShowOptions) None else Some(min)
        } else None
      }

      @tailrec def impl(): Option[(Watch.Event, Watch.Action)] = getEvent match {
        case None =>
          if (interrupted.get || Thread.interrupted) None
          else impl()
        case r => r
      }

      impl()
    }, () => monitor.close())
  }

  private[this] val readFuture = new AtomicReference[Future[Option[Byte]]]

  /**
   * Each task has its own input parser that can be used to modify the watch based on the input
   * read from System.in as well as a custom task-specific input stream that can be used as
   * an alternative source of control. In this method, we create two functions for each task,
   * one from `String => Seq[Watch.Action]` and another from `() => Seq[Watch.Action]`.
   * Each of these functions is invoked to determine the next state transformation for the watch.
   * The first function is a task specific copy of System.in. For each task we keep a mutable
   * buffer of the characters previously seen from System.in. Every time we receive new characters
   * we update the buffer and then try to parse a Watch.Action for each task. Any trailing
   * characters are captured and can be used for the next trigger. Because each task has a local
   * copy of the buffer, we do not have to worry about one task breaking parsing of another. We
   * also provide an alternative per task InputStream that is read in a similar way except that
   * we don't need to copy the custom InputStream which allows the function to be
   * `() => Seq[Watch.Action]` which avoids actually exposing the InputStream anywhere.
   */
  private def parseInputEvents(
      parser: Parser[Watch.Action],
      alternative: Option[(TaskKey[InputStream], InputStream => Watch.Action)],
      state: State,
      terminal: Terminal,
      logger: Logger,
      poll: Boolean,
  )(implicit extracted: Extracted): ExecutorService => Option[Watch.Action] = {
    /*
     * This parses the buffer until all possible actions are extracted. By draining the input
     * to a state where it does not parse an action, we can wait until we receive new input
     * to attempt to parse again.
     */
    type ActionParser = String => Watch.Action
    // Transform the Config.watchSettings.inputParser instances to functions of type
    // String => Watch.Action. The String that is provided will contain any characters that
    // have been read from stdin. If there are any characters available, then it calls the
    // parse method with the InputStream set to a ByteArrayInputStream that wraps the input
    // string. The parse method then appends those bytes to a mutable buffer and attempts to
    // parse the buffer. To make this work with streaming input, we prefix the parser with any.*.
    // If the Config.watchSettings.inputStream is set, the same process is applied except that
    // instead of passing in the wrapped InputStream for the input string, we directly pass
    // in the inputStream provided by Config.watchSettings.inputStream.
    val inputHandler: String => Watch.Action = {
      val any = Parsers.any.*
      val fullParser = any ~> parser ~ matched(any)
      // Each parser gets its own copy of System.in that it can modify while parsing.
      val systemInBuilder = new StringBuilder

      def inputStream(string: String): InputStream = new ByteArrayInputStream(string.getBytes)

      // This string is provided in the closure below by reading from System.in
      val default: String => Watch.Action =
        string => parse(inputStream(string), systemInBuilder, fullParser)
      val alt = alternative
        .map {
          case (key, handler) =>
            val is = extracted.runTask(key, state)._2
            () => handler(is)
        }
        .getOrElse(() => Watch.Ignore)
      string: String =>
        ((if (string.nonEmpty) default(string) else Watch.Ignore) :: alt() :: Nil).min
    }
    executor => {
      val interrupted = new AtomicBoolean(false)
      @tailrec def impl(): Option[Watch.Action] = {
        def getNextByte: Callable[Option[Byte]] =
          () =>
            try {
              Some(terminal.inputStream.read.toByte)
            } finally readFuture.synchronized(readFuture.set(null))
        val action =
          try {
            interrupted.set(false)
            val future = readFuture.synchronized {
              val previous = readFuture.get
              if (Terminal.systemInIsAttached && previous == null) {
                val newFuture = executor.submit(getNextByte)
                readFuture.set(newFuture)
                Some(newFuture)
              } else Option(previous)
            }
            val byte = future.flatMap(_.get)
            val parse: ActionParser => Watch.Action = parser =>
              parser(byte.fold("")(_.toChar.toString))
            parse(inputHandler)
          } catch {
            case _: InterruptedException =>
              interrupted.set(true)
              readFuture.synchronized(readFuture.get) match {
                case null =>
                case f    => f.cancel(true)
              }
              Watch.Ignore
          }
        action match {
          case Watch.Ignore =>
            val stop = interrupted.get || Thread.interrupted
            if ((!Terminal.systemInIsAttached || alternative.isDefined) && !stop) impl()
            else None
          case r => Some(r)
        }
      }

      terminal.withRawSystemIn(impl())
    }
  }

  private def combineInputAndFileEvents(
      nextInputAction: ExecutorService => Option[Watch.Action],
      nextFileEvent: Int => Option[(Watch.Event, Watch.Action)],
      options: String,
      logger: Logger,
      rawLogger: Logger,
      executor: ExecutorService
  ): Int => Watch.Action = count => {
    val events = new LinkedBlockingQueue[Either[Watch.Action, (Watch.Event, Watch.Action)]]

    def submit(f: => Unit): Future[_] = executor.submit((() => f): Runnable)
    val inputJob = submit(nextInputAction(executor).foreach(a => events.put(Left(a))))
    val fileJob = submit(nextFileEvent(count).foreach(e => events.put(Right(e))))
    try {
      val (inputAction: Watch.Action, fileEvent: Option[(Watch.Event, Watch.Action)]) =
        events.take() match {
          case Left(a)  => (a, None)
          case Right(e) => (Watch.Ignore, Some(e))
        }
      val min: Watch.Action = (fileEvent.map(_._2).toSeq :+ inputAction).min
      lazy val inputMessage =
        s"Received input event: $inputAction." +
          (if (inputAction != min) s" Dropping in favor of file event: $min" else "")
      if (inputAction != Watch.Ignore) logger.debug(inputMessage)
      fileEvent
        .collect {
          case (event, action) if action != Watch.Ignore =>
            s"Received file event $action for $event." +
              (if (action != min) s" Dropping in favor of input event: $min" else "")
        }
        .foreach(logger.debug(_))
      min match {
        case ShowOptions =>
          ConsoleOut.systemOut.println("")
          rawLogger.info(options)
          Watch.Ignore
        case m => m
      }
    } finally {
      inputJob.cancel(true)
      fileJob.cancel(true)
      ()
    }
  }

  @tailrec
  private final def parse(
      is: InputStream,
      builder: StringBuilder,
      parser: Parser[(Watch.Action, String)]
  ): Watch.Action = {
    if (is.available > 0) builder += is.read().toChar
    Parser.parse(builder.toString, parser) match {
      case Right((action, rest)) =>
        builder.clear()
        builder ++= rest
        action
      case _ if is.available > 0 => parse(is, builder, parser)
      case _                     => Watch.Ignore
    }
  }

  private type WatchOnEvent = (Int, Watch.Event) => Watch.Action

  /**
   * Contains all of the user defined settings that will be used to build a [[Callbacks]]
   * instance that is used to produce the arguments to [[Watch.apply]]. The
   * callback settings (e.g. onEvent or onInputEvent) come in two forms: those that return a
   * function from [[Arguments]] => F for some function type `F` and those that directly return a function, e.g.
   * `(Int, Boolean) => Watch.Action`. The former are a low level interface that will usually
   * be unspecified and automatically filled in by [[Continuous.aggregate]]. The latter are
   * intended to be user configurable and will be scoped to the input [[ScopedKey]]. To ensure
   * that the scoping makes sense, we first try and extract the setting from the [[ScopedKey]]
   * instance's task scope, which is the scope with the task axis set to the task key. If that
   * fails, we fall back on the task axis. To make this concrete, to get the logLevel for
   * `foo / Compile / compile` (which is a TaskKey with scope `foo / Compile`), we first try and
   * get the setting in the `foo / Compile / compile` scope. If logLevel is not set at the task
   * level, then we fall back to the `foo / Compile` scope.
   *
   * This has to be done by manually extracting the settings via [[Extracted]] because there is
   * no good way to automatically add a [[WatchSettings]] setting to every task in the build.
   * Thankfully these map retrievals are reasonably fast so there is not a significant runtime
   * performance penalty for creating the [[WatchSettings]] this way. The drawback is that we
   * have to manually resolve the settings in multiple scopes which may lead to inconsistencies
   * with scope resolution elsewhere in sbt.
   *
   * @param key the [[ScopedKey]] instance that sets the [[Scope]] for the settings we're extracting
   * @param extracted the [[Extracted]] instance for the build
   */
  private final class WatchSettings private[Continuous] (val key: ScopedKey[_])(
      implicit extracted: Extracted
  ) {
    val antiEntropy: FiniteDuration =
      key.get(watchAntiEntropy).getOrElse(Watch.defaultAntiEntropy)
    val antiEntropyRetentionPeriod: FiniteDuration =
      key
        .get(watchAntiEntropyRetentionPeriod)
        .getOrElse(Watch.defaultAntiEntropyRetentionPeriod)
    val deletionQuarantinePeriod: FiniteDuration =
      key.get(watchDeletionQuarantinePeriod).getOrElse(Watch.defaultDeletionQuarantinePeriod)
    val inputHandler: Option[InputStream => Watch.Action] = key.get(watchInputHandler)
    val inputOptions: Seq[Watch.InputOption] =
      key.get(watchInputOptions).getOrElse(Watch.defaultInputOptions)
    val inputOptionsMessage: String =
      key.get(watchInputOptionsMessage).getOrElse(Watch.defaultInputOptionsMessage(inputOptions))
    val inputParser: Parser[Watch.Action] =
      key.get(watchInputParser).getOrElse(Watch.defaultInputParser(inputOptions))
    val logLevel: Level.Value = key.get(watchLogLevel).getOrElse(Level.Info)
    val beforeCommand: () => Unit = key.get(watchBeforeCommand).getOrElse(() => {})
    val onFileInputEvent: WatchOnEvent =
      key.get(watchOnFileInputEvent).getOrElse(Watch.trigger)
    val onIteration: Option[(Int, ProjectRef, Seq[String]) => Watch.Action] =
      key.get(watchOnIteration)
    val onTermination: Option[(Watch.Action, String, Int, State) => State] =
      key.get(watchOnTermination)
    val pollSystemIn: Boolean = key.get(watchPollSystemIn).getOrElse(false)
    val startMessage: StartMessage = getStartMessage(key)
    val trackMetaBuild: Boolean =
      key.get(onChangedBuildSource).fold(false)(_ == ReloadOnSourceChanges)
    val triggerMessage: TriggerMessage = getTriggerMessage(key)

    // Unlike the rest of the settings, InputStream is a TaskKey which means that if it is set,
    // we have to use Extracted.runTask to get the value. The reason for this is because it is
    // logical that users may want to use a different InputStream on each task invocation. The
    // alternative would be SettingKey[() => InputStream], but that doesn't feel right because
    // one might want the InputStream to depend on other tasks.
    val inputStream: Option[TaskKey[InputStream]] = key.get(watchInputStream)
  }

  /**
   * Container class for all of the components we need to setup a watch for a particular task or
   * input task.
   *
   * @param command       the name of the command/task to run with each iteration
   * @param inputs        the transitive task inputs (see [[SettingsGraph]])
   * @param watchSettings the [[WatchSettings]] instance for the task
   */
  private final class Config private[internal] (
      val command: String,
      val inputs: () => Seq[DynamicInput],
      val watchSettings: WatchSettings
  ) {
    private[sbt] def watchState(count: Int): DeprecatedWatchState =
      WatchState.empty(inputs().map(_.glob)).withCount(count)

    def arguments(logger: Logger): Arguments = new Arguments(logger, inputs())
  }

  private def getStartMessage(key: ScopedKey[_])(implicit e: Extracted): StartMessage = Some {
    lazy val default = key.get(watchStartMessage).getOrElse(Watch.defaultStartWatch)
    key.get(deprecatedWatchingMessage).map(Left(_)).getOrElse(Right(default))
  }

  private def getTriggerMessage(
      key: ScopedKey[_]
  )(implicit e: Extracted): TriggerMessage = {
    lazy val default =
      key.get(watchTriggeredMessage).getOrElse(Watch.defaultOnTriggerMessage)
    key.get(deprecatedTriggeredMessage).map(Left(_)).getOrElse(Right(default))
  }

  private implicit class ScopeOps(val scope: Scope) {

    /**
     * This shows the [[Scope]] in the format that a user would likely type it in a build
     * or in the sbt console. For example, the key corresponding to the command
     * foo/Compile/compile will pretty print as "foo / Compile / compile", not
     * "ProjectRef($URI, foo) / compile / compile", where the ProjectRef part is just noise that
     * is rarely relevant for debugging.
     *
     * @return the pretty printed output.
     */
    def show: String = {
      val mask = ScopeMask(
        config = scope.config.toOption.isDefined,
        task = scope.task.toOption.isDefined,
        extra = scope.extra.toOption.isDefined
      )
      Scope
        .displayMasked(scope, " ", (_: Reference) match {
          case p: ProjectRef => s"${p.project.trim} /"
          case _             => "Global /"
        }, mask)
        .dropRight(3) // delete trailing "/"
        .trim
    }
  }

  private implicit class ScopedKeyOps(val scopedKey: ScopedKey[_]) extends AnyVal {

    /**
     * Gets the value for a setting key scoped to the wrapped [[ScopedKey]]. If the task axis is not
     * set in the [[ScopedKey]], then we first set the task axis and try to extract the setting
     * from that scope otherwise we fallback on the [[ScopedKey]] instance's scope. We use the
     * reverse order if the task is set.
     *
     * @param settingKey the [[SettingKey]] to extract
     * @param extracted  the provided [[Extracted]] instance
     * @tparam T the type of the [[SettingKey]]
     * @return the optional value of the [[SettingKey]] if it is defined at the input
     *         [[ScopedKey]] instance's scope or task scope.
     */
    def get[T](settingKey: SettingKey[T])(implicit extracted: Extracted): Option[T] = {
      lazy val taskScope = Project.fillTaskAxis(scopedKey).scope
      scopedKey.scope match {
        case scope if scope.task.toOption.isDefined =>
          extracted.getOpt(settingKey in scope) orElse extracted.getOpt(settingKey in taskScope)
        case scope =>
          extracted.getOpt(settingKey in taskScope) orElse extracted.getOpt(settingKey in scope)
      }
    }

    /**
     * Gets the [[ScopedKey]] for a task scoped to the wrapped [[ScopedKey]]. If the task axis is
     * not set in the [[ScopedKey]], then we first set the task axis and try to extract the tak
     * from that scope otherwise we fallback on the [[ScopedKey]] instance's scope. We use the
     * reverse order if the task is set.
     *
     * @param taskKey   the [[TaskKey]] to extract
     * @param extracted the provided [[Extracted]] instance
     * @tparam T the type of the [[SettingKey]]
     * @return the optional value of the [[SettingKey]] if it is defined at the input
     *         [[ScopedKey]] instance's scope or task scope.
     */
    def get[T](taskKey: TaskKey[T])(implicit extracted: Extracted): Option[TaskKey[T]] = {
      lazy val taskScope = Project.fillTaskAxis(scopedKey).scope
      scopedKey.scope match {
        case scope if scope.task.toOption.isDefined =>
          if (extracted.getOpt(taskKey in scope).isDefined) Some(taskKey in scope)
          else if (extracted.getOpt(taskKey in taskScope).isDefined) Some(taskKey in taskScope)
          else None
        case scope =>
          if (extracted.getOpt(taskKey in taskScope).isDefined) Some(taskKey in taskScope)
          else if (extracted.getOpt(taskKey in scope).isDefined) Some(taskKey in scope)
          else None
      }
    }

    /**
     * This shows the [[ScopedKey[_]] in the format that a user would likely type it in a build
     * or in the sbt console. For example, the key corresponding to the command
     * foo/Compile/compile will pretty print as "foo / Compile / compile", not
     * "ProjectRef($URI, foo) / compile / compile", where the ProjectRef part is just noise that
     * is rarely relevant for debugging.
     *
     * @return the pretty printed output.
     */
    def show: String = s"${scopedKey.scope.show} / ${scopedKey.key}"
  }

  private implicit class LoggerOps(val logger: Logger) extends AnyVal {

    /**
     * Creates a logger that adds a prefix to the messages that it logs. The motivation is so that
     * we can tell from which FileEventMonitor an event originated.
     *
     * @param prefix the string to prefix the message with
     * @return the wrapped Logger.
     */
    def withPrefix(prefix: String): Logger = new Logger {
      override def trace(t: => Throwable): Unit = logger.trace(t)

      override def success(message: => String): Unit = logger.success(message)

      override def log(level: Level.Value, message: => String): Unit =
        logger.log(level, s"$prefix - $message")
    }
  }

  private[sbt] class FileStampRepository(
      fileStampCache: FileStamp.Cache,
      underlying: FileTreeRepository[FileAttributes]
  ) extends FileTreeRepository[FileAttributes] {
    def putIfAbsent(path: Path, stamper: FileStamper): (Option[FileStamp], Option[FileStamp]) =
      fileStampCache.putIfAbsent(path, stamper)
    override def list(path: Path): Seq[(Path, FileAttributes)] = underlying.list(path)
    override def addObserver(observer: Observer[FileEvent[FileAttributes]]): AutoCloseable =
      underlying.addObserver(observer)
    override def register(glob: Glob): Either[IOException, Observable[FileEvent[FileAttributes]]] =
      underlying.register(glob)
    override def close(): Unit = underlying.close()
  }

  private[sbt] final class ContinuousState(
      val count: Int,
      val commands: Seq[String],
      beforeCommandImpl: (State, mutable.Set[DynamicInput]) => State,
      val afterCommand: State => State,
      val afterWatch: () => Unit,
      val callbacks: Callbacks,
      val dynamicInputs: mutable.Set[DynamicInput]
  ) {
    def beforeCommand(state: State): State = beforeCommandImpl(state, dynamicInputs)
    def this(
        count: Int,
        commands: Seq[String],
        beforeCommand: (State, mutable.Set[DynamicInput]) => State,
        afterCommand: State => State,
        afterWatch: () => Unit,
        callbacks: Callbacks
    ) = this(count, commands, beforeCommand, afterCommand, afterWatch, callbacks, mutable.Set.empty)
    def incremented: ContinuousState = withCount(count + 1)
    private def withCount(c: Int): ContinuousState =
      new ContinuousState(
        c,
        commands,
        beforeCommandImpl,
        afterCommand,
        afterWatch,
        callbacks,
        dynamicInputs
      )
  }
}

private[sbt] object ContinuousCommands {
  private[sbt] def value: Seq[Command] =
    runWatchCommand :: preWatchCommand :: postWatchCommand :: stopWatchCommand :: Nil
  private[sbt] val watchStateCallbacks =
    AttributeKey[java.util.Map[String, (State => State, State => State)]](
      "sbt-watch-state-callbacks",
      "",
      Int.MaxValue
    )
  private[this] val watchStates = new ConcurrentHashMap[String, ContinuousState]
  /*
   * Prefix these command names with __ to hide them from tab completion.
   */
  private[sbt] val runWatch = "__runWatch"
  private[sbt] val preWatch = "__preWatch"
  private[sbt] val postWatch = "__postWatch"
  private[sbt] val stopWatch = "__stopWatch"
  private[this] def noComplete[T](p: Parser[T]): Parser[T] = p.examples()
  private[this] val space = noComplete(Space)
  private[this] def cmdParser(s: String): Parser[String] = noComplete(matched(s)) <~ space
  private[this] def channelParser: Parser[String] =
    noComplete(matched(charClass(c => c.isLetterOrDigit || c == '-').+))

  private[this] val stashedRepo = AttributeKey[FileTreeRepository[FileAttributes]](
    "stashed-file-tree-repository",
    "",
    Int.MaxValue
  )
  private[sbt] val setupWatchState: (String, Int, Seq[String], State) => Unit =
    (channelName, count, commands, state) => {
      watchStates.get(channelName) match {
        case null =>
          val extracted = Project.extract(state)
          val repo = state.get(globalFileTreeRepository) match {
            case Some(r) => localRepo(r)
            case _ =>
              throw new IllegalStateException(s"No file tree repository was found for $state")
          }
          val cache = new FileStamp.Cache
          repo.addObserver(t => cache.invalidate(t.path))
          val persistFileStamps = extracted.get(watchPersistFileStamps)
          val cachingRepo: FileTreeRepository[FileAttributes] =
            if (persistFileStamps) repo else new FileStampRepository(cache, repo)
          val terminal = StandardMain.exchange
            .channelForName(channelName)
            .map(_.terminal)
            .getOrElse(Terminal.get)
          def cb: Continuous.Callbacks =
            Continuous.getCallbacks(state, channelName, commands, terminal, cache)
          val s = new ContinuousState(
            count,
            commands,
            (state, dynamicInputs) => {
              val original = state
                .get(globalFileTreeRepository)
                .getOrElse(
                  throw new IllegalStateException(
                    s"No global file tree repository for state $state"
                  )
                )
              val stateWithRepo =
                state.put(globalFileTreeRepository, cachingRepo).put(stashedRepo, original)
              val stateWithCache =
                if (persistFileStamps) stateWithRepo.put(persistentFileStampCache, cache)
                else stateWithRepo
              stateWithCache.put(Continuous.DynamicInputs, dynamicInputs)
            },
            state => {
              watchStates.get(channelName) match {
                case null =>
                case ws   => watchStates.put(channelName, ws.incremented)
              }
              val restoredState = state.get(stashedRepo) match {
                case None    => throw new IllegalStateException(s"No stashed repository for $state")
                case Some(r) => state.put(globalFileTreeRepository, r)
              }
              restoredState.remove(persistentFileStampCache).remove(Continuous.DynamicInputs)
            },
            () => {
              watchStates.remove(channelName)
              LogExchange.unbindLoggerAppenders(channelName + "-watch")
              repo.close()
            },
            cb
          )
          Util.ignoreResult(watchStates.put(channelName, s))
        case cs =>
          val cmd = cs.commands.mkString("; ")
          val msg =
            s"Tried to start new watch while channel, '$channelName', was already watching '$cmd'"
          throw new IllegalStateException(msg)
      }
    }
  private[this] def watchCommand(
      name: String
  )(updateState: (String, State) => State): Command =
    Command.arb { state =>
      (cmdParser(name) ~> channelParser).map(channel => () => updateState(channel, state))
    } { case (_, newState) => newState() }
  private[this] val runWatchCommand = watchCommand(runWatch) { (channel, state) =>
    watchStates.get(channel) match {
      case null => state
      case cs =>
        val pre = StashOnFailure :: s"$preWatch $channel" :: Nil
        val post = FailureWall :: PopOnFailure :: s"$postWatch $channel" :: Nil
        (pre ::: cs.commands.toList ::: post) ::: state
    }
  }
  private[sbt] def watchUIThreadFor(channel: CommandChannel): Option[UITask] =
    watchStates.get(channel.name) match {
      case null => None
      case cs   => Some(new WatchUITask(channel, cs))
    }
  private[this] class WatchUITask(
      override private[sbt] val channel: CommandChannel,
      cs: ContinuousState,
  ) extends Thread(s"sbt-${channel.name}-watch-ui-thread")
      with UITask {
    override private[sbt] def reader: UITask.Reader = () => {
      def stop = Right(s"${ContinuousCommands.stopWatch} ${channel.name}")
      val exitAction: Watch.Action =
        Watch.apply(
          cs.count,
          _ => (),
          cs.callbacks.onStart,
          cs.callbacks.nextEvent,
          recursive = false
        )
      exitAction match {
        case Watch.Trigger       => Right(s"${ContinuousCommands.runWatch} ${channel.name}")
        case Watch.Reload        => Right("reload")
        case Watch.Run(commands) => stop.map(_ +: commands.map(_.commandLine) mkString ";")
        case Watch.CancelWatch   => stop
        case _                   => stop
      }
    }
    override private[sbt] def onProgressEvent(
        pe: ProgressEvent,
        terminal: Terminal
    ): Unit = {}
  }
  @inline
  private[this] def watchState(channel: String): ContinuousState = watchStates.get(channel) match {
    case null => throw new IllegalStateException(s"No watch state for $channel")
    case s    => s
  }

  private[this] val preWatchCommand = watchCommand(preWatch)(watchState(_).beforeCommand(_))
  private[this] val postWatchCommand = watchCommand(postWatch)(watchState(_).afterCommand(_))
  private[this] val stopWatchCommand = watchCommand(stopWatch) { (channel, state) =>
    watchState(channel).afterWatch()
    state
  }
  /*
   * Creates a FileTreeRepository where it is safe to call close without inadvertently cancelling
   * still active watches.
   */
  private[this] def localRepo[T](r: FileTreeRepository[T]): FileTreeRepository[T] =
    new FileTreeRepository[T] {
      private[this] val closeables = ConcurrentHashMap.newKeySet[AutoCloseable]
      override def addObserver(observer: Observer[FileEvent[T]]): AutoCloseable = {
        val ac = r.addObserver(observer)
        val safeCloseable: AutoCloseable = () =>
          try ac.close()
          catch { case NonFatal(_) => }
        closeables.add(safeCloseable)
        () => {
          closeables.remove(safeCloseable)
          safeCloseable.close()
        }
      }

      override def register(glob: Glob): Either[IOException, Observable[FileEvent[T]]] =
        r.register(glob)
      override def close(): Unit = closeables.forEach { c =>
        try c.close()
        catch { case NonFatal(_) => }
      }
      override def list(path: Path): Seq[(Path, T)] = r.list(path)
    }

}
