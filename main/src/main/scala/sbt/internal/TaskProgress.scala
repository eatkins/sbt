/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal

import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger, AtomicReference }
import java.util.concurrent.{ LinkedBlockingQueue, TimeUnit }

import sbt.internal.util._

import scala.annotation.tailrec
import scala.concurrent.duration._

object TaskProgress extends TaskProgress

/**
 * implements task progress display on the shell.
 */
private[sbt] class TaskProgress private ()
    extends AbstractTaskExecuteProgress
    with ExecuteProgress[Task] {
  private[this] val lastTaskCount = new AtomicInteger(0)
  private[this] val currentProgressThread = new AtomicReference[Option[ProgressThread]](None)
  private[this] val sleepDuration = SysProp.supershellSleep.millis
  private[this] val threshold = 10.millis
  private[this] val tasks = new LinkedBlockingQueue[Task[_]]
  private[this] final class ProgressThread
      extends Thread("task-progress-report-thread")
      with AutoCloseable {
    private[this] val isClosed = new AtomicBoolean(false)
    private[this] val firstTime = new AtomicBoolean(true)
    private[this] val hasReported = new AtomicBoolean(false)
    private[this] def doReport(): Unit = { hasReported.set(true); report() }
    setDaemon(true)
    start()
    private def resetThread(): Unit =
      currentProgressThread.synchronized {
        currentProgressThread.getAndSet(None) match {
          case Some(t) if t != this => currentProgressThread.set(Some(t))
          case _                    =>
        }
      }
    @tailrec override def run(): Unit = {
      if (!isClosed.get() && (!hasReported.get || activeTasks(System.nanoTime).nonEmpty)) {
        try {
          val activeExceedingThreshold = activeTasks(System.nanoTime).filter {
            case (_, d) => d > threshold
          }
          if (activeExceedingThreshold.nonEmpty) doReport()
          val duration =
            if (firstTime.compareAndSet(true, activeExceedingThreshold.isEmpty)) threshold
            else sleepDuration
          val limit = duration.fromNow
          while (Deadline.now < limit && !isClosed.get && activeTasks(System.nanoTime).nonEmpty) {
            tasks.poll((limit - Deadline.now).toMillis, TimeUnit.MILLISECONDS)
            tasks.clear()
            doReport()
          }
        } catch {
          case _: InterruptedException =>
            isClosed.set(true)
            // One last report after close in case the last one hadn't gone through yet.
            doReport()

        }
        run()
      } else {
        resetThread()
      }
    }

    def addTask(task: Task[_]): Unit = tasks.put(task)

    override def close(): Unit = {
      isClosed.set(true)
      interrupt()
      report()
      appendProgress(ProgressEvent("Info", Vector(), None, None, None))
      resetThread()
    }
  }

  override def initial(): Unit = ()

  override def beforeWork(task: Task[_]): Unit = {
    maybeStartThread()
    super.beforeWork(task)
    tasks.put(task)
  }

  override protected def interactiveProgess: Boolean = true
  override def afterReady(task: Task[_]): Unit = maybeStartThread()

  override def afterCompleted[A](task: Task[A], result: Result[A]): Unit = maybeStartThread()

  override def stop(): Unit = currentProgressThread.synchronized {
    currentProgressThread.getAndSet(None).foreach(_.close())
  }

  override def afterAllCompleted(results: RMap[Task, Result]): Unit = {
    reset()
    // send an empty progress report to clear out the previous report
    appendProgress(ProgressEvent("Info", Vector(), Some(lastTaskCount.get), None, None))
  }
  private[this] val skipReportTasks =
    Set(
      "run",
      "runMain",
      "bgRun",
      "fgRun",
      "scala",
      "console",
      "consoleProject",
      "consoleQuick",
      "state"
    )
  private[this] def maybeStartThread(): Unit = {
    currentProgressThread.get() match {
      case None =>
        currentProgressThread.synchronized {
          currentProgressThread.get() match {
            case None => currentProgressThread.set(Some(new ProgressThread))
            case _    =>
          }
        }
      case _ =>
    }
  }
  private[this] def appendProgress(event: ProgressEvent): Unit =
    StandardMain.exchange.updateProgress(event)
  private[this] def report(): Unit = {
    val active = activeTasks(System.nanoTime)
    val currentTasks = active.collect { case (t, d) if d > threshold => (t, d.toMicros) }
    val ltc = lastTaskCount.get
    val currentTasksCount = currentTasks.size
    def event(tasks: Vector[(Task[_], Long)]): ProgressEvent = ProgressEvent(
      "Info",
      tasks
        .map { case (task, elapsed) => ProgressItem(taskName(task), elapsed) }
        .sortBy(_.elapsedMicros),
      Some(ltc),
      None,
      None,
      None,
      Some(containsSkipTasks(active))
    )
    if (active.nonEmpty) maybeStartThread()
    lastTaskCount.set(currentTasksCount)
    appendProgress(event(currentTasks))
  }

  private[this] def containsSkipTasks(tasks: Vector[(Task[_], FiniteDuration)]): Boolean = {
    tasks.iterator.map(t => taskName(t._1)).exists { n =>
      val shortName = n.lastIndexOf('/') match {
        case -1 => n
        case i =>
          var j = i + 1
          while (n(j) == ' ') j += 1
          n.substring(j)
      }
      skipReportTasks.contains(shortName)
    }
  }
}
