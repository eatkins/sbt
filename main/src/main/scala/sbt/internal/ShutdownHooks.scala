/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicInteger }

import scala.util.control.NonFatal

private[sbt] object ShutdownHooks extends AutoCloseable {
  private[this] val idGenerator = new AtomicInteger(0)
  private[this] val hooks = new ConcurrentHashMap[Int, () => Unit]
  private[this] val ranHooks = new AtomicBoolean(false)
  private[this] val thread = new Thread("shutdown-hooks-run-all") {
    override def run(): Unit = runAll()
  }
  private[this] val runtime = Runtime.getRuntime
  runtime.addShutdownHook(thread)
  private[sbt] def add[R](task: () => R): AutoCloseable = {
    val id = idGenerator.getAndIncrement()
    hooks.put(
      id,
      () =>
        try {
          task()
          ()
        } catch {
          case NonFatal(e) =>
            System.err.println(s"Caught exception running shutdown hook: $e")
            e.printStackTrace(System.err)
        }
    )
    () => Option(hooks.remove(id)).foreach(_.apply())
  }
  private def runAll(): Unit = if (ranHooks.compareAndSet(false, true)) {
    val n = System.nanoTime
    hooks.forEachValue(runtime.availableProcessors.toLong, (_: () => Unit).apply())
    val elapsed = System.nanoTime - n
    System.err.println(s"took ${elapsed / 1.0e6} ms to run shutdown hooks")
  }
  override def close(): Unit = {
    runtime.removeShutdownHook(thread)
    runAll()
  }
}
