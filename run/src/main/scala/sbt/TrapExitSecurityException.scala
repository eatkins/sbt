/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

/**
 * A custom SecurityException that tries not to be caught.  Closely based on a similar class in Nailgun.
 * The main goal of this exception is that once thrown, it propagates all of the way up the call stack,
 * terminating the thread's execution.
 */
private final class TrapExitSecurityException(val exitCode: Int) extends SecurityException {
  override def printStackTrace: Unit = throw this
  override def toString: String = throw this
  override def getCause: Throwable = throw this
  override def getMessage: String = throw this
  override def fillInStackTrace: Throwable = throw this
  override def getLocalizedMessage: String = throw this
}
