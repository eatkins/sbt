/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util

import java.io.{ InputStream, OutputStream, PrintStream }
import java.nio.channels.ClosedChannelException
import java.util.Locale
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{ Executors, LinkedBlockingQueue }

import jline.DefaultTerminal2
import jline.console.ConsoleReader
import sbt.internal.util.ConsoleAppender.{ CursorLeft1000, DeleteLine }

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

trait Terminal extends AutoCloseable {

  /**
   * Gets the current width of the terminal. The implementation reads a property from the jline
   * config which is updated if it has been more than a second since the last update. It is thus
   * possible for this value to be stale.
   *
   * @return the terminal width.
   */
  def getWidth: Int

  /**
   * Gets the current height of the terminal. The implementation reads a property from the jline
   * config which is updated if it has been more than a second since the last update. It is thus
   * possible for this value to be stale.
   *
   * @return the terminal height.
   */
  def getHeight: Int

  /**
   * Returns the height and width of the current line that is displayed on the terminal. If the
   * most recently flushed byte is a newline, this will be `(0, 0)`.
   *
   * @return the (height, width) pair
   */
  def getLineHeightAndWidth: (Int, Int)

  /**
   * Returns the number of lines that the input string will cover given the current width of the
   * terminal.
   *
   * @param line the input line
   * @return the number of lines that the line will cover on the terminal
   */
  def lineCount(line: String): Int = {
    val width = getWidth
    val lines = EscHelpers.removeEscapeSequences(line).split('\n')
    def count(l: String): Int = {
      val len = l.length
      if (width > 0 && len > 0) (len - 1 + width) / width else 0
    }
    lines.tail.foldLeft(lines.headOption.fold(0)(count))(_ + count(_))
  }

  /**
   *
   */
  /**
   * Gets the input stream for this Terminal. This could be a wrapper around System.in for the
   * process or it could be a remote input stream for a network channel.
   * @return the input stream.
   */
  def inputStream: InputStream

  /**
   * Gets the input stream for this Terminal. This could be a wrapper around System.in for the
   * process or it could be a remote input stream for a network channel.
   * @return the input stream.
   */
  def outputStream: OutputStream

  /**
   * Returns true if the terminal supports ansi characters.
   *
   * @return true if the terminal supports ansi escape codes.
   */
  def isAnsiSupported: Boolean

  /**
   * Returns true if color is enabled for this terminal.
   *
   * @return true if color is enabled for this terminal.
   */
  def isColorEnabled: Boolean

  /**
   * Returns true if the terminal has echo enabled.
   *
   * @return true if the terminal has echo enabled.
   */
  def isEchoEnabled: Boolean

  /**
   * Returns true if the terminal has supershell enabled.
   *
   * @return true if the terminal has supershell enabled.
   */
  def isSupershellEnabled: Boolean

  /**
   * Returns the last line written to the terminal's output stream.
   * @return the last line
   */
  def getLastLine: Option[String]

  def getBooleanCapability(capability: String): Boolean
  def getNumericCapability(capability: String): Int
  def getStringCapability(capability: String): String

  private[sbt] def withRawSystemIn[T](f: => T): T = f
  private[sbt] def withCanonicalIn[T](f: => T): T = f
  private[sbt] def withEcho[T](f: => T): T = f
  private[sbt] def write(bytes: Int*): Unit
  private[sbt] def printStream: PrintStream
  private[sbt] def withPrintStream[T](f: PrintStream => T): T
  private[sbt] def restore(): Unit = {}
  private[sbt] val progressState = new ProgressState(1)
  private[this] val promptHolder: AtomicReference[Prompt] = new AtomicReference
  private[sbt] final def prompt: Prompt = promptHolder.get
  private[sbt] final def setPrompt(prompt: Prompt): Unit = promptHolder.set(prompt)
}

object Terminal {

  def close(): Unit = {
    if (System.console == null) {
      originalOut.close()
      originalIn.close()
      System.err.close()
    }
  }

  /**
   * Gets the current width of the terminal. The implementation reads a property from the jline
   * config which is updated if it has been more than a second since the last update. It is thus
   * possible for this value to be stale.
   *
   * @return the terminal width.
   */
  def getWidth: Int = console.getWidth

  /**
   * Gets the current height of the terminal. The implementation reads a property from the jline
   * config which is updated if it has been more than a second since the last update. It is thus
   * possible for this value to be stale.
   *
   * @return the terminal height.
   */
  def getHeight: Int = console.getHeight

  /**
   * Returns the height and width of the current line that is displayed on the terminal. If the
   * most recently flushed byte is a newline, this will be `(0, 0)`.
   *
   * @return the (height, width) pair
   */
  def getLineHeightAndWidth: (Int, Int) = console.getLineHeightAndWidth
  def getBooleanCapability(capability: String): Boolean = console.getBooleanCapability(capability)
  def getNumericCapability(capability: String): Int = console.getNumericCapability(capability)
  def getStringCapability(capability: String): String = console.getStringCapability(capability)

  /**
   * Returns the number of lines that the input string will cover given the current width of the
   * terminal.
   *
   * @param line the input line
   * @return the number of lines that the line will cover on the terminal
   */
  def lineCount(line: String): Int = {
    val width = getWidth
    val lines = EscHelpers.removeEscapeSequences(line).split('\n')
    def count(l: String): Int = {
      val len = l.length
      if (width > 0 && len > 0) (len - 1 + width) / width else 0
    }
    lines.tail.foldLeft(lines.headOption.fold(0)(count))(_ + count(_))
  }

  /**
   * Returns true if the current terminal supports ansi characters.
   *
   * @return true if the current terminal supports ansi escape codes.
   */
  def isAnsiSupported: Boolean =
    try console.isAnsiSupported
    catch { case NonFatal(_) => !isWindows }

  /**
   * Returns true if echo is enabled on the terminal.
   *
   * @return true if echo is enabled on the terminal.
   */
  def isEchoEnabled: Boolean = console.isEchoEnabled

  /**
   * Returns true if System.in is attached. When sbt is run as a subprocess, like in scripted or
   * as a server, System.in will not be attached and this method will return false. Otherwise
   * it will return true.
   *
   * @return true if System.in is attached.
   */
  def systemInIsAttached: Boolean = attached.get

  def read: Int = inputStream.get match {
    case null => -1
    case is   => is.read
  }

  /**
   * Returns an InputStream that will throw a [[ClosedChannelException]] if read returns -1.
   * @return the wrapped InputStream.
   */
  private[sbt] def throwOnClosedSystemIn(in: InputStream): InputStream = new InputStream {
    override def available(): Int = in.available()
    override def read(): Int = in.read() match {
      case -1          => throw new ClosedChannelException
      case r if r >= 0 => r
      case _           => -1
    }
  }

  /**
   * Provides a wrapper around System.in. The wrapped stream in will check if the terminal is attached
   * in available and read. If a read returns -1, it will mark System.in as unattached so that
   * it can be detected by [[systemInIsAttached]].
   *
   * @return the wrapped InputStream
   */
  private[sbt] def wrappedSystemIn: InputStream = WrappedSystemIn

  /**
   * Restore the terminal to its initial state.
   */
  private[sbt] def restore(): Unit = console.toJLine.restore()

  /**
   * Runs a thunk ensuring that the terminal has echo enabled. Most of the time sbt should have
   * echo mode on except when it is explicitly set to raw mode via [[Terminal.withRawSystemIn]].
   *
   * @param f the thunk to run
   * @tparam T the result type of the thunk
   * @return the result of the thunk
   */
  private[sbt] def withEcho[T](toggle: Boolean)(f: => T): T = {
    val previous = console.isEchoEnabled
    terminalLock.lockInterruptibly()
    try {
      console.toJLine.setEchoEnabled(toggle)
      f
    } finally {
      console.toJLine.setEchoEnabled(previous)
      terminalLock.unlock()
    }
  }

  /**
   *
   * @param f the thunk to run
   * @tparam T the result type of the thunk
   * @return the result of the thunk
   */
  private[sbt] def withStreams[T](f: => T): T =
    if (System.getProperty("sbt.io.virtual", "true") == "true") {
      withOut(withIn(f))
    } else f

  private[this] object ProxyTerminal extends Terminal {
    private def t: Terminal = currentTerminal.get
    override def getWidth: Int = t.getWidth
    override def getHeight: Int = t.getHeight
    override def getLineHeightAndWidth: (Int, Int) = t.getLineHeightAndWidth
    override def lineCount(line: String): Int = t.lineCount(line)
    override def inputStream: InputStream = t.inputStream
    override def outputStream: OutputStream = t.outputStream
    override def isAnsiSupported: Boolean = t.isAnsiSupported
    override def isColorEnabled: Boolean = t.isColorEnabled
    override def isEchoEnabled: Boolean = t.isEchoEnabled
    override def isSupershellEnabled: Boolean = t.isSupershellEnabled
    override def getBooleanCapability(capability: String): Boolean =
      t.getBooleanCapability(capability)
    override def getNumericCapability(capability: String): Int = t.getNumericCapability(capability)
    override def getStringCapability(capability: String): String = t.getStringCapability(capability)
    override def withRawSystemIn[T](f: => T): T = t.withRawSystemIn(f)
    override def withCanonicalIn[T](f: => T): T = t.withCanonicalIn(f)
    override def withEcho[T](f: => T): T = t.withEcho(f)
    override def printStream: PrintStream = t.printStream
    override def withPrintStream[T](f: PrintStream => T): T = t.withPrintStream(f)
    override def restore(): Unit = t.restore()
    override def close(): Unit = {}
    override private[sbt] def write(bytes: Int*): Unit = t.write(bytes: _*)
    override def getLastLine: Option[String] = t.getLastLine
  }
  private[sbt] def get: Terminal = ProxyTerminal

  private[sbt] def withTerminal[T](terminal: Terminal)(f: => T): T = {
    val originalJLine = jline.TerminalFactory.get
    val original = currentTerminal.getAndSet(terminal)
    try {
      jline.TerminalFactory.set(terminal.toJLine)
      withIn(terminal.inputStream)(withOut(terminal.printStream)(f))
    } finally {
      jline.TerminalFactory.set(originalJLine)
      currentTerminal.set(original)
    }
  }
  private[sbt] def withIn[T](in: InputStream)(f: => T): T = {
    val original = inputStream.get
    try {
      val wrapped = new WrappedInputStream(in)
      inputStream.set(wrapped)
      System.setIn(wrapped)
      scala.Console.withIn(in)(f)
    } finally {
      inputStream.set(original)
      System.setIn(original)
    }
  }
  private[this] class WrappedInputStream(val in: InputStream) extends InputStream {
    override def read(): Int = in.read
    override def read(b: Array[Byte]): Int = in.read(b)
    override def read(b: Array[Byte], off: Int, len: Int): Int = in.read(b, off, len)
    override def skip(n: Long): Long = in.skip(n)
    override def available(): Int = in.available()
    override def close(): Unit = in.close()
    override def mark(readlimit: Int): Unit = in.mark(readlimit)
    override def reset(): Unit = in.reset()
    override def markSupported(): Boolean = in.markSupported()
  }

  private[sbt] def withOut[T](out: PrintStream)(f: => T): T = {
    val originalOut = System.out
    val originalProxyOut = ConsoleOut.getGlobalProxy
    try {
      ConsoleOut.setGlobalProxy(ConsoleOut.printStreamOut(out))
      System.setOut(out)
      scala.Console.withOut(out)(f)
    } finally {
      ConsoleOut.setGlobalProxy(originalProxyOut)
      System.setOut(originalOut)
    }
  }

  private[this] val originalOut = System.out
  private[this] val originalIn = System.in
  private[this] class WriteableInputStream(in: InputStream, name: String)
      extends InputStream
      with AutoCloseable {
    final def write(bytes: Int*): Unit = bytes.foreach(buffer.put)
    private[this] val executor =
      Executors.newSingleThreadExecutor(r => new Thread(r, s"sbt-$name-input-reader"))
    private[this] val buffer = new LinkedBlockingQueue[Int]
    private[this] val readFuture = new AtomicReference[java.util.concurrent.Future[_]]()
    private[this] val runnable: Runnable = () =>
      try buffer.put(in.read)
      finally readFuture.set(null)
    override def read(): Int = {
      readFuture.synchronized(readFuture.get match {
        case null => readFuture.set(executor.submit(runnable))
        case _    =>
      })
      buffer.take match {
        case -1 => throw new ClosedChannelException
        case b  => b
      }
    }

    override def available(): Int = buffer.size
    override def close(): Unit = {
      executor.shutdownNow()
      ()
    }
  }
  private[this] val nonBlockingIn: WriteableInputStream =
    new WriteableInputStream(originalIn, "console")
  private[this] val inputStream = new AtomicReference[InputStream](System.in)
  private[this] def withOut[T](f: => T): T = {
    try {
      System.setOut(console.printStream)
      scala.Console.withOut(console.printStream)(f)
    } finally {
      System.setOut(originalOut)
    }
  }
  private[this] def withIn[T](f: => T): T =
    try {
      inputStream.set(Terminal.wrappedSystemIn)
      System.setIn(Terminal.wrappedSystemIn)
      scala.Console.withIn(Terminal.wrappedSystemIn)(f)
    } finally System.setIn(nonBlockingIn)

  private[sbt] def withPrintStream[T](f: PrintStream => T): T = console.withPrintStream(f)
  private[this] val terminalLock = new ReentrantLock()
  private[this] val attached = new AtomicBoolean(true)
  private[this] val terminalHolder = new AtomicReference(wrap(jline.TerminalFactory.get))
  private[this] val currentTerminal = new AtomicReference[Terminal](terminalHolder.get)
  private[this] lazy val isWindows =
    System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).indexOf("windows") >= 0
  private[this] object WrappedSystemIn extends InputStream {
    private[this] val in = nonBlockingIn
    override def available(): Int = if (attached.get) in.available() else 0
    override def read(): Int = synchronized {
      if (attached.get) {
        val res = in.read()
        if (res == -1) attached.set(false)
        res
      } else -1
    }
  }

  private[this] def wrap(terminal: jline.Terminal): Terminal = {
    val term: jline.Terminal with jline.Terminal2 = new jline.Terminal with jline.Terminal2 {
      private[this] val hasConsole = System.console != null
      private[this] def alive = hasConsole && attached.get
      private[this] val term2: jline.Terminal2 = terminal match {
        case t: jline.Terminal2 => t
        case _                  => new DefaultTerminal2(terminal)
      }
      override def init(): Unit = if (alive) terminal.init()
      override def restore(): Unit = if (alive) terminal.restore()
      override def reset(): Unit = if (alive) terminal.reset()
      override def isSupported: Boolean = terminal.isSupported
      override def getWidth: Int = terminal.getWidth
      override def getHeight: Int = terminal.getHeight
      override def isAnsiSupported: Boolean = terminal.isAnsiSupported
      override def wrapOutIfNeeded(out: OutputStream): OutputStream = terminal.wrapOutIfNeeded(out)
      override def wrapInIfNeeded(in: InputStream): InputStream = terminal.wrapInIfNeeded(in)
      override def hasWeirdWrap: Boolean = terminal.hasWeirdWrap
      override def isEchoEnabled: Boolean = terminal.isEchoEnabled
      override def setEchoEnabled(enabled: Boolean): Unit = if (alive) {
        terminal.setEchoEnabled(enabled)
      }
      override def disableInterruptCharacter(): Unit =
        if (alive) terminal.disableInterruptCharacter()
      override def enableInterruptCharacter(): Unit =
        if (alive) terminal.enableInterruptCharacter()
      override def getOutputEncoding: String = terminal.getOutputEncoding
      override def getBooleanCapability(capability: String): Boolean =
        term2.getBooleanCapability(capability)
      override def getNumericCapability(capability: String): Integer =
        term2.getNumericCapability(capability)
      override def getStringCapability(capability: String): String = {
        term2.getStringCapability(capability)
      }
    }
    term.restore()
    term.setEchoEnabled(true)
    new JLineTerminal(term, nonBlockingIn, originalOut)
  }

  private[util] def reset(): Unit = {
    jline.TerminalFactory.reset()
    console.close()
    terminalHolder.set(wrap(jline.TerminalFactory.get))
  }

  // translate explicit class names to type in order to support
  //  older Scala, since it shaded classes but not the system property
  private[this] def fixTerminalProperty(): Unit = {
    val terminalProperty = "jline.terminal"
    val newValue = System.getProperty(terminalProperty) match {
      case "jline.UnixTerminal"                             => "unix"
      case null if System.getProperty("sbt.cygwin") != null => "unix"
      case "jline.WindowsTerminal"                          => "windows"
      case "jline.AnsiWindowsTerminal"                      => "windows"
      case "jline.UnsupportedTerminal"                      => "none"
      case x                                                => x
    }
    if (newValue != null) {
      System.setProperty(terminalProperty, newValue)
      ()
    }
  }
  fixTerminalProperty()

  private[sbt] def createReader(term: Terminal, prompt: Prompt): ConsoleReader = {
    new ConsoleReader(term.inputStream, prompt.wrappedOutputStream(term), term.toJLine) {
      override def readLine(prompt: String, mask: Character): String =
        term.withRawSystemIn(super.readLine(prompt, mask))
      override def readLine(prompt: String): String = term.withRawSystemIn(super.readLine(prompt))
    }
  }

  private[sbt] def console: Terminal = terminalHolder.get match {
    case null => throw new IllegalStateException("Uninitialized terminal.")
    case term => term
  }

  @deprecated("For compatibility only", "1.4.0")
  private[sbt] def deprecatedTeminal: jline.Terminal = console.toJLine
  private[sbt] implicit class TerminalOps(private val term: Terminal) extends AnyVal {
    def toJLine: jline.Terminal with jline.Terminal2 = term match {
      case t: JLineTerminal => t.term
      case _ =>
        new jline.Terminal with jline.Terminal2 {
          override def init(): Unit = {}
          override def restore(): Unit = {}
          override def reset(): Unit = {}
          override def isSupported: Boolean = true
          override def getWidth: Int = term.getWidth
          override def getHeight: Int = term.getHeight
          override def isAnsiSupported: Boolean = term.isAnsiSupported
          override def wrapOutIfNeeded(out: OutputStream): OutputStream = out
          override def wrapInIfNeeded(in: InputStream): InputStream = in
          override def hasWeirdWrap: Boolean = false
          override def isEchoEnabled: Boolean = term.isEchoEnabled
          override def setEchoEnabled(enabled: Boolean): Unit = {}
          override def disableInterruptCharacter(): Unit = {}
          override def enableInterruptCharacter(): Unit = {}
          override def getOutputEncoding: String = null
          override def getBooleanCapability(capability: String): Boolean = {
            term.getBooleanCapability(capability)
          }
          override def getNumericCapability(capability: String): Integer = {
            term.getNumericCapability(capability)
          }
          override def getStringCapability(capability: String): String = {
            term.getStringCapability(capability)
          }
        }
    }
  }
  private class JLineTerminal(
      val term: jline.Terminal with jline.Terminal2,
      in: InputStream,
      out: OutputStream
  ) extends TerminalImpl(in, out, "console") {
    override def getWidth: Int = term.getWidth
    override def getHeight: Int = term.getHeight
    override def isAnsiSupported: Boolean = term.isAnsiSupported
    override def isEchoEnabled: Boolean = term.isEchoEnabled
    override def getBooleanCapability(capability: String): Boolean =
      term.getBooleanCapability(capability)
    override def getNumericCapability(capability: String): Int =
      term.getNumericCapability(capability)
    override def getStringCapability(capability: String): String =
      term.getStringCapability(capability)
    override private[sbt] def restore(): Unit = term.restore()

    override def withRawSystemIn[T](f: => T): T = term.synchronized {
      try {
        term.init()
        term.setEchoEnabled(false)
        f
      } finally {
        term.restore()
        term.setEchoEnabled(true)
      }
    }
    override def isColorEnabled: Boolean = ConsoleAppender.formatEnabledInEnv

    override def isSupershellEnabled: Boolean = System.getProperty("sbt.supershell") match {
      case null   => !sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") && isColorEnabled
      case "true" => true
      case _      => false
    }
  }
  private[sbt] abstract class TerminalImpl(val in: InputStream, val out: OutputStream, name: String)
      extends Terminal {
    private[this] val currentLine = new AtomicReference(new ArrayBuffer[Byte])
    private[this] val lineBuffer = new LinkedBlockingQueue[Byte]
    private[this] val flushQueue = new LinkedBlockingQueue[Seq[Byte]]
    private[this] val writeLock = new AnyRef
    private[this] val writeableInputStream = in match {
      case w: WriteableInputStream => w
      case _                       => new WriteableInputStream(in, name)
    }

    override val outputStream: OutputStream = new OutputStream {
      override def write(b: Int): Unit = {
        writeLock.synchronized {
          lineBuffer.put(b.toByte)
          if (b == 10) flush()
        }
      }
      override def write(b: Array[Byte]): Unit = write(b, 0, b.length)
      override def write(b: Array[Byte], off: Int, len: Int): Unit = writeLock.synchronized {
        val lo = math.max(0, off)
        val hi = math.min(math.max(off + len, 0), b.length)
        (lo until hi).foreach(i => lineBuffer.put(b(i)))
      }
      val pBytes = s"scala-compile".getBytes
      override def flush(): Unit = writeLock.synchronized {
        val res = new VectorBuilder[Byte]
        while (!lineBuffer.isEmpty) res += lineBuffer.poll
        val bytes = res.result
//        if (!bytes.lastOption.contains(10.toByte) && bytes.nonEmpty) {
//          new Exception(s"flush ${bytes.lastOption} ${bytes.map(_.toChar)}")
//            .printStackTrace(System.err)
//        }
        if (bytes.nonEmpty) flushQueue.put(res.result())
      }
    }
    override private[sbt] val printStream: PrintStream = new PrintStream(outputStream, true)
    override def inputStream: InputStream = writeableInputStream

    private[sbt] def write(bytes: Int*): Unit = writeableInputStream.write(bytes: _*)
    private[this] val isStopped = new AtomicBoolean(false)
    import scala.collection.JavaConverters._
    private[this] object WriteThread extends Thread(s"sbt-stdout-write-thread-$name") {
      setDaemon(true)
      def close(): Unit = {
        isStopped.set(true)
        interrupt()
        runOnce(lineBuffer.asScala.toVector)
        join()
        ()
      }
      private[this] val clear = s"$DeleteLine$CursorLeft1000"
      def runOnce(bytes: Seq[Byte]): Unit = {
        writeLock.synchronized {
          val remaining = bytes.foldLeft(new ArrayBuffer[Byte]) { (buf, i) =>
            def write(b: Byte): Unit = out.write(b & 0xFF)
            if (i == 10) {
              progressState.addBytes(buf)
              progressState.clearBytes()
              if (buf.nonEmpty && isAnsiSupported) clear.getBytes.foreach(write)
              buf.foreach(write)
              write(10)
              val cl = new ArrayBuffer[Byte]
              val p = Option(prompt)
                .map(_.render())
                .getOrElse(if (getLineHeightAndWidth._2 > 0) clear else "")
              if (p.nonEmpty) {
                p.getBytes.foreach(write)
                cl ++= p.getBytes
              }
              currentLine.set(cl)
              new ArrayBuffer[Byte]
            } else buf += i
          }
          if (remaining.nonEmpty) {
            currentLine.get ++= remaining
            out.write(remaining.toArray)
          }
          out.flush()
        }
      }
      @tailrec override def run(): Unit = {
        try runOnce(flushQueue.take())
        catch { case _: InterruptedException => isStopped.set(true) }
        if (!isStopped.get) run()
      }
    }
    WriteThread.start()

    override def getLineHeightAndWidth: (Int, Int) = currentLine.get.toArray match {
      case bytes if bytes.isEmpty => (0, 0)
      case bytes =>
        val width = getWidth
        val line = EscHelpers.stripColorsAndMoves(new String(bytes))
        val count = lineCount(line)
        (count, line.length - ((count - 1) * width))
    }

    override def getLastLine: Option[String] = currentLine.get.toArray match {
      case bytes if bytes.isEmpty => None
      case bytes                  => Some(new String(bytes))
    }

    private[this] val rawPrintStream: PrintStream = new PrintStream(out, true) {
      override def close(): Unit = {}
    }
    override def withPrintStream[T](f: PrintStream => T): T =
      writeLock.synchronized(f(rawPrintStream))

    override def close(): Unit = {
      isStopped.set(true)
      writeableInputStream.close()
      WriteThread.close()
    }
  }
}
