/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util

import java.io.{ InputStream, InterruptedIOException, OutputStream, PrintStream }
import java.nio.channels.ClosedChannelException
import java.util.{ Arrays, Locale }
import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }
import java.util.concurrent.{ ArrayBlockingQueue, Executors, LinkedBlockingQueue, TimeUnit }

import jline.DefaultTerminal2
import jline.console.ConsoleReader
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try
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
  def getLineHeightAndWidth(line: String): (Int, Int)

  /**
   * Gets the input stream for this Terminal. This could be a wrapper around System.in for the
   * process or it could be a remote input stream for a network channel.
   * @return the input stream.
   */
  def inputStream: InputStream

  /**
   * Gets the output stream for this Terminal.
   * @return the output stream.
   */
  def outputStream: OutputStream

  /**
   * Gets the error stream for this Terminal.
   * @return the error stream.
   */
  def errorStream: OutputStream

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
   * Returns true if the terminal has success enabled, which it may not if it is for batch
   * commands because the client will print the success results when received from the
   * server.
   *
   * @return true if the terminal has success enabled
   */
  def isSuccessEnabled: Boolean

  /**
   * Returns true if the terminal has supershell enabled.
   *
   * @return true if the terminal has supershell enabled.
   */
  def isSupershellEnabled: Boolean

  /*
   * The methods below this comment are implementation details that are in
   * some cases specific to jline2. These methods may need to change or be
   * removed if/when sbt upgrades to jline 3.
   */

  /**
   * Returns the last line written to the terminal's output stream.
   * @return the last line
   */
  private[sbt] def getLastLine: Option[String]

  /**
   * Returns the buffered lines that have been written to the terminal. The
   * main use case is to display the system startup log lines when a client
   * connects to a booting server. This could also be used to implement a more
   * tmux like experience where multiple clients connect to the same console.
   *
   * @return the lines
   */
  private[sbt] def getLines: Seq[String]

  private[sbt] def getBooleanCapability(capability: String, jline3: Boolean): Boolean
  private[sbt] def getNumericCapability(capability: String, jline3: Boolean): Integer
  private[sbt] def getStringCapability(capability: String, jline3: Boolean): String
  private[sbt] def getAttributes: Map[String, String]
  private[sbt] def setAttributes(attributes: Map[String, String]): Unit
  private[sbt] def setSize(width: Int, height: Int): Unit

  private[sbt] def name: String
  private[sbt] def withRawInput[T](f: => T): T = f
  private[sbt] def withCanonicalIn[T](f: => T): T = f
  private[sbt] def write(bytes: Int*): Unit
  private[sbt] def printStream: PrintStream
  private[sbt] def withPrintStream[T](f: PrintStream => T): T
  private[sbt] def withRawOutput[R](f: => R): R
  private[sbt] def restore(): Unit = {}
  private[sbt] val progressState = new ProgressState(1)
  private[this] val promptHolder: AtomicReference[Prompt] = new AtomicReference(Prompt.Running)
  private[sbt] final def prompt: Prompt = promptHolder.get
  private[sbt] final def setPrompt(newPrompt: Prompt): Unit =
    if (prompt != Prompt.NoPrompt) promptHolder.set(newPrompt)

  /**
   * Returns the number of lines that the input string will cover given the current width of the
   * terminal.
   *
   * @param line the input line
   * @return the number of lines that the line will cover on the terminal
   */
  private[sbt] def lineCount(line: String): Int = {
    val lines = EscHelpers.stripColorsAndMoves(line).split('\n')
    val width = getWidth
    def count(l: String): Int = {
      val len = l.length
      if (width > 0 && len > 0) (len - 1 + width) / width else 0
    }
    if (lines.nonEmpty) lines.tail.foldLeft(lines.headOption.fold(0)(count))(_ + count(_))
    else 0
  }

}

object Terminal {
  // Disable noisy jline log spam
  if (System.getProperty("sbt.jline.verbose", "false") != "true")
    jline.internal.Log.setOutput(new PrintStream(_ => {}, false))
  def consoleLog(string: String): Unit = {
    Terminal.console.printStream.println(s"[info] $string")
  }
  private[sbt] def set(terminal: Terminal) = {
    activeTerminal.set(terminal)
    jline.TerminalFactory.set(terminal.toJLine)
  }
  implicit class TerminalOps(private val term: Terminal) extends AnyVal {
    def ansi(richString: => String, string: => String): String =
      if (term.isAnsiSupported) richString else string
    /*
     * Whenever we are dealing with JLine, which is true in sbt's ConsoleReader
     * as well as in the scala `console` task, we need to provide a jline.Terminal2
     * instance that can be consumed by the ConsoleReader. The ConsoleTerminal
     * already wraps a jline terminal, so we can just return the wrapped jline
     * terminal.
     */
    private[sbt] def toJLine: jline.Terminal with jline.Terminal2 = term match {
      case t: ConsoleTerminal => t.term
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
            term.getBooleanCapability(capability, jline3 = false)
          }
          override def getNumericCapability(capability: String): Integer = {
            term.getNumericCapability(capability, jline3 = false)
          }
          override def getStringCapability(capability: String): String = {
            term.getStringCapability(capability, jline3 = false)
          }
        }
    }
  }

  /*
   * Closes the standard input and output streams for the process. This allows
   * the sbt client to detach from the server it launches.
   */
  def close(): Unit = {
    if (System.console == null) {
      originalOut.close()
      originalIn.close()
      originalErr.close()
    }
  }

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

  private[this] val hasProgress: AtomicBoolean = new AtomicBoolean(false)

  /**
   *
   * @param progress toggles whether or not the console terminal has progress
   * @param f the thunk to run
   * @tparam T the result type of the thunk
   * @return the result of the thunk
   */
  private[sbt] def withStreams[T](isServer: Boolean)(f: => T): T =
    if (System.getProperty("sbt.io.virtual", "true") == "true") {
      hasProgress.set(isServer)
      try withOut(withIn(f))
      finally {
        jline.TerminalFactory.reset()
        if (isServer) {
          console match {
            case c: ConsoleTerminal if !isWindows =>
              /*
               * Entering raw mode in this way causes the standard in InputStream
               * to become non-blocking. After we set it to non-blocking, we spin
               * up a thread that reads from the inputstream and the resets it
               * back to blocking mode. We can then close the console. We do
               * this on a background thread to avoid blocking sbt's exit.
               */
              val prev = c.system.enterRawMode()
              val runnable: Runnable = () => {
                c.inputStream.read()
                c.system.setAttributes(prev)
                c.close()
              }
              val thread = new Thread(runnable, "sbt-console-background-close")
              thread.setDaemon(true)
              thread.start()
            case c => c.close()
          }
        } else {
          console.close()
        }
      }
    } else f

  private[this] object ProxyTerminal extends Terminal {
    private def t: Terminal = activeTerminal.get
    override def getWidth: Int = t.getWidth
    override def getHeight: Int = t.getHeight
    override def getLineHeightAndWidth(line: String): (Int, Int) = t.getLineHeightAndWidth(line)
    override def lineCount(line: String): Int = t.lineCount(line)
    override def inputStream: InputStream = t.inputStream
    override def outputStream: OutputStream = t.outputStream
    override def errorStream: OutputStream = t.errorStream
    override def isAnsiSupported: Boolean = t.isAnsiSupported
    override def isColorEnabled: Boolean = t.isColorEnabled
    override def isEchoEnabled: Boolean = t.isEchoEnabled
    override def isSuccessEnabled: Boolean = t.isSuccessEnabled
    override def isSupershellEnabled: Boolean = t.isSupershellEnabled
    override def getBooleanCapability(capability: String, jline3: Boolean): Boolean =
      t.getBooleanCapability(capability, jline3)
    override def getNumericCapability(capability: String, jline3: Boolean): Integer =
      t.getNumericCapability(capability, jline3)
    override def getStringCapability(capability: String, jline3: Boolean): String =
      t.getStringCapability(capability, jline3)
    override private[sbt] def getAttributes: Map[String, String] = t.getAttributes
    override private[sbt] def setAttributes(attributes: Map[String, String]): Unit =
      t.setAttributes(attributes)
    override private[sbt] def setSize(width: Int, height: Int): Unit = t.setSize(width, height)
    override def withRawInput[T](f: => T): T = t.withRawInput(f)
    override def withCanonicalIn[T](f: => T): T = t.withCanonicalIn(f)
    override def printStream: PrintStream = t.printStream
    override def withPrintStream[T](f: PrintStream => T): T = t.withPrintStream(f)
    override private[sbt] def withRawOutput[R](f: => R): R = t.withRawOutput(f)
    override def restore(): Unit = t.restore()
    override def close(): Unit = {}
    override private[sbt] def write(bytes: Int*): Unit = t.write(bytes: _*)
    override def getLastLine: Option[String] = t.getLastLine
    override def getLines: Seq[String] = t.getLines
    override private[sbt] def name: String = t.name
  }
  private[sbt] def get: Terminal = ProxyTerminal

  private[sbt] def withIn[T](in: InputStream)(f: => T): T = {
    val original = inputStream.get
    try {
      inputStream.set(in)
      System.setIn(in)
      scala.Console.withIn(in)(f)
    } finally {
      inputStream.set(original)
      System.setIn(original)
    }
  }

  private[sbt] def withOut[T](out: PrintStream)(f: => T): T = {
    val originalOut = System.out
    val originalErr = System.err
    val originalProxyOut = ConsoleOut.getGlobalProxy
    try {
      ConsoleOut.setGlobalProxy(ConsoleOut.printStreamOut(out))
      System.setOut(out)
      System.setErr(out)
      scala.Console.withErr(out)(scala.Console.withOut(out)(f))
    } finally {
      ConsoleOut.setGlobalProxy(originalProxyOut)
      System.setOut(originalOut)
      System.setErr(originalErr)
    }
  }

  val sepBytes = System.lineSeparator.getBytes("UTF-8")
  private class LinePrintStream(outputStream: OutputStream)
      extends PrintStream(outputStream, true) {
    override def println(s: String): Unit = synchronized {
      out.write(s.getBytes("UTF-8") ++ sepBytes)
      out.flush()
    }
  }
  private[this] val originalOut = new LinePrintStream(System.out)
  private[this] val originalErr = System.err
  private[this] val originalIn = System.in
  private[sbt] class WriteableInputStream(in: InputStream, name: String)
      extends InputStream
      with AutoCloseable {
    final def write(bytes: Int*): Unit = waiting.synchronized {
      waiting.poll match {
        case null =>
          bytes.foreach(b => buffer.put(b))
        case w =>
          if (bytes.length > 1) bytes.tail.foreach(b => buffer.put(b))
          bytes.headOption.foreach(b => w.put(b))
      }
    }
    private[this] val executor =
      Executors.newSingleThreadExecutor(r => new Thread(r, s"sbt-$name-input-reader"))
    private[this] val buffer = new LinkedBlockingQueue[Integer]
    private[this] val closed = new AtomicBoolean(false)
    private[this] val readQueue = new LinkedBlockingQueue[Unit]
    private[this] val waiting = new ArrayBlockingQueue[LinkedBlockingQueue[Integer]](1)
    private[this] val readThread = new AtomicReference[Thread]
    /*
     * Starts a loop that waits for consumers of the InputStream to call read.
     * When read is called, we enqueue a `LinkedBlockingQueue[Int]` to which
     * the runnable can return a byte from stdin. If the read caller is interrupted,
     * they remove the result from the waiting set and any byte read will be
     * enqueued in the buffer. It is done this way so that we only read from
     * System.in when a caller actually asks for bytes. If we constantly poll
     * from System.in, then when the user calls reboot from the console, the
     * first character they type after reboot is swallowed by the previous
     * sbt main program. If the user calls reboot from a remote client, we
     * can't avoid losing the first byte inputted in the console. A more
     * robust fix would be to override System.in at the launcher level instead
     * of at the sbt level. At the moment, the use case of a user calling
     * reboot from a network client and the adding input at the server console
     * seems pathological enough that it isn't worth putting more effort into
     * fixing.
     *
     */
    private[this] val runnable: Runnable = () => {
      @tailrec def impl(): Unit = {
        val _ = readQueue.take
        val b = in.read
        // The downstream consumer may have been interrupted. Buffer the result
        // when that hapens.
        waiting.poll match {
          case null => buffer.put(b)
          case q    => q.put(b)
        }
        if (b != -1 && !Thread.interrupted()) impl()
        else closed.set(true)
      }
      try impl()
      catch { case _: InterruptedException => closed.set(true) }
    }
    executor.submit(runnable)
    override def read(): Int =
      if (closed.get) -1
      else
        synchronized {
          readThread.set(Thread.currentThread)
          try buffer.poll match {
            case null =>
              val result = new LinkedBlockingQueue[Integer]
              waiting.synchronized(waiting.put(result))
              readQueue.put(())
              try result.take.toInt
              catch {
                case e: InterruptedException =>
                  waiting.remove(result)
                  -1
              }
            case b if b == -1 => throw new ClosedChannelException
            case b            => b.toInt
          } finally readThread.set(null)
        }
    def cancel(): Unit = waiting.synchronized {
      Option(readThread.getAndSet(null)).foreach(_.interrupt())
      waiting.forEach(_.put(-2))
      waiting.clear()
      readQueue.clear()
    }

    override def available(): Int = {
      buffer.size
    }
    override def close(): Unit = if (closed.compareAndSet(false, true)) {
      executor.shutdownNow()
      ()
    }
  }
  private[this] val nonBlockingIn: WriteableInputStream =
    new WriteableInputStream(jline.TerminalFactory.get.wrapInIfNeeded(originalIn), "console")

  private[this] val inputStream = new AtomicReference[InputStream](System.in)
  private[this] def withOut[T](f: => T): T = {
    try {
      System.setOut(proxyPrintStream)
      System.setErr(proxyErrorStream)
      scala.Console.withErr(proxyErrorStream)(scala.Console.withOut(proxyPrintStream)(f))
    } finally {
      System.setOut(originalOut)
      System.setErr(originalErr)
    }
  }
  private[this] def withIn[T](f: => T): T =
    try {
      inputStream.set(Terminal.wrappedSystemIn)
      System.setIn(wrappedSystemIn)
      scala.Console.withIn(proxyInputStream)(f)
    } finally System.setIn(originalIn)

  private[sbt] def withPrintStream[T](f: PrintStream => T): T = console.withPrintStream(f)
  private[this] val attached = new AtomicBoolean(true)

  /**
   * A wrapped instance of a jline.Terminal2 instance. It should only ever be changed when the
   * backgrounds sbt with ctrl+z and then foregrounds sbt which causes a call to reset. The
   * Terminal.console method returns this terminal and the ConsoleChannel delegates its
   * terminal method to it.
   */
  private[this] val consoleTerminalHolder = new AtomicReference(wrap(jline.TerminalFactory.get))

  /**
   * The terminal that is currently being used by the proxyInputStream and proxyOutputStream.
   * It is set through the Terminal.set method which is called by the SetTerminal command, which
   * is used to change the terminal during task evaluation. This allows us to route System.in and
   * System.out through the terminal's input and output streams.
   */
  private[this] val activeTerminal = new AtomicReference[Terminal](null)
  set(consoleTerminalHolder.get)

  /**
   * The boot input stream allows a remote client to forward input to the sbt process while
   * it is still loading. It works by updating proxyInputStream to read from the
   * value of bootInputStreamHolder if it is non-null as well as from the normal process
   * console io (assuming there is console io).
   */
  private[this] val bootInputStreamHolder = new AtomicReference[InputStream]

  /**
   * The boot output stream allows sbt to relay the bytes written to stdout to one or
   * more remote clients while the sbt build is loading and hasn't yet loaded a server.
   * The output stream of TerminalConsole is updated to write to value of
   * bootOutputStreamHolder when it is non-null as well as the normal process console
   * output stream.
   */
  private[this] val bootOutputStreamHolder = new AtomicReference[OutputStream]
  private[sbt] def setBootStreams(
      bootInputStream: InputStream,
      bootOutputStream: OutputStream
  ): Unit = {
    bootInputStreamHolder.set(bootInputStream)
    bootOutputStreamHolder.set(bootOutputStream)
  }

  private[this] object proxyInputStream extends InputStream {
    private[this] val isScripted = System.getProperty("sbt.scripted", "false") == "true"
    /*
     * This is to handle the case when a remote client starts sbt and the build fails.
     * We need to be able to consume input bytes from the remote client, but they
     * haven't yet connected to the main server but may be connected to the
     * BootServerSocket. Unfortunately there is no poll method on input stream that
     * takes a duration so we have to manually implement that here. All of the input
     * streams that we create in sbt are interruptible, so we can just poll each
     * of the input streams and periodically interrupt the thread to switch between
     * the two input streams.
     */
    private class ReadThread extends Thread with AutoCloseable {
      val result = new LinkedBlockingQueue[Integer]
      setDaemon(true)
      start()
      val running = new AtomicBoolean(true)
      override def run(): Unit = while (running.get) {
        bootInputStreamHolder.get match {
          case null =>
          case is =>
            def readFrom(inputStream: InputStream) =
              try {
                if (running.get) {
                  inputStream.read match {
                    case -1 =>
                    case i =>
                      result.put(i)
                      running.set(false)
                  }
                }
              } catch { case _: InterruptedException => }
            readFrom(is)
            readFrom(activeTerminal.get().inputStream)
        }
      }
      override def close(): Unit = if (running.compareAndSet(true, false)) this.interrupt()
    }
    def read(): Int = {
      if (isScripted) -1
      else if (bootInputStreamHolder.get == null) activeTerminal.get().inputStream.read()
      else {
        val thread = new ReadThread
        @tailrec def poll(): Int = thread.result.poll(10, TimeUnit.MILLISECONDS) match {
          case null =>
            thread.interrupt()
            poll()
          case i => i
        }
        poll()
      }
    }
  }
  private[this] object proxyOutputStream extends OutputStream {
    private[this] def os: OutputStream = activeTerminal.get().outputStream
    def write(byte: Int): Unit = {
      os.write(byte)
      os.flush()
      if (byte == 10) os.flush()
    }
    override def write(bytes: Array[Byte]): Unit = write(bytes, 0, bytes.length)
    override def write(bytes: Array[Byte], offset: Int, len: Int): Unit = {
      os.write(bytes, offset, len)
      os.flush()
    }
    override def flush(): Unit = os.flush()
  }
  private[this] val proxyPrintStream = new LinePrintStream(proxyOutputStream) {
    override def toString: String = s"proxyPrintStream($proxyOutputStream)"
  }
  private[this] object proxyErrorOutputStream extends OutputStream {
    private[this] def os: OutputStream = activeTerminal.get().errorStream
    def write(byte: Int): Unit = os.write(byte)
    override def write(bytes: Array[Byte]): Unit = write(bytes, 0, bytes.length)
    override def write(bytes: Array[Byte], offset: Int, len: Int): Unit =
      os.write(bytes, offset, len)
    override def flush(): Unit = os.flush()
  }
  private[this] val proxyErrorStream = new PrintStream(proxyErrorOutputStream, true)
  private[this] lazy val isWindows =
    System.getProperty("os.name", "").toLowerCase(Locale.ENGLISH).indexOf("windows") >= 0
  private[this] object WrappedSystemIn extends InputStream {
    private[this] val in = proxyInputStream
    override def available(): Int = if (attached.get) in.available() else 0
    override def read(): Int = synchronized {
      if (attached.get) {
        val res = in.read()
        if (res == -1) attached.set(false)
        res
      } else -1
    }
  }

  /*
   * When the server is booted by a remote client, it may not be able to accurately
   * calculate the terminal properties. To work around this, we can set the
   * properties via an environment property. It was too difficult to get system
   * properties working correctly with windows.
   */
  private class Props(
      val width: Int,
      val height: Int,
      val ansi: Boolean,
      val color: Boolean,
      val supershell: Boolean
  )
  private[sbt] val TERMINAL_PROPS = "SBT_TERMINAL_PROPS"
  private val props = System.getenv(TERMINAL_PROPS) match {
    case null => None
    case p =>
      p.split(",") match {
        case Array(width, height, ansi, color, supershell) =>
          Try(
            new Props(
              width.toInt,
              height.toInt,
              ansi.toBoolean,
              color.toBoolean,
              supershell.toBoolean
            )
          ).toOption
        case _ => None
      }
  }
  private[sbt] def startedByRemoteClient = props.isDefined

  /**
   * Creates an instance of [[Terminal]] that delegates most of its methods to an underlying
   * jline.Terminal2 instance. In the long run, sbt should upgrade to jline3, which has a
   * completely different terminal interface so whereever possible, we should avoid
   * directly referencing jline.Terminal. Wrapping jline Terminal in sbt terminal helps
   * with that goal.
   *
   * @param terminal the jline terminal to wrap
   * @return an sbt Terminal
   */
  private[this] def wrap(terminal: jline.Terminal): Terminal = {
    val term: jline.Terminal with jline.Terminal2 = new jline.Terminal with jline.Terminal2 {
      private[this] val hasConsole = System.console != null
      private[this] def alive = hasConsole && attached.get
      private[this] val term2: jline.Terminal2 = terminal match {
        case t: jline.Terminal2 => t
        case _                  => new DefaultTerminal2(terminal)
      }
      override def init(): Unit =
        if (alive)
          try terminal.init()
          catch {
            case _: InterruptedException | _: java.io.IOError =>
          }
      override def restore(): Unit =
        if (alive)
          try terminal.restore()
          catch {
            case _: InterruptedException | _: java.io.IOError =>
          }
      override def reset(): Unit =
        try terminal.reset()
        catch { case _: InterruptedException => }
      override def isSupported: Boolean = terminal.isSupported
      override def getWidth: Int = props.map(_.width).getOrElse(terminal.getWidth)
      override def getHeight: Int = props.map(_.height).getOrElse(terminal.getHeight)
      override def isAnsiSupported: Boolean = props.map(_.ansi).getOrElse(terminal.isAnsiSupported)
      override def wrapOutIfNeeded(out: OutputStream): OutputStream = terminal.wrapOutIfNeeded(out)
      override def wrapInIfNeeded(in: InputStream): InputStream = terminal.wrapInIfNeeded(in)
      override def hasWeirdWrap: Boolean = terminal.hasWeirdWrap
      override def isEchoEnabled: Boolean = terminal.isEchoEnabled

      override def setEchoEnabled(enabled: Boolean): Unit =
        if (alive) terminal.setEchoEnabled(enabled)
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
    new ConsoleTerminal(term, nonBlockingIn, originalOut)
  }

  private[sbt] def reset(): Unit = {
    jline.TerminalFactory.reset()
    console.close()
    consoleTerminalHolder.set(wrap(jline.TerminalFactory.get))
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
    new ConsoleReader(term.inputStream, term.outputStream, term.toJLine) {
      override def readLine(prompt: String, mask: Character): String =
        term.withRawInput(super.readLine(prompt, mask))
      override def readLine(prompt: String): String = term.withRawInput(super.readLine(prompt))
    }
  }

  def console: Terminal = consoleTerminalHolder.get match {
    case null => throw new IllegalStateException("Uninitialized terminal.")
    case term => term
  }

  private val capabilityMap =
    org.jline.utils.InfoCmp.Capability.values().map(c => c.toString -> c).toMap

  @deprecated("For compatibility only", "1.4.0")
  private[sbt] def deprecatedTeminal: jline.Terminal = console.toJLine
  private class ConsoleTerminal(
      val term: jline.Terminal with jline.Terminal2,
      in: WriteableInputStream,
      out: OutputStream
  ) extends TerminalImpl(in, out, originalErr, "console0") {
    private[util] lazy val system = JLine3.system
    override private[sbt] def getSizeImpl: (Int, Int) = {
      val size = system.getSize
      (size.getColumns, size.getRows)
    }
    private[this] def isCI = sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI")
    override def isAnsiSupported: Boolean = term.isAnsiSupported && !isCI
    override def isEchoEnabled: Boolean = system.echo()
    override def isSuccessEnabled: Boolean = true
    override def getBooleanCapability(capability: String, jline3: Boolean): Boolean =
      if (jline3) capabilityMap.get(capability).fold(false)(system.getBooleanCapability)
      else term.getBooleanCapability(capability)
    override def getNumericCapability(capability: String, jline3: Boolean): Integer =
      if (jline3) capabilityMap.get(capability).fold(null: Integer)(system.getNumericCapability)
      else term.getNumericCapability(capability)
    override def getStringCapability(capability: String, jline3: Boolean): String =
      if (jline3) capabilityMap.get(capability).fold(null: String)(system.getStringCapability)
      else term.getStringCapability(capability)
    override private[sbt] def restore(): Unit = term.restore()

    override private[sbt] def getAttributes: Map[String, String] =
      Try(JLine3.toMap(system.getAttributes)).getOrElse(Map.empty)
    override private[sbt] def setAttributes(attributes: Map[String, String]): Unit =
      system.setAttributes(JLine3.attributesFromMap(attributes))
    override private[sbt] def setSize(width: Int, height: Int): Unit =
      system.setSize(new org.jline.terminal.Size(width, height))

    override def withRawInput[T](f: => T): T = term.synchronized {
      try {
        term.init()
        term.setEchoEnabled(false)
        f
      } catch { case _: InterruptedIOException => throw new InterruptedException } finally {
        term.restore()
        term.setEchoEnabled(true)
      }
    }
    override def isColorEnabled: Boolean =
      props.map(_.color).getOrElse(ConsoleAppender.formatEnabledInEnv)

    override def isSupershellEnabled: Boolean =
      props
        .map(_.supershell)
        .getOrElse(System.getProperty("sbt.supershell") match {
          case null =>
            !(sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI")) && isColorEnabled
          case "true" => true
          case _      => false
        })
    override def close(): Unit = {
      try system.close()
      catch { case NonFatal(_) => }
      super.close()
    }
  }
  private[sbt] abstract class TerminalImpl private[sbt] (
      val in: WriteableInputStream,
      val out: OutputStream,
      override val errorStream: OutputStream,
      override private[sbt] val name: String
  ) extends Terminal {
    private[sbt] def getSizeImpl: (Int, Int)
    private[this] val sizeRefreshPeriod = 1.second
    private[this] val size =
      new AtomicReference[((Int, Int), Deadline)](((1, 1), Deadline.now - 1.day))
    private[this] def setSize() = Try(getSizeImpl).foreach(s => size.set((s, Deadline.now)))
    private[this] def getSize = size.get match {
      case (s, d) if (d + sizeRefreshPeriod).isOverdue =>
        setSize()
        size.get._1
      case (s, _) => s
    }
    override def getWidth: Int = getSize._1
    override def getHeight: Int = getSize._2
    private[this] val rawMode = new AtomicBoolean(false)
    private[this] val writeLock = new AnyRef
    def throwIfClosed[R](f: => R): R = if (isStopped.get) throw new ClosedChannelException else f
    override def getLastLine: Option[String] = progressState.currentLine
    override def getLines: Seq[String] = progressState.getLines

    private val combinedOutputStream = new OutputStream {
      override def write(b: Int): Unit = {
        Option(bootOutputStreamHolder.get).foreach(_.write(b))
        out.write(b)
      }
      override def write(b: Array[Byte]): Unit = {
        write(b, 0, b.length)
      }
      override def write(b: Array[Byte], offset: Int, len: Int): Unit = {
        Option(bootOutputStreamHolder.get).foreach(_.write(b, offset, len))
        out.write(b, offset, len)
      }
      override def flush(): Unit = {
        Option(bootOutputStreamHolder.get).foreach(_.flush())
        out.flush()
      }
    }

    override val outputStream = new OutputStream {
      override def write(b: Int): Unit = throwIfClosed {
        write(Array((b & 0xFF).toByte))
      }
      override def write(b: Array[Byte]): Unit = throwIfClosed {
        writeLock.synchronized(doWrite(b))
      }
      override def write(b: Array[Byte], offset: Int, length: Int): Unit = throwIfClosed {
        write(Arrays.copyOfRange(b, offset, offset + length))
      }
      override def flush(): Unit = combinedOutputStream.flush()
    }
    private def doWrite(bytes: Array[Byte]): Unit = withPrintStream { ps =>
      progressState.write(TerminalImpl.this, bytes, ps, hasProgress.get && !rawMode.get)
    }
    override private[sbt] val printStream: PrintStream = new LinePrintStream(outputStream)
    override def inputStream: InputStream = in

    private[sbt] def write(bytes: Int*): Unit = in.write(bytes: _*)
    private[this] val isStopped = new AtomicBoolean(false)

    override def getLineHeightAndWidth(line: String): (Int, Int) = getWidth match {
      case width if width > 0 =>
        val position = EscHelpers.cursorPosition(line)
        val count = (position + width - 1) / width
        (count, position - (math.max((count - 1), 0) * width))
      case _ => (0, 0)
    }

    private[sbt] def withRawOutput[R](f: => R): R = {
      rawMode.set(true)
      try f
      finally rawMode.set(false)
    }
    private[this] val rawPrintStream: PrintStream = new LinePrintStream(combinedOutputStream)
    override def withPrintStream[T](f: PrintStream => T): T =
      writeLock.synchronized(f(rawPrintStream))

    override def close(): Unit = if (isStopped.compareAndSet(false, true)) {
      in.close()
    }
  }
  private[sbt] val NullTerminal = new Terminal {
    override def close(): Unit = {}
    override def getBooleanCapability(capability: String, jline3: Boolean): Boolean = false
    override def getHeight: Int = 0
    override def getLastLine: Option[String] = None
    override def getLines: Seq[String] = Nil
    override def getLineHeightAndWidth(line: String): (Int, Int) = (0, 0)
    override def getNumericCapability(capability: String, jline3: Boolean): Integer = null
    override def getStringCapability(capability: String, jline3: Boolean): String = null
    override def getWidth: Int = 0
    override def inputStream: java.io.InputStream = () => {
      try this.synchronized(this.wait)
      catch { case _: InterruptedException => }
      -1
    }
    override def isAnsiSupported: Boolean = false
    override def isColorEnabled: Boolean = false
    override def isEchoEnabled: Boolean = false
    override def isSuccessEnabled: Boolean = false
    override def isSupershellEnabled: Boolean = false
    override def outputStream: java.io.OutputStream = _ => {}
    override def errorStream: java.io.OutputStream = _ => {}
    override private[sbt] def getAttributes: Map[String, String] = Map.empty
    override private[sbt] def setAttributes(attributes: Map[String, String]): Unit = {}
    override private[sbt] def setSize(width: Int, height: Int): Unit = {}
    override private[sbt] def name: String = "NullTerminal"
    override private[sbt] val printStream: java.io.PrintStream =
      new PrintStream(outputStream, false)
    override private[sbt] def withPrintStream[T](f: java.io.PrintStream => T): T = f(printStream)
    override private[sbt] def write(bytes: Int*): Unit = {}
    override private[sbt] def withRawOutput[R](f: => R): R = f
  }
}
