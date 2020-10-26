/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package testpkg

import java.io.{ InputStream, OutputStream, PrintStream }
import java.util.concurrent.{ LinkedBlockingQueue, TimeUnit, TimeoutException }
import sbt.internal.client.NetworkClient
import sbt.internal.util.Util
import sbt.io.IO
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{ Success, Try }
import java.io.File

object ClientTest extends AbstractServerTest {
  override val testDirectory: String = "client"
  object NullInputStream extends InputStream {
    override def read(): Int = {
      try this.synchronized(this.wait)
      catch { case _: InterruptedException => }
      -1
    }
  }
  val NullPrintStream = new PrintStream(_ => {}, false)
  class CachingPrintStream extends { val cos = new CachingOutputStream }
  with PrintStream(cos, true) {
    def lines = cos.lines
  }
  class CachingOutputStream extends OutputStream {
    private val byteBuffer = new mutable.ArrayBuffer[Byte]
    override def write(i: Int) = Util.ignoreResult(byteBuffer += i.toByte)
    def lines = new String(byteBuffer.toArray, "UTF-8").linesIterator.toSeq
  }
  class FixedInputStream(keys: Char*) extends InputStream {
    var i = 0
    override def read(): Int = {
      if (i < keys.length) {
        val res = keys(i).toInt
        i += 1
        res
      } else -1
    }
  }
  class WriteableInputStream extends InputStream {
    val queue = new LinkedBlockingQueue[Int]
    def write(i: Int): Unit = queue.put(i)
    override def read(): Int = queue.take
  }
  private[this] def background[R](f: => R): R = {
    val result = new LinkedBlockingQueue[R]
    val thread = new Thread("client-bg-thread") {
      setDaemon(true)
      start()
      override def run(): Unit = result.put(f)
    }
    result.poll(1, TimeUnit.MINUTES) match {
      case null =>
        thread.interrupt()
        thread.join(5000)
        throw new TimeoutException
      case r => r
    }
  }
  private[this] def runInBackground(f: () => Unit, threadName: String): Thread = {
    new Thread(threadName) {
      override def run(): Unit = f()
      setDaemon(true)
      start()
    }
  }
  private def client(inputStream: InputStream, args: String*): Int = {
    background(
      NetworkClient.client(
        testPath.toFile,
        args.toArray,
        inputStream,
        NullPrintStream,
        NullPrintStream,
        false
      )
    )
  }
  private def client(args: String*): Int = client(NullInputStream, args: _*)
  // This ensures that the completion command will send a tab that triggers
  // sbt to call definedTestNames or discoveredMainClasses if there hasn't
  // been a necessary compilation
  def tabs = new FixedInputStream('\t', '\t')
  private def complete(completionString: String): Seq[String] = {
    val cps = new CachingPrintStream
    background(
      NetworkClient.complete(
        testPath.toFile,
        Array(s"--completions=sbtn $completionString"),
        false,
        tabs,
        cps
      )
    )
    cps.lines
  }
  test("exit success") { c =>
    assert(client("willSucceed") == 0)
  }
  test("exit failure") { _ =>
    assert(client("willFail") == 1)
  }
  test("two commands") { _ =>
    assert(client("compile;willSucceed") == 0)
  }
  test("two commands with failing second") { _ =>
    assert(client("compile;willFail") == 1)
  }
  test("two commands with leading failure") { _ =>
    assert(client("willFail;willSucceed") == 1)
  }
  test("three commands") { _ =>
    assert(client("compile;clean;willSucceed") == 0)
  }
  test("three commands with middle failure") { _ =>
    assert(client("compile;willFail;willSucceed") == 1)
  }
  test("compi completions") { _ =>
    val expected = Vector(
      "compile",
      "compile:",
      "compileAnalysisFile",
      "compileAnalysisFilename",
      "compileAnalysisTargetRoot",
      "compileEarly",
      "compileIncSetup",
      "compileIncremental",
      "compileJava",
      "compileOutputs",
      "compileProgress",
      "compileScalaBackend",
      "compileSplit",
      "compilers",
    )

    assert(complete("compi") == expected)
  }
  test("testOnly completions") { _ =>
    val testOnlyExpected = Vector(
      "testOnly",
      "testOnly/",
      "testOnly::",
      "testOnly;",
    )
    assert(complete("testOnly") == testOnlyExpected)

    val testOnlyOptionsExpected = Vector("--", ";", "test.pkg.FooSpec")
    assert(complete("testOnly ") == testOnlyOptionsExpected)
  }
  test("quote with semi") { _ =>
    assert(complete("\"compile; fooB") == Vector("compile; fooBar"))
  }
  test("multiple clients can simultaneously call run") { _ =>
    // In this test, we start two clients on background thread which both
    // call the project main class. The main class takes a filename as
    // argument. It first writes the string "start" to that file and then
    // blocks on System.in until it receives a byte. Then it writes "finish"
    // to the file and exits. We wait until the first client has successfully
    // written to the file before starting up the second client but we write
    // to the input stream of the second client first so that it is actually
    // able to exit before the first client.
    def blockUntilStart(file: File): Unit = {
      val limit = 30.seconds.fromNow
      while (Try(IO.read(file)) != Success("start") && !limit.isOverdue) {
        Thread.sleep(20)
      }
      if (limit.isOverdue) throw new TimeoutException
    }
    IO.withTemporaryDirectory { dir =>
      val fileA = new File(dir, "a")
      val fileB = new File(dir, "b")
      val aInputStream = new WriteableInputStream
      val bInputStream = new WriteableInputStream
      val aResult = new LinkedBlockingQueue[Int]
      val bResult = new LinkedBlockingQueue[Int]
      val aClientThread =
        runInBackground(() => aResult.put(client(aInputStream, s"run $fileA")), "athread")
      blockUntilStart(fileA)
      val bClientThread =
        runInBackground(() => bResult.put(client(bInputStream, s"run $fileB")), "bthread")

      blockUntilStart(fileB)
      bInputStream.write(10)
      assert(bResult.poll(10, TimeUnit.SECONDS) == 0)
      assert(IO.read(fileB) == "finish")

      assert(IO.read(fileA) == "start")
      aInputStream.write(10)
      assert(aResult.poll(10, TimeUnit.SECONDS) == 0)
      assert(IO.read(fileA) == "finish")

      assert(!bClientThread.isAlive)
      assert(!aClientThread.isAlive)
    }
  }
}
