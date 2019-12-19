/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal
package client

import java.net.{ Socket, SocketTimeoutException }
import java.nio.channels.ClosedChannelException
import java.util.concurrent.atomic.AtomicBoolean

import sbt.protocol._
import sbt.internal.protocol._

import scala.annotation.tailrec

abstract class ServerConnection(connection: Socket) {

  private val running = new AtomicBoolean(true)
  private val retByte: Byte = '\r'.toByte
  private val delimiter: Byte = '\n'.toByte

  private val out = connection.getOutputStream

  val thread = new Thread(s"sbt-serverconnection-${connection.getPort}") {
    override def run(): Unit = {
      try {
        val in = connection.getInputStream
        connection.setSoTimeout(5000)
        def readFrame: Array[Byte] = {
          // 'Content-Length: ' has 16 characters
          (0 until 16).foreach(_ => in.read())
          var break = false
          // the content length should be less than 16 digits
          val numberBuffer = new Array[Byte](16)
          var index = 0
          do {
            in.read match {
              case -1 => throw new ClosedChannelException
              case 13 =>
              case 10 => break = true
              case i =>
                numberBuffer(index) = i.toByte
                index += 1
            }
          } while (!break)
          val len = new String(numberBuffer, 0, index).toInt
          @tailrec def drainLine(): Unit = in.read match {
            case -1 => throw new ClosedChannelException
            case 10 =>
            case _  => drainLine()
          }
          drainLine()
          drainLine()
          val readBuffer = new Array[Byte](len)
          @tailrec def fillBuffer(offset: Int): Unit =
            in.read(readBuffer, offset, len - offset) match {
              case -1        => throw new ClosedChannelException
              case bytesRead => if (offset + bytesRead < len) fillBuffer(offset + bytesRead)
            }
          fillBuffer(0)
          readBuffer
        }

        while (running.get) {
          try {
            val frame = readFrame
            Serialization
              .deserializeJsonMessage(frame)
              .fold(
                { errorDesc =>
                  val s = frame.mkString("") // new String(: Array[Byte], "UTF-8")
                  println(s"Got invalid chunk from server: $s \n" + errorDesc)
                },
                _ match {
                  case msg: JsonRpcRequestMessage      => onRequest(msg)
                  case msg: JsonRpcResponseMessage     => onResponse(msg)
                  case msg: JsonRpcNotificationMessage => onNotification(msg)
                }
              )
          } catch {
            case _: SocketTimeoutException => // its ok
          }
        }
      } finally {
        shutdown()
      }
    }
  }
  thread.start()

  def sendString(message: String): Unit = {
    val a = message.getBytes("UTF-8")
    writeLine(s"""Content-Length: ${a.length + 2}""".getBytes("UTF-8"))
    writeLine(Array())
    writeLine(a)
  }

  def writeLine(a: Array[Byte]): Unit = {
    def writeEndLine(): Unit = {
      out.write(retByte.toInt)
      out.write(delimiter.toInt)
      out.flush
    }
    if (a.nonEmpty) {
      out.write(a)
    }
    writeEndLine
  }

  def onRequest(msg: JsonRpcRequestMessage): Unit
  def onResponse(msg: JsonRpcResponseMessage): Unit
  def onNotification(msg: JsonRpcNotificationMessage): Unit

  def onShutdown(): Unit

  def shutdown(): Unit = {
    println("Shutting down client connection")
    running.set(false)
    out.close()
    onShutdown
  }

}
