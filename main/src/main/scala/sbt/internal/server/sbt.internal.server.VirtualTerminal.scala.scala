/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

package internal
package server

import java.util.concurrent.{ ArrayBlockingQueue, ConcurrentHashMap }
import java.util.UUID
import sbt.protocol.Serialization.{
  attach,
  systemIn,
  terminalCapabilities,
  terminalPropertiesQuery,
}
import sjsonnew.support.scalajson.unsafe.Converter
import sbt.protocol.{ Attach, TerminalCapabilitiesResponse, TerminalPropertiesResponse }

object VirtualTerminal {
  private[this] val pendingTerminalProperties =
    new ConcurrentHashMap[(String, String), ArrayBlockingQueue[TerminalPropertiesResponse]]()
  private[this] val pendingTerminalCapabilities =
    new ConcurrentHashMap[(String, String), ArrayBlockingQueue[TerminalCapabilitiesResponse]]
  private[sbt] def sendTerminalPropertiesQuery(
      channelName: String,
      jsonRpcNotify: (String, String) => Unit
  ): ArrayBlockingQueue[TerminalPropertiesResponse] = {
    val id = UUID.randomUUID.toString
    val queue = new ArrayBlockingQueue[TerminalPropertiesResponse](1)
    pendingTerminalProperties.put((channelName, id), queue)
    jsonRpcNotify(terminalPropertiesQuery, id)
    queue
  }
  private[sbt] def sendTerminalCapabilitiesQuery(
      channelName: String,
      jsonRpcNotify: (String, String) => Unit
  ): ArrayBlockingQueue[TerminalCapabilitiesResponse] = {
    val id = UUID.randomUUID.toString
    val queue = new ArrayBlockingQueue[TerminalCapabilitiesResponse](1)
    pendingTerminalCapabilities.put((channelName, id), queue)
    jsonRpcNotify(terminalCapabilities, id)
    queue
  }
  private[sbt] def cancelRequests(name: String): Unit = {
    pendingTerminalCapabilities.forEach {
      case (k @ (`name`, _), q) =>
        pendingTerminalCapabilities.remove(k)
        q.put(TerminalCapabilitiesResponse("", None, None, None))
      case _ =>
    }
    pendingTerminalProperties.forEach {
      case (k @ (`name`, _), q) =>
        pendingTerminalProperties.remove(k)
        q.put(TerminalPropertiesResponse(0, 0, false, false, false, false))
      case _ =>
    }
  }
  val requestHandler: ServerHandler = ServerHandler { callback =>
    ServerIntent.request {
      case r if r.method == attach =>
        import sbt.protocol.codec.JsonProtocol.AttachFormat
        val isInteractive = r.params
          .flatMap(Converter.fromJson[Attach](_).toOption.map(_.interactive))
          .exists(identity)
        System.err.println(s"attach!")
        StandardMain.exchange.channelForName(callback.name) match {
          case Some(nc: NetworkChannel) => nc.setInteractive(r.id, isInteractive)
          case _                        =>
        }
    }
  }
  val responseHandler: ServerHandler = ServerHandler { callback =>
    ServerIntent.response {
      case r if pendingTerminalProperties.get((callback.name, r.id)) != null =>
        import sbt.protocol.codec.JsonProtocol._
        val response =
          r.result.flatMap(Converter.fromJson[TerminalPropertiesResponse](_).toOption)
        pendingTerminalProperties.remove((callback.name, r.id)) match {
          case null   =>
          case buffer => response.foreach(buffer.put)
        }
      case r if pendingTerminalCapabilities.get((callback.name, r.id)) != null =>
        System.err.println(r)
        import sbt.protocol.codec.JsonProtocol._
        val response =
          r.result.flatMap(
            Converter.fromJson[TerminalCapabilitiesResponse](_).toOption
          )
        pendingTerminalCapabilities.remove((callback.name, r.id)) match {
          case null =>
          case buffer =>
            buffer.put(
              response.getOrElse(TerminalCapabilitiesResponse("", None, None, None))
            )
        }
    }
  }
  val notificationHandler: ServerHandler = ServerHandler { callback =>
    ServerIntent.notify {
      case n if n.method == systemIn =>
        import sjsonnew.BasicJsonProtocol._
        n.params.flatMap(Converter.fromJson[Byte](_).toOption).foreach { byte =>
          StandardMain.exchange.channelForName(callback.name) match {
            case Some(nc: NetworkChannel) => nc.write(byte)
            case _                        =>
          }
        }
    }
  }
}
