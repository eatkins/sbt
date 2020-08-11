/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

import java.util.concurrent.atomic.AtomicInteger
import sbt.internal.util.{
  AutoJson,
  JsonBuilder,
  JsonUnbuilder,
  JsonDeserializationError,
  Length,
  ForEach
}
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import sjsonnew.JsonFormat
import sjsonnew.support.scalajson.unsafe.Converter

object AutoJsonHelpers {
  def roundTrip[A](a: A)(implicit aj: AutoJson[A]) = {
    val builder = new ArrayJsonBuilder
    aj.write(a, builder)
    aj.read(new ArrayJsonUnbuilder(builder.build()))
  }
  def roundTripSjson[A](a: A)(implicit aj: AutoJson[A]): Try[A] = {
    implicit val jf: JsonFormat[A] = AutoJson.jsonFormat(aj)
    val json = Converter.toJson(a).get
    Converter.fromJson[A](json)
  }
  class ArrayJsonBuilder extends JsonBuilder {
    val result = new ArrayBuffer[Any]
    def writeBoolean(b: Boolean): Unit = result += b
    def writeDouble(d: Double): Unit = result += d
    def writeInt(i: Int): Unit = result += i
    def writeLong(l: Long): Unit = result += l
    def writeString(s: String): Unit = result += s
    def writeSeq[M[_]: ForEach: Length, T](t: M[T])(f: (T, JsonBuilder) => Unit): Unit = {
      writeInt(Length(t))
      ForEach(t)(f(_, this))
    }
    def build(): Array[Any] = result.toArray
  }
  class ArrayJsonUnbuilder(array: Array[Any]) extends JsonUnbuilder {
    val position = new AtomicInteger(0)
    def readBoolean: Boolean = {
      array(position.getAndIncrement) match {
        case b: Boolean => b
        case _          => throw JsonDeserializationError
      }
    }
    def readDouble: Double = {
      array(position.getAndIncrement) match {
        case d: Double => d
        case _         => throw JsonDeserializationError
      }
    }
    def readInt: Int = {
      array(position.getAndIncrement) match {
        case i: Int => i
        case _      => throw JsonDeserializationError
      }
    }
    def readLong: Long = {
      array(position.getAndIncrement) match {
        case l: Long => l
        case _       => throw JsonDeserializationError
      }
    }
    def readString: String = {
      array(position.getAndIncrement) match {
        case s: String => s
        case _         => throw JsonDeserializationError
      }
    }
    def readSeq[T](f: (JsonUnbuilder, Int) => T): T = {
      val len = readInt
      f(this, len)
    }
  }
}
