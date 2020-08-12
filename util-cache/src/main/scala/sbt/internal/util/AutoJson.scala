/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal.util

import sjsonnew.JsonFormat
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox
import java.io.File
import java.net.URL

object JsonDeserializationError extends java.lang.Throwable("", null, true, true)
object JsonSerializationError extends java.lang.Throwable("", null, true, true)
trait JsonBuilder extends Any {
  def writeBoolean(b: Boolean): Unit
  def writeDouble(d: Double): Unit
  def writeInt(i: Int): Unit
  def writeLong(l: Long): Unit
  def writeString(s: String): Unit
  def writeSeq[M[_]: ForEach: Length, T](t: M[T])(f: (T, JsonBuilder) => Unit): Unit
}
trait JsonUnbuilder extends Any {
  def readBoolean: Boolean
  def readDouble: Double
  def readInt: Int
  def readLong: Long
  def readString: String
  def readSeq[T](f: (JsonUnbuilder, Int) => T): T
}
trait AutoJson[T] {
  def read(unbuilder: JsonUnbuilder): T
  def write(obj: T, builder: JsonBuilder): Unit
}
trait ForEach[M[_]] {
  def apply[T](m: M[T])(f: T => Unit): Unit
}
object ForEach {
  def apply[T, M[_]](m: M[T])(f: T => Unit)(implicit foreach: ForEach[M]): Unit = foreach(m)(f)
  private def traversable[M[_] <: Traversable[_]]: ForEach[M] = new ForEach[M] {
    override def apply[T](m: M[T])(f: T => Unit): Unit = m.asInstanceOf[Traversable[T]].foreach(f)
  }
  implicit val set: ForEach[Set] = traversable[Set]
  implicit val seq: ForEach[collection.immutable.Seq] = traversable[collection.immutable.Seq]
  implicit val vector: ForEach[Vector] = traversable[Vector]
  implicit val list: ForEach[List] = traversable[List]
  implicit object array extends ForEach[Array] {
    override def apply[T](m: Array[T])(f: T => Unit): Unit = m.foreach(f)
  }
  implicit object option extends ForEach[Option] {
    override def apply[T](m: Option[T])(f: T => Unit): Unit = m.foreach(f)
  }
}
trait Length[M[_]] {
  def length(m: M[_]): Int
}
object Length {
  def apply[M[_]](m: M[_])(implicit length: Length[M]): Int = length.length(m)
  implicit val seq: Length[collection.immutable.Seq] = _.size
  implicit val set: Length[Set] = _.size
  implicit val vector: Length[Vector] = _.size
  implicit val list: Length[List] = _.size
  implicit val array: Length[Array] = _.size
  implicit object option extends Length[Option] {
    override def length(m: Option[_]): Int = if (m.isDefined) 1 else 0
  }
}
trait CollectionBuilder[T, M[_]] {
  def newBuilder(): CollectionBuilder[T, M]
  def add(t: T): Unit
  def build(): M[T]
}
object CollectionBuilder {
  implicit def setBuilder[T]: CollectionBuilder[T, Set] = {
    class SetCollectionBuilder[R] extends CollectionBuilder[R, Set] {
      private val result = new mutable.HashSet[R]
      def newBuilder(): CollectionBuilder[R, Set] = new SetCollectionBuilder[R]
      def add(t: R) = { result.add(t); () }
      def build(): Set[R] = {
        val res = result.toSet
        result.clear()
        res
      }
      override def toString: String = s"SetBuilder($result)"
    }
    new SetCollectionBuilder[T]
  }
  implicit def listBuilder[T]: CollectionBuilder[T, List] = {
    class ListCollectionBuilder[R] extends CollectionBuilder[R, List] {
      private val result = new mutable.ListBuffer[R]
      def newBuilder(): CollectionBuilder[R, List] = new ListCollectionBuilder[R]
      def add(t: R) = { result += t; () }
      def build(): List[R] = {
        val res = result.toList
        result.clear()
        res
      }
      override def toString: String = s"ListBuilder($result)"
    }
    new ListCollectionBuilder[T]
  }
  implicit def vectorBuilder[T]: CollectionBuilder[T, Vector] = {
    class VectorCollectionBuilder[R] extends CollectionBuilder[R, Vector] {
      private val result = new VectorBuilder[R]
      def newBuilder(): CollectionBuilder[R, Vector] = new VectorCollectionBuilder[R]
      def add(t: R) = { result += t; () }
      def build(): Vector[R] = {
        val res = result.result()
        result.clear()
        res
      }
      override def toString: String = s"VectorBuilder($result)"
    }
    new VectorCollectionBuilder[T]
  }
  implicit def seqBuilder[T]: CollectionBuilder[T, collection.immutable.Seq] =
    vectorBuilder[T].asInstanceOf[CollectionBuilder[T, collection.immutable.Seq]]
  implicit def arrayBuilder[T: ClassTag]: CollectionBuilder[T, Array] = {
    class ArrayCollectionBuilder[R: ClassTag] extends CollectionBuilder[R, Array] {
      private val result = new mutable.ArrayBuffer[R]
      def newBuilder(): CollectionBuilder[R, Array] = new ArrayCollectionBuilder[R]
      def add(t: R) = result += t
      def build(): Array[R] = {
        val res = result.toArray
        result.clear()
        res
      }
      override def toString: String = s"ArrayBuilder($result)"
    }
    new ArrayCollectionBuilder[T]
  }
}
object AutoJson extends LowPriorityAutoJson {
  class SjsonBuilder[J](val builder: sjsonnew.Builder[J]) extends AnyVal with JsonBuilder {
    def writeBoolean(b: Boolean): Unit = builder.writeBoolean(b)
    def writeDouble(d: Double): Unit = builder.writeDouble(d)
    def writeInt(i: Int): Unit = builder.writeInt(i)
    def writeLong(l: Long): Unit = builder.writeLong(l)
    def writeString(s: String): Unit = builder.writeString(s)
    def writeSeq[M[_]: ForEach: Length, T](t: M[T])(f: (T, JsonBuilder) => Unit): Unit = {
      val len = Length(t)
      val sbuilder = new SjsonBuilder(builder)
      sbuilder.writeInt(len)
      ForEach(t)(f(_, sbuilder))
    }
  }
  class SjsonUnbuilder[J](val unbuilder: sjsonnew.Unbuilder[J]) extends JsonUnbuilder {
    def readBoolean: Boolean = unbuilder.readBoolean(unbuilder.nextElement)
    def readDouble: Double = unbuilder.readDouble(unbuilder.nextElement)
    def readInt: Int = unbuilder.readInt(unbuilder.nextElement)
    def readLong: Long = unbuilder.readLong(unbuilder.nextElement)
    def readString: String = unbuilder.readString(unbuilder.nextElement)
    def readSeq[T](f: (JsonUnbuilder, Int) => T): T = {
      val size = unbuilder.readInt(unbuilder.nextElement)
      val res = f(this, size)
      res
    }
  }
  implicit def jsonFormat[T](implicit aj: AutoJson[T]): JsonFormat[T] = new JsonFormat[T] {
    def read[J](jsOpt: Option[J], sunbuilder: sjsonnew.Unbuilder[J]): T = jsOpt match {
      case Some(j) =>
        sunbuilder.beginArray(j)
        val res = aj.read(new SjsonUnbuilder(sunbuilder))
        sunbuilder.endArray()
        res
      case _ => throw JsonDeserializationError
    }
    def write[J](obj: T, sbuilder: sjsonnew.Builder[J]): Unit = {
      sbuilder.beginArray()
      aj.write(obj, new SjsonBuilder(sbuilder))
      sbuilder.endArray()
    }
  }
  trait IsoString[T] {
    def to(t: T): String
    def from(s: String): T
  }
  object IsoString {
    implicit object file extends IsoString[File] {
      def to(f: File): String = f.toString
      def from(s: String): File = new File(s)
    }
    implicit object url extends IsoString[URL] {
      def to(u: URL): String = u.toString
      def from(s: String): URL = new URL(s)
    }
  }
  implicit def isoStringAutoJson[T](implicit isoString: IsoString[T]): AutoJson[T] =
    new AutoJson[T] {
      override def read(unbuilder: JsonUnbuilder): T = isoString.from(unbuilder.readString)
      override def write(obj: T, builder: JsonBuilder): Unit =
        builder.writeString(isoString.to(obj))
    }
  implicit def optionAutoJson[T: ClassTag](implicit aj: AutoJson[T]): AutoJson[Option[T]] =
    new AutoJson[Option[T]] {
      def read(unbuilder: JsonUnbuilder): Option[T] = unbuilder.readSeq { (u, len) =>
        if (len == 0) None
        else Some(aj.read(u))
      }
      def write(obj: Option[T], builder: JsonBuilder): Unit =
        builder.writeSeq(obj)(aj.write(_, _))
    }
  implicit def seqAutoJson[M[_], T](
      implicit aj: AutoJson[T],
      builder: CollectionBuilder[T, M],
      t: ForEach[M],
      l: Length[M]
  ): AutoJson[M[T]] =
    new AutoJson[M[T]] {
      override def read(unbuilder: JsonUnbuilder): M[T] = unbuilder.readSeq { (u, len) =>
        var i = 0
        val b = builder.newBuilder
        while (i < len) {
          b.add(aj.read(u))
          i += 1
        }
        b.build()
      }
      override def write(a: M[T], builder: JsonBuilder): Unit = {
        builder.writeSeq(a)(aj.write(_, _))
      }
    }
  implicit def setAutoJson[T](implicit aj: AutoJson[T]): AutoJson[Set[T]] =
    seqAutoJson[Set, T]
  implicit def vectorAutoJson[T](implicit aj: AutoJson[T]): AutoJson[Vector[T]] =
    seqAutoJson[Vector, T]
  implicit def mapAutoJson[K, V](implicit aj: AutoJson[(K, V)]): AutoJson[Map[K, V]] =
    new AutoJson[Map[K, V]] {
      private val vectorBuilder = seqAutoJson[Vector, (K, V)]
      override def read(unbuilder: JsonUnbuilder): Map[K, V] = {
        Map(vectorBuilder.read(unbuilder): _*)
      }
      override def write(obj: Map[K, V], builder: JsonBuilder): Unit =
        vectorBuilder.write(obj.toVector, builder)
    }
  implicit object booleanAutoJson extends AutoJson[Boolean] {
    override def read(unbuilder: JsonUnbuilder): Boolean = unbuilder.readBoolean
    override def write(b: Boolean, builder: JsonBuilder): Unit = builder.writeBoolean(b)
  }
  implicit object doubleAutoJson extends AutoJson[Double] {
    override def read(unbuilder: JsonUnbuilder): Double = unbuilder.readDouble
    override def write(d: Double, builder: JsonBuilder): Unit = builder.writeDouble(d)
  }
  implicit object intAutoJson extends AutoJson[Int] {
    override def read(unbuilder: JsonUnbuilder): Int = unbuilder.readInt
    override def write(i: Int, builder: JsonBuilder): Unit = builder.writeInt(i)
  }
  implicit object longAutoJson extends AutoJson[Long] {
    override def read(unbuilder: JsonUnbuilder): Long = unbuilder.readLong
    override def write(l: Long, builder: JsonBuilder): Unit = builder.writeLong(l)
  }
  implicit object stringAutoJson extends AutoJson[String] {
    override def read(unbuilder: JsonUnbuilder): String = unbuilder.readString
    override def write(s: String, builder: JsonBuilder): Unit = builder.writeString(s)
  }
}

trait LowPriorityAutoJson extends AutoJsonTuple with LowLowPriorityAutoJson
trait LowLowPriorityAutoJson {
  implicit def macroDefault[T]: AutoJson[T] = macro AutoJsonMacro.impl[T]
}

private object AutoJsonMacro {
  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[AutoJson[T]] = {
    import c.universe._
    val tType = weakTypeOf[T]
    println(tType)
    val autoJson = weakTypeOf[AutoJson[_]]
    val jsonBuilder = weakTypeOf[JsonBuilder]
    val jsonUnbuilder = weakTypeOf[JsonUnbuilder]
    if (tType <:< weakTypeOf[Seq[_]])
      c.abort(c.enclosingPosition, s"Cannot generate AutoJson for $tType")
    val notFound = s"Couldn't find public constructor or static method to initailize $tType"
    val (mkInstance, decls) = tType.decls
      .collectFirst {
        case m: MethodSymbol if m.name == TermName("<init>") && m.isPrimaryConstructor =>
          val decls = m.typeSignature.paramLists.map(_.map { p =>
            val decl = tType.decl(p.name)
            if (!decl.isPublic)
              c.abort(c.enclosingPosition, s"Class $tType has private constructor parameters.")
            decl
          })
          if (!tType.typeSymbol.isAbstract && m.isPublic) (None, decls)
          else {
            tType.typeSymbol.companion.typeSignature.decls
              .find(_.typeSignatureIn(tType) =:= m.typeSignatureIn(tType)) match {
              case Some(method) => (Some(method), decls)
              case _            => c.abort(c.enclosingPosition, notFound)
            }
          }
      }
      .getOrElse(
        c.abort(
          c.enclosingPosition,
          s"Could not find constructor or static method to initialize $tType"
        )
      )
    val formats = decls
      .map(_.map { s =>
        val tpe = c.universe.appliedType(autoJson, s.typeSignatureIn(tType).finalResultType)
        val format = try c.inferImplicitValue(tpe, silent = false)
        catch { case t: Throwable => println(s"failed to infer $tType $tpe"); throw t }
        //println(s"$tType $tpe $format")
        (q"$format.write(obj.${s.name.toTermName}, builder)", q"$format.read(unbuilder)")
      })
    val newInstance = mkInstance match {
      case Some(mki) => q"$mki(...${formats.map(_.map(_._2))})"
      case None      => q"new $tType(...${formats.map(_.map(_._2))})"
    }
    val classname = TypeName(c.freshName("auto"))
    val tree = q"""
      class $classname extends sbt.internal.util.AutoJson[$tType] {
        def read(unbuilder: $jsonUnbuilder): $tType = $newInstance
        def write(obj: $tType, builder: $jsonBuilder): Unit = {
          ..${formats.flatten.map(_._1)}
        }
      }
      new $classname
    """
    c.Expr[AutoJson[T]](tree)
  }
}
