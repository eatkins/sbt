/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.std

import sbt.internal.util.complete
import sbt.internal.util.complete.DefaultParsers
import sbt.{ Def, InputTask, Task }
import sbt.{ InputKey, SettingKey, TaskKey }

/*object UseTask
{
		import Def._

	val set = setting { 23 }
	val plain = PlainTaskMacro task { 19 }

	val x = task { set.value }
	val y = task { true }
	val z = task { if(y.value) x.value else plain.value }
	val a = taskDyn {
		if(y.value) z else x
	}
}*/
object Assign {
  import java.io.File

  import Def.{ Initialize, inputKey, macroValueT, parserToInput, settingKey, taskKey }
  //	import UseTask.{x,y,z,a,set,plain}

  val ak: TaskKey[Int] = taskKey[Int]("a")
  val bk: TaskKey[Seq[Int]] = taskKey[Seq[Int]]("b")
  val ck: SettingKey[File] = settingKey[File]("c")
  val sk: TaskKey[Set[_]] = taskKey[Set[_]]("s")

  val ik: InputKey[Int] = inputKey[Int]("i")
  val isk: InputKey[String] = inputKey[String]("is")
  val mk: SettingKey[Int] = settingKey[Int]("m")
  val tk: TaskKey[Int] = taskKey[Int]("t")
  val name: SettingKey[String] = settingKey[String]("name")
  val dummyt: TaskKey[complete.Parser[String]] = taskKey[complete.Parser[String]]("dummyt")
  val dummys: SettingKey[complete.Parser[String]] = settingKey[complete.Parser[String]]("dummys")
  val dummy3: SettingKey[complete.Parser[(String, Int)]] =
    settingKey[complete.Parser[(String, Int)]]("dummy3")
  val tsk: complete.Parser[Task[String]] = DefaultParsers.failure("ignored")
  val itsk: Initialize[InputTask[Int]] = inputKey[Int]("ignored")
  val seqSetting: SettingKey[Seq[String]] = settingKey[Seq[String]]("seqSetting")
  val listSetting: SettingKey[List[String]] = settingKey[List[String]]("listSetting")

  /*	def azy = sk.value

	def azy2 = appmacro.Debug.checkWild(Def.task{ sk.value.size })

	val settings = Seq(
		ak += z.value + (if(y.value) set.value else plain.value),
		ck := new File(ck.value, "asdf"),
		ak := sk.value.size,
		bk ++= Seq(z.value)
	)*/

  val zz: Initialize[Task[Int]] = Def.task {
    mk.value + tk.value + mk.value + tk.value + mk.value + tk.value + mk.value + tk.value + mk.value + tk.value + mk.value + tk.value
  }

  import DefaultParsers._
  val p: Initialize[complete.Parser[String]] = Def.setting { name.value ~> Space ~> ID }
  val is: Seq[Def.Setting[_]] = Seq(
    mk := 3,
    name := "asdf",
    tk := (math.random * 1000).toInt,
    isk := dummys.value.parsed // should not compile: cannot use a task to define the parser
    //		ik := { if( tsk.parsed.value == "blue") tk.value else mk.value }
  )

  val it1: Initialize[InputTask[Task[String]]] = Def.inputTask {
    tsk.parsed //"as" //dummy.value.parsed
  }
  val it2: Initialize[InputTask[String]] = Def.inputTask {
    "lit"
  }

  val it3: Initialize[InputTask[String]] = Def.inputTask[String] {
    tsk.parsed.value + itsk.parsed.value.toString + isk.evaluated
  }
  // should not compile: cannot use a task to define the parser
  /*	val it4 = Def.inputTask {
		dummyt.value.parsed
	}*/
  // should compile: can use a setting to define the parser
  val it5: Initialize[InputTask[String]] = Def.inputTask {
    dummys.parsed
  }
  val it6: Initialize[InputTask[Int]] = Def.inputTaskDyn {
    val d3 = dummy3.parsed
    val i = d3._2
    Def.task { tk.value + i }
  }

  val it7: Initialize[InputTask[Task[String]]] = Def.inputTask {
    it5.parsed
  }

  def bool: Initialize[Boolean] = Def.setting { true }
  def enabledOnly[T](key: Initialize[T]): Initialize[Seq[T]] = Def.setting {
    val keys: Seq[T] = forallIn(key).value
    val enabled: Seq[Boolean] = forallIn(bool).value
    (keys zip enabled) collect { case (a, true) => a }
  }
  def forallIn[T](key: Initialize[T]): Initialize[Seq[T]] = Def.setting {
    key.value :: Nil
  }

  // Test that Append.Sequence instances for Seq/List work and don't mess up with each other
  seqSetting := Seq("test1")
  seqSetting ++= Seq("test2")
  seqSetting ++= List("test3")
  seqSetting += "test4"

  listSetting := List("test1")
  listSetting ++= List("test2")
  listSetting += "test4"
}
