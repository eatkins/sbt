/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt.internal

import sbt.BuildSyntax._
import sbt.KeyRanks._
import sbt.SettingKey
import sbt.internal.librarymanagement.{ CustomHttp => LMCustomHttp }

import okhttp3._

object CustomHttp {
  val okhttpClientBuilder: SettingKey[OkHttpClient.Builder] =
    settingKey[OkHttpClient.Builder]("Builder for the HTTP client.").withRank(CSetting)
  val okhttpClient: SettingKey[OkHttpClient] =
    settingKey[OkHttpClient]("HTTP client used for library management.").withRank(CSetting)

  def defaultHttpClientBuilder: OkHttpClient.Builder = {
    LMCustomHttp.defaultHttpClientBuilder
  }
}
