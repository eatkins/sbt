/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt
package internal.util.codec

import sbt.internal.util.SuccessEvent
import sbt.util.ShowLines

trait SuccessEventShowLines {
  implicit val sbtSuccessEventShowLines: ShowLines[SuccessEvent] =
    ShowLines[SuccessEvent]((e: SuccessEvent) => {
      Vector(e.message)
    })
}

object SuccessEventShowLines extends SuccessEventShowLines
