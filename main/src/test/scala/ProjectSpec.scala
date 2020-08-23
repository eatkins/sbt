/*
 * sbt
 * Copyright 2011 - 2018, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * Licensed under Apache License 2.0 (see LICENSE)
 */

package sbt

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

class ProjectSpec extends Specification {
  def is: SpecStructure = s2"""

  This is a specification to check utility methods on the Project object

  Project should
    normalize projectIDs if they are empty  ${normalizeEmptyFileName}

  """

  def emptyFilename = ""

  def normalizeEmptyFileName: MatchResult[Either[String, String]] =
    Project.normalizeProjectID(emptyFilename) must equalTo(Right("root"))
}
