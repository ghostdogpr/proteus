package proteus.diff

import zio.test.*

import proteus.{Change, CompatMode, Severity, SeverityOverrides}

object MainSpec extends ZIOSpecDefault {

  private val warningChange = Change.FieldOptionalityChanged(List("M"), "f", 1, wasOptional = false)
  private val infoChange    = Change.FileAdded(List("a.proto"))

  def spec = suite("MainSpec")(
    test("--fail-on below the display threshold still trips the exit gate") {
      val (filtered, shouldFail) =
        Main.evaluate(List(warningChange), CompatMode.Wire, SeverityOverrides.empty, display = Severity.Error, failOn = Severity.Warning)
      assertTrue(filtered.isEmpty, shouldFail)
    },
    test("--fail-on does not trip on changes below the fail-on threshold") {
      val (_, shouldFail) =
        Main.evaluate(List(infoChange), CompatMode.Wire, SeverityOverrides.empty, display = Severity.Info, failOn = Severity.Warning)
      assertTrue(!shouldFail)
    }
  )
}
