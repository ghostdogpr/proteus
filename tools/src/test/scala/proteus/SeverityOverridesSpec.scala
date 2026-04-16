package proteus

import zio.test.*

import proteus.Change.*

object SeverityOverridesSpec extends ZIOSpecDefault {

  def spec = suite("SeverityOverridesSpec")(
    suite("parse")(
      test("parses valid overrides") {
        val result = SeverityOverrides.parse(List("wire.FieldRemoved=info", "source.FieldRenamed=warning"))
        assertTrue(
          result == Right(
            SeverityOverrides(
              wire = Map("FieldRemoved" -> Severity.Info),
              source = Map("FieldRenamed" -> Severity.Warning)
            )
          )
        )
      },
      test("empty list") {
        assertTrue(SeverityOverrides.parse(Nil) == Right(SeverityOverrides.empty))
      },
      test("rejects unknown change type") {
        assertTrue(SeverityOverrides.parse(List("wire.NotARealChange=error")).isLeft)
      },
      test("rejects invalid severity") {
        assertTrue(SeverityOverrides.parse(List("wire.FieldRemoved=fatal")).isLeft)
      },
      test("rejects invalid mode") {
        assertTrue(SeverityOverrides.parse(List("strictest.FieldRemoved=info")).isLeft)
      },
      test("rejects missing equals") {
        assertTrue(SeverityOverrides.parse(List("wire.FieldRemoved info")).isLeft)
      }
    ),
    suite("severity overrides")(
      test("override wire severity") {
        val overrides = SeverityOverrides(wire = Map("FieldRemoved" -> Severity.Info))
        val change    = FieldRemoved(List("Foo"), "bar", 1, numberReserved = false)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Wire, overrides) == Severity.Info
        )
      },
      test("override source severity") {
        val overrides = SeverityOverrides(source = Map("FieldRenamed" -> Severity.Info))
        val change    = FieldRenamed(List("Foo"), 1, "old", "new")
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Source) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Source, overrides) == Severity.Info
        )
      },
      test("strictest uses overridden values") {
        val overrides = SeverityOverrides(
          wire = Map("FieldTypeChanged" -> Severity.Warning),
          source = Map("FieldTypeChanged" -> Severity.Info)
        )
        val change    = FieldTypeChanged(List("Foo"), "bar", 1, ProtoIR.Type.String, ProtoIR.Type.Int32)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Strictest) == Severity.Error,
          ProtoDiff.severity(change, CompatMode.Strictest, overrides) == Severity.Warning
        )
      },
      test("no override falls back to default") {
        val overrides = SeverityOverrides(wire = Map("FieldRemoved" -> Severity.Info))
        val change    = FieldAdded(List("Foo"), "bar", 1)
        assertTrue(
          ProtoDiff.severity(change, CompatMode.Wire, overrides) == Severity.Info
        )
      }
    )
  )
}
