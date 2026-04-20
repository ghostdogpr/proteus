package proteus.diff

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import zio.test.*

import proteus.{Change, CompatMode, ProtoDiff}

/**
  * End-to-end tests driven by scenarios under `src/test/resources/scenarios/`.
  *
  * Each scenario is a directory with `old/` and `new/` subdirectories plus expected outputs:
  * `expected.txt` (text format), `expected.md` (markdown), and `expected.json` (JSON).
  * The test loads both proto trees, computes the diff, formats it for each format the
  * CLI supports, and asserts the output matches the corresponding fixture exactly.
  *
  * To add a new scenario, drop a folder under `diff/src/test/resources/scenarios/`.
  */
object IntegrationSpec extends ZIOSpecDefault {

  private val scenariosRoot: Path =
    Paths.get(getClass.getClassLoader.getResource("scenarios").toURI)

  private def readFixture(scenario: Path, name: String): String =
    new String(Files.readAllBytes(scenario.resolve(name)), StandardCharsets.UTF_8)

  private def computeChanges(scenario: Path): (List[Change], Boolean) = {
    val oldDir   = scenario.resolve("old")
    val newDir   = scenario.resolve("new")
    val oldFiles = ProtoFiles.load(oldDir).fold(err => throw new RuntimeException(err), identity)
    val newFiles = ProtoFiles.load(newDir).fold(err => throw new RuntimeException(err), identity)
    val byFile   = Files.isDirectory(oldDir) && Files.isDirectory(newDir) && (oldFiles.size > 1 || newFiles.size > 1)
    val changes  =
      if (byFile) ProtoDiff.diffFiles(oldFiles, newFiles)
      else {
        // Single-file scenarios: bypass diffFiles to avoid file-segment polluting the path
        val oldUnit = oldFiles.values.head
        val newUnit = newFiles.values.head
        ProtoDiff.diff(oldUnit, newUnit)
      }
    (changes, byFile)
  }

  def spec = suite("IntegrationSpec")(
    Files
      .list(scenariosRoot)
      .iterator()
      .asScala
      .filter(Files.isDirectory(_))
      .toList
      .sortBy(_.getFileName.toString)
      .flatMap { scenarioDir =>
        val name              = scenarioDir.getFileName.toString
        val (changes, byFile) = computeChanges(scenarioDir)
        List(
          test(s"scenario: $name (text)") {
            assertTrue(Report.format(changes, CompatMode.Strictest, byFile) == readFixture(scenarioDir, "expected.txt"))
          },
          test(s"scenario: $name (markdown)") {
            assertTrue(Report.formatMarkdown(changes, CompatMode.Strictest, byFile) == readFixture(scenarioDir, "expected.md"))
          },
          test(s"scenario: $name (json)") {
            assertTrue(Report.formatJson(changes, CompatMode.Strictest) == readFixture(scenarioDir, "expected.json"))
          }
        )
      }*
  )
}
