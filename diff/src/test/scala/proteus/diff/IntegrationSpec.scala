package proteus.diff

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import zio.test.*

import proteus.{CompatMode, ProtoDiff}

/**
  * End-to-end tests driven by scenarios under `src/test/resources/scenarios/`.
  *
  * Each scenario is a directory with an `old/` subdirectory, a `new/` subdirectory, and an
  * `expected.txt` file. The test loads both proto trees, computes the diff, formats it as the
  * CLI would, and asserts the output matches `expected.txt` exactly.
  *
  * To add a new scenario, drop a folder under `cli/src/test/resources/scenarios/`.
  */
object IntegrationSpec extends ZIOSpecDefault {

  private val scenariosRoot: Path =
    Paths.get(getClass.getClassLoader.getResource("scenarios").toURI)

  private def readExpected(scenario: Path): String =
    new String(Files.readAllBytes(scenario.resolve("expected.txt")), StandardCharsets.UTF_8)

  private def runScenario(scenario: Path): String = {
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
    Report.format(changes, CompatMode.Strictest, byFile)
  }

  def spec = suite("IntegrationSpec")(
    Files
      .list(scenariosRoot)
      .iterator()
      .asScala
      .filter(Files.isDirectory(_))
      .toList
      .sortBy(_.getFileName.toString)
      .map { scenarioDir =>
        val name = scenarioDir.getFileName.toString
        test(s"scenario: $name") {
          val expected = readExpected(scenarioDir)
          val actual   = runScenario(scenarioDir)
          assertTrue(actual == expected)
        }
      }*
  )
}
