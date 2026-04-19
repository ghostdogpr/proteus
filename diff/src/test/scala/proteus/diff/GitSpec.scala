package proteus.diff

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.test.*

object GitSpec extends ZIOSpecDefault {

  private def gitAvailable: Boolean =
    try {
      val p = new ProcessBuilder("git", "--version").redirectErrorStream(true).start()
      p.waitFor() == 0
    } catch { case _: Throwable => false }

  private def run(dir: Path, cmd: String*): Int = {
    val p = new ProcessBuilder(cmd*).directory(dir.toFile).redirectErrorStream(true).start()
    p.getInputStream.readAllBytes(): Unit
    p.waitFor()
  }

  private def initRepo(dir: Path): Unit = {
    run(dir, "git", "init", "-q"): Unit
    run(dir, "git", "config", "user.email", "test@example.com"): Unit
    run(dir, "git", "config", "user.name", "Test"): Unit
    run(dir, "git", "config", "commit.gpgsign", "false"): Unit
  }

  private def commit(dir: Path, message: String): Unit = {
    run(dir, "git", "add", "-A"): Unit
    run(dir, "git", "commit", "-q", "-m", message): Unit
  }

  private def writeFile(path: Path, content: String): Unit = {
    Option(path.getParent).foreach(p => Files.createDirectories(p): Unit)
    Files.writeString(path, content, StandardCharsets.UTF_8): Unit
  }

  private def withRepo[A](f: Path => A): A = {
    val dir = Files.createTempDirectory("proteus-diff-git-test-")
    try {
      initRepo(dir)
      f(dir)
    } finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).iterator().asScala.foreach(p => Files.deleteIfExists(p): Unit)
  }

  def spec =
    if (!gitAvailable) suite("GitSpec")(test("skipped — git not on PATH")(assertCompletes))
    else
      suite("GitSpec")(
        test("extractProtos extracts proto files from a ref") {
          val (hasA, hasB, hasMd, isRight) = withRepo { dir =>
            writeFile(dir.resolve("a.proto"), """syntax = "proto3"; message Foo {}""")
            writeFile(dir.resolve("sub/b.proto"), """syntax = "proto3"; message Bar {}""")
            writeFile(dir.resolve("README.md"), "hi")
            commit(dir, "initial")
            val result = Git.extractProtos("HEAD", Some(dir))
            (
              result.toOption.exists(p => Files.exists(p.resolve("a.proto"))),
              result.toOption.exists(p => Files.exists(p.resolve("sub/b.proto"))),
              result.toOption.exists(p => Files.exists(p.resolve("README.md"))),
              result.isRight
            )
          }
          assertTrue(isRight, hasA, hasB, !hasMd)
        },
        test("extractProtos returns error for missing ref") {
          val isLeft = withRepo { dir =>
            writeFile(dir.resolve("a.proto"), """syntax = "proto3"; message Foo {}""")
            commit(dir, "initial")
            Git.extractProtos("definitely-not-a-ref-12345", Some(dir)).isLeft
          }
          assertTrue(isLeft)
        }
      )
}
