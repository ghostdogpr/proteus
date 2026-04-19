package proteus.diff

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.test.*

object ProtoFilesSpec extends ZIOSpecDefault {

  private def withTempDir[A](f: Path => A): A = {
    val dir = Files.createTempDirectory("proteus-diff-test")
    try f(dir)
    finally deleteRecursively(dir)
  }

  private def deleteRecursively(path: Path): Unit = {
    if (Files.isDirectory(path))
      Files.list(path).forEach(deleteRecursively)
    Files.deleteIfExists(path): Unit
  }

  private def writeFile(path: Path, content: String): Path = {
    Option(path.getParent).foreach(p => Files.createDirectories(p): Unit)
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  def spec = suite("ProtoFilesSpec")(
    suite("loadFile")(
      test("loads and parses a valid proto file") {
        withTempDir { dir =>
          val file   = writeFile(dir.resolve("foo.proto"), """syntax = "proto3"; message Foo { string name = 1; }""")
          val result = ProtoFiles.loadFile(file)
          assertTrue(result.exists(_.statements.nonEmpty))
        }
      },
      test("returns Left for missing file") {
        val result = ProtoFiles.loadFile(Path.of("/nonexistent/file.proto"))
        assertTrue(result.left.exists(_.contains("File not found")))
      },
      test("returns Left when path is a directory") {
        withTempDir { dir =>
          val result = ProtoFiles.loadFile(dir)
          assertTrue(result.left.exists(_.contains("Expected a file")))
        }
      },
      test("returns Left with parse error containing the file path") {
        withTempDir { dir =>
          val file   = writeFile(dir.resolve("bad.proto"), "this is not a proto file")
          val result = ProtoFiles.loadFile(file)
          assertTrue(
            result.isLeft,
            result.left.exists(_.contains("bad.proto"))
          )
        }
      }
    ),
    suite("loadDirectory")(
      test("recursively loads all .proto files keyed by relative path") {
        withTempDir { dir =>
          writeFile(dir.resolve("a.proto"), """syntax = "proto3"; message A {}""")
          writeFile(dir.resolve("sub/b.proto"), """syntax = "proto3"; message B {}""")
          val result = ProtoFiles.loadDirectory(dir)
          assertTrue(
            result.exists(_.keySet == Set("a.proto", "sub/b.proto"))
          )
        }
      },
      test("ignores non-proto files") {
        withTempDir { dir =>
          writeFile(dir.resolve("a.proto"), """syntax = "proto3"; message A {}""")
          writeFile(dir.resolve("README.md"), "not a proto file")
          val result = ProtoFiles.loadDirectory(dir)
          assertTrue(result.exists(_.keySet == Set("a.proto")))
        }
      },
      test("returns Left for missing directory") {
        val result = ProtoFiles.loadDirectory(Path.of("/nonexistent/dir"))
        assertTrue(result.left.exists(_.contains("Directory not found")))
      },
      test("returns Left when path is a file") {
        withTempDir { dir =>
          val file   = writeFile(dir.resolve("a.proto"), """syntax = "proto3"; message A {}""")
          val result = ProtoFiles.loadDirectory(file)
          assertTrue(result.left.exists(_.contains("Expected a directory")))
        }
      },
      test("aggregates parse errors per file") {
        withTempDir { dir =>
          writeFile(dir.resolve("a.proto"), "garbage one")
          writeFile(dir.resolve("b.proto"), "garbage two")
          val result = ProtoFiles.loadDirectory(dir)
          assertTrue(
            result.isLeft,
            result.left.exists(_.contains("a.proto")),
            result.left.exists(_.contains("b.proto"))
          )
        }
      },
      test("empty directory produces empty map") {
        withTempDir { dir =>
          val result = ProtoFiles.loadDirectory(dir)
          assertTrue(result == Right(Map.empty))
        }
      }
    ),
    suite("load")(
      test("dispatches to loadFile for a single file") {
        withTempDir { dir =>
          val file   = writeFile(dir.resolve("foo.proto"), """syntax = "proto3"; message Foo {}""")
          val result = ProtoFiles.load(file)
          assertTrue(result.exists(_.keySet == Set("foo.proto")))
        }
      },
      test("dispatches to loadDirectory for a directory") {
        withTempDir { dir =>
          writeFile(dir.resolve("a.proto"), """syntax = "proto3"; message A {}""")
          writeFile(dir.resolve("b.proto"), """syntax = "proto3"; message B {}""")
          val result = ProtoFiles.load(dir)
          assertTrue(result.exists(_.keySet == Set("a.proto", "b.proto")))
        }
      },
      test("returns Left for missing path") {
        val result = ProtoFiles.load(Path.of("/nonexistent/anything"))
        assertTrue(result.left.exists(_.contains("Path not found")))
      }
    )
  )
}
