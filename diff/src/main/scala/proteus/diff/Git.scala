package proteus.diff

import java.io.{InputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

object Git {

  def extractProtos(ref: String, cwd: Option[Path] = None): Either[String, Path] =
    runGit(cwd, "ls-tree", "-r", "--name-only", ref).flatMap { stdout =>
      val protos   = stdout.linesIterator.filter(_.endsWith(".proto")).toList
      val tmp      = Files.createTempDirectory("proteus-diff-git-")
      sys.addShutdownHook(deleteRecursively(tmp))
      val attempts = protos.map { path =>
        val target = tmp.resolve(path)
        Option(target.getParent).foreach(p => Files.createDirectories(p): Unit)
        runGit(cwd, "show", s"$ref:$path").map { content =>
          Files.writeString(target, content, StandardCharsets.UTF_8): Unit
        }
      }
      attempts.find(_.isLeft) match {
        case Some(Left(err)) => Left(s"failed to extract $ref: $err")
        case _               => Right(tmp)
      }
    }

  private def runGit(cwd: Option[Path], args: String*): Either[String, String] = {
    val pb = new ProcessBuilder(("git" +: args)*)
    cwd.foreach(p => pb.directory(p.toFile): Unit)
    try {
      val p      = pb.start()
      val stdout = readStream(p.getInputStream)
      val stderr = readStream(p.getErrorStream)
      val code   = p.waitFor()
      if (code == 0) Right(stdout)
      else Left(if (stderr.nonEmpty) stderr.trim else s"git exited with code $code")
    } catch {
      case _: IOException => Left("git not found on PATH")
    }
  }

  private def readStream(is: InputStream): String =
    new String(is.readAllBytes(), StandardCharsets.UTF_8)

  private def deleteRecursively(root: Path): Unit =
    if (Files.exists(root))
      Files
        .walk(root)
        .sorted(java.util.Comparator.reverseOrder())
        .iterator()
        .asScala
        .foreach(p => Files.deleteIfExists(p): Unit)
}
