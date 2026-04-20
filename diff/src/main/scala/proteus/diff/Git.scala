package proteus.diff

import java.io.{InputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

object Git {

  /**
    * Extracts all files at the given ref into a temp dir via a `git archive | tar -x` pipeline.
    * Much faster than invoking `git show` per file (one subprocess pair vs N+1).
    */
  def extractProtos(ref: String, cwd: Option[Path] = None): Either[String, Path] = {
    val tmp = Files.createTempDirectory("proteus-diff-git-")
    sys.addShutdownHook(deleteRecursively(tmp))

    val gitPb = new ProcessBuilder("git", "archive", "--format=tar", ref, "--", ":(glob)**/*.proto").redirectErrorStream(false)
    cwd.foreach(p => gitPb.directory(p.toFile): Unit)
    val tarPb = new ProcessBuilder("tar", "-x", "-C", tmp.toString)

    try {
      val pipeline  = ProcessBuilder.startPipeline(java.util.List.of(gitPb, tarPb)).asScala.toList
      val exitCodes = pipeline.map(_.waitFor())
      if (exitCodes.forall(_ == 0)) Right(tmp)
      else {
        val gitErr = readStream(pipeline.head.getErrorStream).trim
        Left(if (gitErr.nonEmpty) gitErr else s"git/tar pipeline failed with exit codes ${exitCodes.mkString(",")}")
      }
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
