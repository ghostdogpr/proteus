package proteus.diff

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import proteus.ProtoIR.CompilationUnit
import proteus.ProtoParser

case class Resolved(files: Map[String, CompilationUnit], singleFile: Boolean)

object ProtoFiles {

  def loadFile(path: Path): Either[String, CompilationUnit] =
    if (!Files.exists(path)) Left(s"File not found: $path")
    else if (Files.isDirectory(path)) Left(s"Expected a file, got a directory: $path")
    else {
      val content = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      ProtoParser.parse(content).left.map(err => s"$path: $err")
    }

  def loadDirectory(root: Path): Either[String, Map[String, CompilationUnit]] =
    if (!Files.exists(root)) Left(s"Directory not found: $root")
    else if (!Files.isDirectory(root)) Left(s"Expected a directory, got a file: $root")
    else {
      val protoFiles = Files
        .walk(root)
        .iterator()
        .asScala
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".proto"))
        .toList
        .sorted

      val results: List[(String, Either[String, CompilationUnit])] = protoFiles.map { p =>
        val rel = root.relativize(p).toString
        rel -> loadFile(p)
      }

      val errors = results.collect { case (_, Left(err)) => err }
      val parsed = results.collect { case (rel, Right(unit)) => rel -> unit }.toMap

      if (errors.nonEmpty) Left(errors.mkString("\n"))
      else Right(parsed)
    }

  def load(path: Path): Either[String, Map[String, CompilationUnit]] =
    if (!Files.exists(path)) Left(s"Path not found: $path")
    else if (Files.isDirectory(path)) loadDirectory(path)
    else loadFile(path).map(unit => Map(path.getFileName.toString -> unit))

  /**
    * Resolves an argument as either a filesystem path or a git ref.
    *
    * `git:<ref>` forces git mode. Otherwise, a valid filesystem path wins; else git ref is tried.
    */
  def resolve(arg: String): Either[String, Resolved] =
    if (arg.startsWith("git:")) resolveRef(arg.stripPrefix("git:"), explicit = true)
    else {
      val path = Paths.get(arg)
      if (Files.isDirectory(path)) loadDirectory(path).map(files => Resolved(files, singleFile = false))
      else if (Files.isRegularFile(path))
        loadFile(path).map(unit => Resolved(Map(path.getFileName.toString -> unit), singleFile = true))
      else resolveRef(arg, explicit = false)
    }

  private def resolveRef(ref: String, explicit: Boolean): Either[String, Resolved] =
    Git.extractProtos(ref) match {
      case Right(dir)            => loadDirectory(dir).map(files => Resolved(files, singleFile = false))
      case Left(err) if explicit => Left(err)
      case Left(_)               =>
        Left(s"'$ref' is not a file/directory, and not a valid git ref.\nHint: check that the path or ref exists.")
    }
}
