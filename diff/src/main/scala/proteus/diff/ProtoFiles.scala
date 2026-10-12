package proteus.diff

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

import proteus.ProtoIR.CompilationUnit
import proteus.ProtoParser

enum Resolved {
  case Single(name: String, unit: CompilationUnit)
  case Multi(files: Map[String, CompilationUnit])

  def asFiles: Map[String, CompilationUnit] = this match {
    case Single(name, unit) => Map(name -> unit)
    case Multi(files)       => files
  }
}

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
    * Resolves both sides of a comparison. A git ref is read from the directory given on the other side,
    * so `HEAD ./proto` compares `proto/` at `HEAD` with its working copy.
    */
  def resolvePair(old: String, `new`: String): Either[String, (Resolved, Resolved)] =
    for {
      o <- resolve(old, directory(`new`))
      n <- resolve(`new`, directory(old))
    } yield (o, n)

  def resolve(arg: String, refDir: Option[Path] = None): Either[String, Resolved] =
    if (arg.startsWith("git:")) resolveRef(arg.stripPrefix("git:"), explicit = true, refDir)
    else {
      val path = Paths.get(arg)
      if (Files.isDirectory(path)) loadDirectory(path).map(Resolved.Multi(_))
      else if (Files.isRegularFile(path))
        loadFile(path).map(Resolved.Single(path.getFileName.toString, _))
      else resolveRef(arg, explicit = false, refDir)
    }

  private def directory(arg: String): Option[Path] =
    Option.unless(arg.startsWith("git:"))(Paths.get(arg)).filter(Files.isDirectory(_))

  private def resolveRef(ref: String, explicit: Boolean, refDir: Option[Path]): Either[String, Resolved] =
    Git.extractProtos(ref, refDir) match {
      case Right(dir)            => loadDirectory(dir).map(Resolved.Multi(_))
      case Left(err) if explicit => Left(err)
      case Left(_)               =>
        Left(s"'$ref' is not a file/directory, and not a valid git ref.\nHint: check that the path or ref exists.")
    }
}
