package proteus.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import proteus.ProtoIR.CompilationUnit
import proteus.ProtoParser

/**
  * File loading helpers for the proto diff CLI.
  *
  * `loadFile` parses a single `.proto` file. `loadDirectory` recursively finds and parses all
  * `.proto` files under a directory, keying them by their path relative to the root. `load`
  * auto-detects file vs directory.
  */
object ProtoFiles {

  /**
    * Reads and parses a single `.proto` file.
    */
  def loadFile(path: Path): Either[String, CompilationUnit] =
    if (!Files.exists(path)) Left(s"File not found: $path")
    else if (Files.isDirectory(path)) Left(s"Expected a file, got a directory: $path")
    else {
      val content = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      ProtoParser.parse(content).left.map(err => s"$path: $err")
    }

  /**
    * Recursively finds all `*.proto` files under `root`, parses each, and returns them keyed by
    * path relative to `root`. Aggregates parse errors per file.
    */
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

  /**
    * Loads either a single proto file or all proto files in a directory.
    *
    * For a single file, returns a one-entry map keyed by the file name.
    */
  def load(path: Path): Either[String, Map[String, CompilationUnit]] =
    if (!Files.exists(path)) Left(s"Path not found: $path")
    else if (Files.isDirectory(path)) loadDirectory(path)
    else loadFile(path).map(unit => Map(path.getFileName.toString -> unit))
}
