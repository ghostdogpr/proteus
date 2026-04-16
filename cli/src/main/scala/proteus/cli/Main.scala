package proteus.cli

import java.nio.file.{Files, Paths}

import mainargs.{arg, main, ParserForMethods, TokensReader}

import proteus.{CompatMode, ProtoDiff, Severity, SeverityOverrides}

object Main {

  /**
    * Compares two `.proto` files (or two directories of `.proto` files) and prints the changes,
    * grouped by severity, then by file (when comparing directories), then by change type.
    *
    * Exits with code 1 if any Error-severity change is reported after filtering, 0 otherwise.
    */
  @main
  def run(
    @arg(positional = true, doc = "old proto file or directory") old: String,
    @arg(positional = true, doc = "new proto file or directory") `new`: String,
    @arg(short = 'm', doc = "compat mode: wire | source | strictest (default: strictest)")
    mode: CompatMode = CompatMode.Strictest,
    @arg(short = 's', doc = "minimum severity: error | warning | info (default: error)")
    severity: Severity = Severity.Error,
    @arg(short = 'o', doc = "severity override: mode.ChangeType=severity (e.g. wire.FieldRemoved=info)")
    `override`: List[String] = Nil
  ): Unit = {
    val oldPath               = Paths.get(old)
    val newPath               = Paths.get(`new`)
    val isDirectoryComparison = Files.isDirectory(oldPath) || Files.isDirectory(newPath)

    val overrides = SeverityOverrides.parse(`override`).fold(err => fail(err), identity)

    val changes  =
      if (isDirectoryComparison) {
        val oldFiles = ProtoFiles.load(oldPath).fold(err => fail(err), identity)
        val newFiles = ProtoFiles.load(newPath).fold(err => fail(err), identity)
        ProtoDiff.diffFiles(oldFiles, newFiles)
      } else {
        val oldUnit = ProtoFiles.loadFile(oldPath).fold(err => fail(err), identity)
        val newUnit = ProtoFiles.loadFile(newPath).fold(err => fail(err), identity)
        ProtoDiff.diff(oldUnit, newUnit)
      }
    val filtered = changes.filter(c => ProtoDiff.severity(c, mode, overrides).level >= severity.level)

    val output    = Report.format(filtered, mode, isDirectoryComparison, overrides)
    print(output)
    val hasErrors = filtered.exists(c => ProtoDiff.severity(c, mode, overrides) == Severity.Error)
    sys.exit(if (hasErrors) 1 else 0)
  }

  private def fail(message: String): Nothing = {
    System.err.println(message)
    sys.exit(2)
  }

  // ── mainargs typeclass instances ──────────────────────────────────────

  given TokensReader.Simple[CompatMode] = new TokensReader.Simple[CompatMode] {
    def shortName: String                                   = "mode"
    def read(strs: Seq[String]): Either[String, CompatMode] =
      strs.last.toLowerCase match {
        case "wire"      => Right(CompatMode.Wire)
        case "source"    => Right(CompatMode.Source)
        case "strictest" => Right(CompatMode.Strictest)
        case other       => Left(s"unknown mode '$other' (expected: wire | source | strictest)")
      }
  }

  given TokensReader.Simple[Severity] = new TokensReader.Simple[Severity] {
    def shortName: String                                 = "severity"
    def read(strs: Seq[String]): Either[String, Severity] =
      strs.last.toLowerCase match {
        case "error"   => Right(Severity.Error)
        case "warning" => Right(Severity.Warning)
        case "info"    => Right(Severity.Info)
        case other     => Left(s"unknown severity '$other' (expected: error | warning | info)")
      }
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args.toIndexedSeq): Unit
}
