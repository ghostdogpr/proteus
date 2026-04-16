package proteus.cli

import java.nio.file.{Files, Paths}

import mainargs.{arg, main, ParserForMethods, TokensReader}

import proteus.{CompatMode, ProtoDiff, Severity, SeverityOverrides}

object Main {

  @main
  def run(
    @arg(positional = true, doc = "old proto file or directory") old: String,
    @arg(positional = true, doc = "new proto file or directory") `new`: String,
    @arg(short = 'm', doc = "compat mode: wire | source | strictest (default: strictest)")
    mode: CompatMode = CompatMode.Strictest,
    @arg(short = 's', doc = "minimum severity: error | warning | info (default: error)")
    severity: Severity = Severity.Error,
    @arg(short = 'o', doc = "severity override: mode.ChangeType=severity (e.g. wire.FieldRemoved=info)")
    `override`: List[String] = Nil,
    @arg(short = 'f', doc = "output format: text | json (default: text)")
    format: String = "text",
    @arg(doc = "exit 1 if any change at this severity or above: error | warning | info (default: error)")
    failOn: Severity = Severity.Error,
    @arg(doc = "color output: auto | always | never (default: auto)")
    color: String = "auto"
  ): Unit = {
    val useColor = color match {
      case "always" => true
      case "never"  => false
      case _        => System.console() != null
    }

    val outputFormat = format.toLowerCase match {
      case "json" => "json"
      case "text" => "text"
      case other  => fail(s"unknown format '$other' (expected: text | json)")
    }

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

    val output     = outputFormat match {
      case "json" => Report.formatJson(filtered, mode, overrides)
      case _      => Report.format(filtered, mode, isDirectoryComparison, overrides, useColor)
    }
    print(output)
    val shouldFail = filtered.exists(c => ProtoDiff.severity(c, mode, overrides).level >= failOn.level)
    sys.exit(if (shouldFail) 1 else 0)
  }

  private def fail(message: String): Nothing = {
    System.err.println(message)
    sys.exit(2)
  }

  private val helpText: String =
    """Usage: proteus-diff [options] <old> <new>
      |
      |Arguments:
      |  old                        old proto file or directory
      |  new                        new proto file or directory
      |
      |Options:
      |  -m, --mode <mode>          compat mode: wire | source | strictest (default: strictest)
      |  -s, --severity <severity>  minimum severity to display: error | warning | info (default: error)
      |  -f, --format <format>      output format: text | json (default: text)
      |  --fail-on <severity>       exit 1 at this severity or above: error | warning | info (default: error)
      |  -o, --override <override>  severity override: mode.ChangeType=severity (repeatable)
      |  --color <mode>             color output: auto | always | never (default: auto)
      |  -v, --version              print version and exit
      |  -h, --help                 print this help""".stripMargin

  private val buildVersion: String =
    Option(getClass.getResourceAsStream("/proteus-version.txt"))
      .map(is => new String(is.readAllBytes()).trim)
      .getOrElse("dev")

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

  def main(args: Array[String]): Unit =
    if (args.contains("-v") || args.contains("--version")) {
      println(s"proteus-diff $buildVersion")
      sys.exit(0)
    } else if (args.contains("-h") || args.contains("--help") || args.isEmpty) {
      println(helpText)
      sys.exit(0)
    } else ParserForMethods(this).runOrExit(args.toIndexedSeq): Unit
}
