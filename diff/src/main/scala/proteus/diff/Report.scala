package proteus.diff

import proteus.{Change, CompatMode, ProtoDiff, Severity, SeverityOverrides}

object Report {

  def format(
    changes: List[Change],
    mode: CompatMode,
    byFile: Boolean,
    overrides: SeverityOverrides = SeverityOverrides.empty,
    color: Boolean = false
  ): String =
    if (changes.isEmpty) "No changes detected.\n"
    else {
      val c  = if (color) Colors.on else Colors.off
      val sb = new StringBuilder
      sb.append(s"Proto changes (${changes.size})\n")
      if (byFile) {
        val byFileGroup = changes.groupBy(_.path.headOption.getOrElse("<root>"))
        byFileGroup.toList.sortBy(_._1).foreach { case (file, fileChanges) =>
          sb.append(s"  $file\n")
          appendByType(sb, fileChanges, mode, overrides, indent = "    ", stripFile = true, c)
        }
      } else appendByType(sb, changes, mode, overrides, indent = "  ", stripFile = false, c)
      sb.toString
    }

  def formatMarkdown(
    changes: List[Change],
    mode: CompatMode,
    byFile: Boolean,
    overrides: SeverityOverrides = SeverityOverrides.empty
  ): String =
    if (changes.isEmpty) "**No proto changes detected.**\n"
    else {
      val sb         = new StringBuilder
      val bySeverity = changes.groupBy(ch => ProtoDiff.severity(ch, mode, overrides))
      sb.append(s"## Proto changes (${changes.size})\n\n")
      List(Severity.Error, Severity.Warning, Severity.Info).foreach { sev =>
        bySeverity.get(sev).foreach { cs =>
          sb.append(s"- ${sevEmoji(sev)} ${cs.size} ${sevPlural(sev, cs.size)}\n")
        }
      }
      sb.append('\n')
      if (byFile) {
        val byFileGroup = changes.groupBy(ch => ch.path.headOption.getOrElse("<root>"))
        byFileGroup.toList.sortBy(_._1).foreach { case (file, fileChanges) =>
          sb.append(s"### `$file`\n\n")
          fileChanges.foreach(ch => appendMarkdownLine(sb, ch, mode, overrides, stripFile = true))
          sb.append('\n')
        }
      } else changes.foreach(ch => appendMarkdownLine(sb, ch, mode, overrides, stripFile = false))
      sb.toString
    }

  private def appendMarkdownLine(sb: StringBuilder, ch: Change, mode: CompatMode, overrides: SeverityOverrides, stripFile: Boolean): Unit = {
    val sev      = ProtoDiff.severity(ch, mode, overrides)
    val rendered = if (stripFile) renderWithoutFile(ch) else ch.toString
    sb.append(s"- ${sevEmoji(sev)} **${ch.productPrefix}** — $rendered\n")
  }

  private def sevEmoji(sev: Severity): String = sev match {
    case Severity.Error   => "🔴"
    case Severity.Warning => "🟡"
    case Severity.Info    => "🔵"
  }

  private def sevPlural(sev: Severity, n: Int): String = {
    val base = sev match {
      case Severity.Error   => "error"
      case Severity.Warning => "warning"
      case Severity.Info    => "info"
    }
    if (sev == Severity.Info || n == 1) base else base + "s"
  }

  def formatJson(
    changes: List[Change],
    mode: CompatMode,
    overrides: SeverityOverrides = SeverityOverrides.empty
  ): String = {
    val entries = changes.map { ch =>
      val sev  = ProtoDiff.severity(ch, mode, overrides)
      val path = ch.path.map(escapeJson).map(s => s"\"$s\"").mkString("[", ", ", "]")
      s"""  {"type": "${ch.productPrefix}", "severity": "${sev.toString.toLowerCase}", "path": $path, "message": "${escapeJson(ch.toString)}"}"""
    }
    entries.mkString("[\n", ",\n", "\n]\n")
  }

  private def appendByType(
    sb: StringBuilder,
    changes: List[Change],
    mode: CompatMode,
    overrides: SeverityOverrides,
    indent: String,
    stripFile: Boolean,
    c: Colors
  ): Unit = {
    val byType = changes.groupBy(changeTypeName).toList.sortBy(_._1)
    byType.foreach { case (typeName, group) =>
      sb.append(s"$indent$typeName (${group.size})\n")
      group.foreach { ch =>
        val sev      = ProtoDiff.severity(ch, mode, overrides)
        val label    = sevLabel(sev, c)
        val rendered = if (stripFile) renderWithoutFile(ch) else ch.toString
        sb.append(s"$indent  $label $rendered\n")
      }
    }
  }

  private def sevLabel(sev: Severity, c: Colors): String = sev match {
    case Severity.Error   => s"${c.red}error:${c.reset}"
    case Severity.Warning => s"${c.yellow}warning:${c.reset}"
    case Severity.Info    => s"${c.blue}info:${c.reset}"
  }

  private def changeTypeName(c: Change): String = c.productPrefix

  private def renderWithoutFile(c: Change): String = {
    val s          = c.toString
    val fullPrefix = c.path.mkString(".") + ": "
    val tailPrefix = if (c.path.sizeIs <= 1) "" else c.path.tail.mkString(".") + ": "
    val stripped   = if (s.startsWith(fullPrefix)) s.substring(fullPrefix.length) else s
    tailPrefix + stripped
  }

  private def escapeJson(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")

  private case class Colors(red: String, yellow: String, blue: String, reset: String)

  private object Colors {
    val on: Colors  = Colors(red = "\u001b[31m", yellow = "\u001b[33m", blue = "\u001b[34m", reset = "\u001b[0m")
    val off: Colors = Colors(red = "", yellow = "", blue = "", reset = "")
  }
}
