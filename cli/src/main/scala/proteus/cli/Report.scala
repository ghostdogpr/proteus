package proteus.cli

import proteus.{Change, CompatMode, ProtoDiff, Severity, SeverityOverrides}

/**
  * Formats a list of [[Change]]s into a human-readable report.
  *
  * Output is grouped by **severity → change type**, with an extra **file** level inserted between
  * severity and change type when comparing directories (where each change has a file segment as
  * the first path element).
  */
object Report {

  private val severityOrder: List[Severity] = List(Severity.Error, Severity.Warning, Severity.Info)

  /**
    * Renders the report as a single string (with a trailing newline). Returns `"No changes detected.\n"`
    * when the input is empty.
    */
  def format(
    changes: List[Change],
    mode: CompatMode,
    byFile: Boolean,
    overrides: SeverityOverrides = SeverityOverrides.empty
  ): String =
    if (changes.isEmpty) "No changes detected.\n"
    else {
      val sb         = new StringBuilder
      val bySeverity = changes.groupBy(c => ProtoDiff.severity(c, mode, overrides))
      severityOrder.foreach { sev =>
        bySeverity.get(sev).foreach { sevChanges =>
          val label = sev match {
            case Severity.Error   => "Errors"
            case Severity.Warning => "Warnings"
            case Severity.Info    => "Info"
          }
          sb.append(s"$label (${sevChanges.size})\n")
          if (byFile) {
            val byFileGroup = sevChanges.groupBy(c => c.path.headOption.getOrElse("<root>"))
            byFileGroup.toList.sortBy(_._1).foreach { case (file, fileChanges) =>
              sb.append(s"  $file\n")
              appendByType(sb, fileChanges, indent = "    ", stripFile = true)
            }
          } else {
            appendByType(sb, sevChanges, indent = "  ", stripFile = false)
          }
          sb.append('\n')
        }
      }
      sb.toString
    }

  private def appendByType(sb: StringBuilder, changes: List[Change], indent: String, stripFile: Boolean): Unit = {
    val byType = changes.groupBy(changeTypeName).toList.sortBy(_._1)
    byType.foreach { case (typeName, group) =>
      sb.append(s"$indent$typeName (${group.size})\n")
      group.foreach { c =>
        val rendered = if (stripFile) renderWithoutFile(c) else c.toString
        sb.append(s"$indent  $rendered\n")
      }
    }
  }

  private def changeTypeName(c: Change): String = c.productPrefix

  /**
    * Renders a change using its tail-of-path (drops the file segment, since file is shown as a header).
    */
  private def renderWithoutFile(c: Change): String = {
    val s          = c.toString
    val fullPrefix = c.path.mkString(".") + ": "
    val tailPrefix = if (c.path.sizeIs <= 1) "" else c.path.tail.mkString(".") + ": "
    val stripped   = if (s.startsWith(fullPrefix)) s.substring(fullPrefix.length) else s
    tailPrefix + stripped
  }
}
