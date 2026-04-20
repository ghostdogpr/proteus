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
      appendSummary(sb, changes, mode, overrides, c)
      if (byFile) {
        val byFileGroup = changes.groupBy(_.path.headOption.getOrElse("<root>"))
        byFileGroup.toList.sortBy(_._1).foreach { case (file, fileChanges) =>
          sb.append(s"\n${c.boldCyan}$file${c.reset}\n")
          appendByEnclosingType(sb, fileChanges, mode, overrides, indent = "  ", stripFile = true, c)
        }
      } else {
        sb.append('\n')
        appendByEnclosingType(sb, changes, mode, overrides, indent = "", stripFile = false, c)
      }
      sb.toString
    }

  private def appendSummary(
    sb: StringBuilder,
    changes: List[Change],
    mode: CompatMode,
    overrides: SeverityOverrides,
    c: Colors
  ): Unit = {
    val sevCounts = changes.groupBy(ch => ProtoDiff.severity(ch, mode, overrides)).view.mapValues(_.size).toMap
    val errors    = sevCounts.getOrElse(Severity.Error, 0)
    val warnings  = sevCounts.getOrElse(Severity.Warning, 0)
    val infos     = sevCounts.getOrElse(Severity.Info, 0)
    sb.append(s"Proto changes (${changes.size}): ")
    sb.append(s"${c.red}$errors ${sevPlural(Severity.Error, errors)}${c.reset}, ")
    sb.append(s"${c.yellow}$warnings ${sevPlural(Severity.Warning, warnings)}${c.reset}, ")
    sb.append(s"${c.blue}$infos ${sevPlural(Severity.Info, infos)}${c.reset}\n")
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
          appendMarkdownByEnclosingType(sb, fileChanges, mode, overrides, stripFile = true)
          val n = sb.length
          if (n < 2 || sb.charAt(n - 1) != '\n' || sb.charAt(n - 2) != '\n') sb.append('\n')
        }
      } else appendMarkdownByEnclosingType(sb, changes, mode, overrides, stripFile = false)
      sb.toString
    }

  private def appendMarkdownByEnclosingType(
    sb: StringBuilder,
    changes: List[Change],
    mode: CompatMode,
    overrides: SeverityOverrides,
    stripFile: Boolean
  ): Unit =
    forEachGroup(changes, mode, overrides, stripFile)(
      onFileLevel = (ch, sev) => sb.append(s"- ${markdownLine(ch, sev)}\n"),
      onGroupHeader = (group, _) => sb.append(s"#### `$group`\n\n"),
      onTypedLine = (ch, sev) => sb.append(s"- ${markdownLine(ch, sev)}\n"),
      betweenFileLevelAndTyped = () => sb.append('\n'),
      afterGroup = () => sb.append('\n')
    )

  private def markdownLine(ch: Change, sev: Severity): String =
    s"${sevEmoji(sev)} **${ch.productPrefix}** — ${renderChangeNoPrefix(ch)}"

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

  private def appendByEnclosingType(
    sb: StringBuilder,
    changes: List[Change],
    mode: CompatMode,
    overrides: SeverityOverrides,
    indent: String,
    stripFile: Boolean,
    c: Colors
  ): Unit = {
    def line(ch: Change, sev: Severity, extraIndent: String): Unit =
      sb.append(s"$indent$extraIndent${sevLabel(sev, c)} [${ch.productPrefix}] ${renderChangeNoPrefix(ch)}\n")
    forEachGroup(changes, mode, overrides, stripFile)(
      onFileLevel = (ch, sev) => line(ch, sev, ""),
      onGroupHeader = (group, n) => sb.append(s"$indent${c.bold}$group${c.reset} ($n)\n"),
      onTypedLine = (ch, sev) => line(ch, sev, "  "),
      betweenFileLevelAndTyped = () => (),
      afterGroup = () => ()
    )
  }

  private def forEachGroup(changes: List[Change], mode: CompatMode, overrides: SeverityOverrides, stripFile: Boolean)(
    onFileLevel: (Change, Severity) => Unit,
    onGroupHeader: (String, Int) => Unit,
    onTypedLine: (Change, Severity) => Unit,
    betweenFileLevelAndTyped: () => Unit,
    afterGroup: () => Unit
  ): Unit = {
    val withSev                            = changes.map(ch => ch -> ProtoDiff.severity(ch, mode, overrides))
    def severityKey(p: (Change, Severity)) = (-p._2.level, sortKey(p._1))
    val (fileLevel, typed)                 = withSev.partition { case (ch, _) => isFileLevel(ch) }
    fileLevel.sortBy(severityKey).foreach { case (ch, sev) => onFileLevel(ch, sev) }
    if (fileLevel.nonEmpty && typed.nonEmpty) betweenFileLevelAndTyped()
    typed.groupBy { case (ch, _) => enclosingType(ch, stripFile) }.toList.sortBy(_._1).foreach { case (group, items) =>
      onGroupHeader(group, items.size)
      items.sortBy(severityKey).foreach { case (ch, sev) => onTypedLine(ch, sev) }
      afterGroup()
    }
  }

  private def isFileLevel(ch: Change): Boolean = {
    import Change.*
    ch match {
      case _: PackageChanged | _: FileAdded | _: FileRemoved | _: ImportAdded | _: ImportRemoved | _: ImportModifierChanged => true
      case _                                                                                                                => false
    }
  }

  private def enclosingType(ch: Change, stripFile: Boolean): String = {
    import Change.*
    val effectivePath        = if (stripFile && ch.path.nonEmpty) ch.path.tail else ch.path
    def qualified(n: String) = if (effectivePath.isEmpty) n else (effectivePath :+ n).mkString(".")
    def parentOrRoot         = if (effectivePath.isEmpty) "<root>" else effectivePath.mkString(".")
    ch match {
      case MessageAdded(_, name)         => qualified(name)
      case MessageRemoved(_, name)       => qualified(name)
      case MessageRenamed(_, _, newName) => qualified(newName)
      case EnumAdded(_, name)            => qualified(name)
      case EnumRemoved(_, name)          => qualified(name)
      case EnumRenamed(_, _, newName)    => qualified(newName)
      case ServiceAdded(_, name)         => qualified(name)
      case ServiceRemoved(_, name)       => qualified(name)
      case MessageMoved(_, name, _, _)   => qualified(name)
      case EnumMoved(_, name, _, _)      => qualified(name)
      case _                             => parentOrRoot
    }
  }

  private def sortKey(ch: Change): (Int, String) = {
    import Change.*
    ch match {
      case FieldAdded(_, n, num)                 => (num, n)
      case FieldRemoved(_, n, num, _)            => (num, n)
      case FieldNumberChanged(_, n, num, _)      => (num, n)
      case FieldRenamed(_, num, n, _)            => (num, n)
      case FieldTypeChanged(_, n, num, _, _)     => (num, n)
      case FieldTypeRefRenamed(_, n, num, _, _)  => (num, n)
      case FieldOptionalityChanged(_, n, num, _) => (num, n)
      case FieldOneOfChanged(_, n, num, _, _)    => (num, n)
      case EnumValueAdded(_, n, num)             => (num, n)
      case EnumValueRemoved(_, n, num, _)        => (num, n)
      case EnumValueNumberChanged(_, n, num, _)  => (num, n)
      case EnumValueRenamed(_, num, n, _)        => (num, n)
      case _                                     => (Int.MaxValue, ch.toString)
    }
  }

  private def renderChangeNoPrefix(ch: Change): String = {
    val s      = ch.toString
    val prefix = ch.path.mkString(".") + ": "
    if (s.startsWith(prefix)) s.substring(prefix.length) else s
  }

  private def sevLabel(sev: Severity, c: Colors): String = sev match {
    case Severity.Error   => s"${c.red}error${c.reset}"
    case Severity.Warning => s"${c.yellow}warning${c.reset}"
    case Severity.Info    => s"${c.blue}info${c.reset}"
  }

  private def escapeJson(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")

  private case class Colors(red: String, yellow: String, blue: String, bold: String, boldCyan: String, reset: String)

  private object Colors {
    val on: Colors  =
      Colors(red = "\u001b[31m", yellow = "\u001b[33m", blue = "\u001b[34m", bold = "\u001b[1m", boldCyan = "\u001b[1;36m", reset = "\u001b[0m")
    val off: Colors = Colors(red = "", yellow = "", blue = "", bold = "", boldCyan = "", reset = "")
  }
}
