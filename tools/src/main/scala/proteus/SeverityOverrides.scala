package proteus

case class SeverityOverrides(wire: Map[String, Severity] = Map.empty, source: Map[String, Severity] = Map.empty)

object SeverityOverrides {

  val empty: SeverityOverrides = SeverityOverrides()

  private val validChangeTypes: Set[String] = Set(
    "PackageChanged",
    "FileAdded",
    "FileRemoved",
    "MessageMoved",
    "EnumMoved",
    "ImportAdded",
    "ImportRemoved",
    "ImportModifierChanged",
    "MessageAdded",
    "MessageRemoved",
    "MessageRenamed",
    "FieldAdded",
    "FieldRemoved",
    "FieldNumberChanged",
    "FieldRenamed",
    "FieldTypeChanged",
    "FieldTypeRefRenamed",
    "FieldOptionalityChanged",
    "FieldOrderChanged",
    "FieldOneOfChanged",
    "EnumAdded",
    "EnumRemoved",
    "EnumRenamed",
    "EnumValueAdded",
    "EnumValueRemoved",
    "EnumValueNumberChanged",
    "EnumValueRenamed",
    "ReservedAdded",
    "ReservedRemoved",
    "OptionAdded",
    "OptionRemoved",
    "OptionChanged",
    "ServiceAdded",
    "ServiceRemoved",
    "RpcAdded",
    "RpcRemoved",
    "RpcRequestTypeChanged",
    "RpcResponseTypeChanged",
    "RpcStreamingChanged",
    "CommentAdded",
    "CommentRemoved",
    "CommentChanged"
  )

  def parse(entries: Seq[String]): Either[String, SeverityOverrides] =
    entries.foldLeft[Either[String, SeverityOverrides]](Right(empty)) {
      case (Left(err), _)      => Left(err)
      case (Right(acc), entry) =>
        parseEntry(entry).map { case (mode, changeType, sev) =>
          mode match {
            case "wire"   => acc.copy(wire = acc.wire + (changeType -> sev))
            case "source" => acc.copy(source = acc.source + (changeType -> sev))
            case _        => acc
          }
        }
    }

  private def parseEntry(entry: String): Either[String, (String, String, Severity)] =
    entry.split("=", 2) match {
      case Array(lhs, rhs) =>
        for {
          sev <- parseSeverity(rhs.trim).toRight(s"invalid severity '${rhs.trim}' in '$entry' (expected: error, warning, info)")
          mct <- lhs.trim.split("\\.", 2) match {
                   case Array(m @ ("wire" | "source"), ct) => Right((m, ct))
                   case _                                  => Left(s"invalid override '$entry' (expected: mode.ChangeType=severity, mode is wire or source)")
                 }
          _   <- Either.cond(validChangeTypes.contains(mct._2), (), s"unknown change type '${mct._2}' in '$entry'")
        } yield (mct._1, mct._2, sev)
      case _               =>
        Left(s"invalid override '$entry' (expected: mode.ChangeType=severity)")
    }

  private def parseSeverity(s: String): Option[Severity] = s.toLowerCase match {
    case "error"   => Some(Severity.Error)
    case "warning" => Some(Severity.Warning)
    case "info"    => Some(Severity.Info)
    case _         => None
  }
}
