package proteus
package internal

import proteus.ProtoIR.*
import proteus.internal.Text.*

private[proteus] object Renderer {

  def render(compilationUnit: CompilationUnit): String = {
    val text = many(
      statement(s"""syntax = "proto3""""),
      emptyLine,
      maybe(
        compilationUnit.packageName.map(packageName => many(renderPackageName(packageName), emptyLine))
      ),
      many(
        compilationUnit.options.map(renderOption) ++
          compilationUnit.options.headOption.toList.map(_ => emptyLine)
      ),
      intersperse(
        many(compilationUnit.statements.map(renderStatement)),
        emptyLine
      ),
      emptyLine
    )

    renderText(text)
  }

  def renderTopLevelDef(topLevelDef: TopLevelDef): Text =
    topLevelDef match {
      case TopLevelDef.MessageDef(message) => renderMessage(message)
      case TopLevelDef.EnumDef(enumDef)    => renderEnum(enumDef)
      case TopLevelDef.ServiceDef(service) => renderService(service)
    }

  private def renderComment(comment: String): Text = {
    val lines = comment.split("\n")
    if (lines.length == 1) {
      line(s"// $comment")
    } else {
      many(lines.map(l => line(s"// $l")).toList)
    }
  }

  private def renderPackageName(packageName: String): Text =
    statement(s"package $packageName")

  private def renderOption(opt: TopLevelOption): Text =
    renderOptionStatement(opt.name, opt.value)

  private def renderOptionValCompact(v: OptionVal): String = v match {
    case OptionVal.Identifier(value)                      => value
    case OptionVal.IntLit(value)                          => value.toString
    case OptionVal.FloatLit(value)                        => renderFloatLit(value)
    case OptionVal.StringLit(value)                       => s""""$value""""
    case OptionVal.BoolLit(value)                         => value.toString
    case OptionVal.MessageValue(fields) if fields.isEmpty => "{}"
    case OptionVal.MessageValue(fields)                   =>
      fields.map { case (k, v) => s"$k: ${renderOptionValCompact(v)}" }.mkString("{ ", ", ", " }")
    case OptionVal.ScalarList(values)                     =>
      values.map(renderOptionValCompact).mkString("[", ", ", "]")
    case OptionVal.MessageList(values)                    =>
      values.map(renderOptionValCompact).mkString("[", ", ", "]")
  }

  private def renderFloatLit(value: Double): String =
    if (value == Double.PositiveInfinity) "inf"
    else if (value == Double.NegativeInfinity) "-inf"
    else if (value.isNaN) "nan"
    else value.toString

  private def renderMessageField(key: String, value: OptionVal): Text = value match {
    case OptionVal.MessageValue(fields) if fields.isEmpty => line(s"$key: {}")
    case OptionVal.MessageValue(fields)                   =>
      many(
        line(s"$key: {"),
        indent(fields.map { case (k, v) => renderMessageField(k, v) }),
        line("}")
      )
    case OptionVal.ScalarList(values)                     =>
      line(s"$key: [${values.map(renderOptionValCompact).mkString(", ")}]")
    case OptionVal.MessageList(values) if values.isEmpty  =>
      line(s"$key: []")
    case OptionVal.MessageList(values)                    =>
      many(
        line(s"$key: ["),
        indent(values.map(v => renderMessageField("", v))),
        line("]")
      )
    case other                                            =>
      line(s"$key: ${renderOptionValCompact(other)}")
  }

  private def renderInlineOptions(options: List[OptionValue]): String =
    if (options.isEmpty) "" else options.map(o => s"${o.name.render} = ${renderOptionValCompact(o.value)}").mkString(" [", ", ", "]")

  private def renderStatementOptions(options: List[OptionValue]): List[Text] =
    options.map(o => renderOptionStatement(o.name, o.value))

  private def renderOptionStatement(name: OptionName, value: OptionVal): Text =
    value match {
      case OptionVal.MessageValue(fields) if fields.isEmpty => line(s"option ${name.render} = {};")
      case OptionVal.MessageValue(fields)                   =>
        many(
          line(s"option ${name.render} = {"),
          indent(fields.map { case (k, v) => renderMessageField(k, v) }),
          line("};")
        )
      case other                                            => line(s"option ${name.render} = ${renderOptionValCompact(other)};")
    }

  private def renderStatement(st: Statement): Text =
    st match {
      case Statement.ImportStatement(path, modifier) =>
        val prefix = modifier.fold("")(_ + " ")
        statement(s"""import $prefix"$path"""")
      case Statement.TopLevelStatement(tld)          => renderTopLevelDef(tld)
    }

  private def renderEnumElement(enumValue: EnumValue): Text = {
    val opts = renderInlineOptions(enumValue.options)
    enumValue match {
      case EnumValue(identifier, intvalue, comment, _) =>
        comment match {
          case Some(c) if c.contains("\n") =>
            // Multiline comment - render on previous line(s)
            many(
              renderComment(c),
              line(s"$identifier = $intvalue$opts;")
            )
          case Some(c)                     =>
            // Single-line comment - render inline
            line(s"$identifier = $intvalue$opts; // $c")
          case None                        =>
            // No comment
            line(s"$identifier = $intvalue$opts;")
        }
    }
  }

  private def renderEnum(enumeration: Enum): Text = {
    val hasContent  = enumeration.reserved.nonEmpty || enumeration.values.nonEmpty || enumeration.options.nonEmpty
    val commentLine = enumeration.comment.map(renderComment).getOrElse(many())
    if (hasContent) {
      many(
        commentLine,
        line(s"enum ${enumeration.name} {"),
        indent(renderStatementOptions(enumeration.options)),
        indent(renderReserved(enumeration.reserved)),
        indent(enumeration.values.map(renderEnumElement)),
        line("}")
      )
    } else
      many(
        commentLine,
        line(s"enum ${enumeration.name} {}")
      )
  }

  private def renderMessage(message: Message): Text = {
    val hasOtherContent = message.reserved.nonEmpty || message.elements.exists(_ != ProtoIR.excludedMessageElement)
    val hasContent      = hasOtherContent || message.options.nonEmpty
    val commentLine     = message.comment.map(renderComment).getOrElse(many())
    val optionsSep      = if (message.options.nonEmpty && hasOtherContent) emptyLine else many()
    if (hasContent) {
      many(
        commentLine,
        line(s"message ${message.name} {"),
        indent(renderStatementOptions(message.options)),
        optionsSep,
        indent(renderReserved(message.reserved)),
        indent(message.elements.map(renderMessageElement)),
        line("}")
      )
    } else
      many(
        commentLine,
        line(s"message ${message.name} {}")
      )
  }

  private def renderOneOf(oneOf: OneOf): Text = {
    val commentLine = oneOf.comment.map(renderComment).getOrElse(many())
    if (oneOf.fields.nonEmpty) {
      many(
        commentLine,
        line(s"oneof ${oneOf.name} {"),
        indent(renderStatementOptions(oneOf.options)),
        indent(oneOf.fields.map(renderField(_, isOneOf = true))),
        line("}")
      )
    } else if (oneOf.options.nonEmpty) {
      many(
        commentLine,
        line(s"oneof ${oneOf.name} {"),
        indent(renderStatementOptions(oneOf.options)),
        line("}")
      )
    } else
      many(
        commentLine,
        line(s"oneof ${oneOf.name} {}")
      )
  }

  private def renderMessageElement(element: MessageElement): Text =
    element match {
      case MessageElement.FieldElement(field)           => renderField(field)
      case MessageElement.OneOfElement(oneOf)           => renderOneOf(oneOf)
      case MessageElement.NestedMessageElement(message) => many(renderMessage(message), emptyLine)
      case MessageElement.NestedEnumElement(enumDef)    => many(renderEnum(enumDef), emptyLine)
    }

  private def renderReserved(reserved: List[Reserved]): Text = {
    val numeric = reserved.collect { case r @ (_: Reserved.Number | _: Reserved.Range) => renderReservedValue(r) }
    val names   = reserved.collect { case r: Reserved.Name => renderReservedValue(r) }

    many(
      maybe(
        if (numeric.nonEmpty)
          Some(many(line(numeric.mkString("reserved ", ", ", ";")), emptyLine))
        else None
      ),
      maybe(
        if (names.nonEmpty) Some(many(line(names.mkString("reserved ", ", ", ";")), emptyLine))
        else None
      )
    )
  }

  private def renderField(field: Field, isOneOf: Boolean = false): Text =
    if (field == ProtoIR.excludedField) many()
    else {
      val ty       = renderType(field.ty)
      val opts     = renderInlineOptions(field.options)
      val optional = field.ty match {
        case _: Type.PrimitiveType | _: Type.EnumRefType | _: Type.RefType =>
          if (field.optional && !isOneOf) "optional " else ""
        case _: Type.ListType | _: Type.MapType                            =>
          ""
      }

      field.comment match {
        case Some(c) if c.contains("\n") =>
          // Multiline comment - render on previous line(s)
          many(
            renderComment(c),
            line(s"$optional$ty ${field.name} = ${field.number}$opts;")
          )
        case Some(c)                     =>
          // Single-line comment - render inline
          line(s"$optional$ty ${field.name} = ${field.number}$opts; // $c")
        case None                        =>
          // No comment
          line(s"$optional$ty ${field.name} = ${field.number}$opts;")
      }
    }

  private def renderService(service: Service): Text = {
    val commentLine = service.comment.map(renderComment).getOrElse(many())
    val hasRpcs     = service.rpcs.nonEmpty
    val hasContent  = service.options.nonEmpty || hasRpcs
    val optionsSep  = if (service.options.nonEmpty && hasRpcs) emptyLine else many()
    if (hasContent) {
      many(
        commentLine,
        line(s"service ${service.name} {"),
        indent(renderStatementOptions(service.options)),
        optionsSep,
        indent(service.rpcs.map(renderRpc)),
        line("}")
      )
    } else {
      many(
        commentLine,
        line(s"service ${service.name} {}")
      )
    }
  }

  private def renderRpc(rpc: Rpc): Text = {
    val commentLine  = rpc.comment.map(renderComment).getOrElse(many())
    val requestType  = if (rpc.streamingRequest) s"stream ${rpc.request.fqn.render}" else rpc.request.fqn.render
    val responseType = if (rpc.streamingResponse) s"stream ${rpc.response.fqn.render}" else rpc.response.fqn.render
    if (rpc.options.nonEmpty)
      many(
        commentLine,
        line(s"rpc ${rpc.name} ($requestType) returns ($responseType) {"),
        indent(renderStatementOptions(rpc.options)),
        line("}")
      )
    else
      many(
        commentLine,
        line(s"rpc ${rpc.name} ($requestType) returns ($responseType) {}")
      )
  }

  private[proteus] def renderReservedValue(r: Reserved): String = r match {
    case Reserved.Number(n)   => n.toString
    case Reserved.Name(name)  => s""""$name""""
    case Reserved.Range(s, e) => s"$s to $e"
  }

  private[proteus] def renderType(ty: Type): String = {
    import Type._
    ty match {
      case Double                      => "double"
      case Float                       => "float"
      case Int32                       => "int32"
      case Int64                       => "int64"
      case Uint32                      => "uint32"
      case Uint64                      => "uint64"
      case Sint32                      => "sint32"
      case Sint64                      => "sint64"
      case Fixed32                     => "fixed32"
      case Fixed64                     => "fixed64"
      case Sfixed32                    => "sfixed32"
      case Sfixed64                    => "sfixed64"
      case Bool                        => "bool"
      case String                      => "string"
      case Bytes                       => "bytes"
      case MapType(keyType, valueType) =>
        s"map<${renderType(keyType)}, ${renderType(valueType)}>"
      case ListType(valueType)         => s"repeated ${renderType(valueType)}"
      case r: RefType                  => r.name
      case r: EnumRefType              => r.name
    }
  }

  private def statement(string: String): Text =
    line(s"$string;")
}
