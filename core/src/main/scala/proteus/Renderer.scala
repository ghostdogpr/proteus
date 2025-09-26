package proteus

import proteus.ProtoIR.*
import proteus.internal.Text
import proteus.internal.Text.*

object Renderer {
  def render(compilationUnit: CompilationUnit): String = {
    def renderPackageName(packageName: String): Text =
      statement(s"package $packageName")

    def renderOption(opt: TopLevelOption): Text =
      statement(s"""option ${opt.key} = "${opt.value}"""")

    def renderStatement(st: Statement): Text =
      st match {
        case Statement.ImportStatement(path)  => statement(s"""import "$path"""")
        case Statement.TopLevelStatement(tld) => renderTopLevelDef(tld)
      }

    def renderTopLevelDef(tld: TopLevelDef): Text =
      tld match {
        case TopLevelDef.MessageDef(message) => renderMessage(message)
        case TopLevelDef.EnumDef(enumDef)    => renderEnum(enumDef)
        case TopLevelDef.ServiceDef(service) => renderService(service)
      }

    def renderEnumElement(enumValue: EnumValue): Text =
      enumValue match {
        case EnumValue(identifier, intvalue) =>
          line(s"$identifier = $intvalue;")
      }

    def renderEnum(enumeration: Enum): Text = {
      val hasContent  = enumeration.reserved.nonEmpty || enumeration.values.nonEmpty
      val commentLine = enumeration.comment.map(c => line(s"// $c")).getOrElse(many())
      if (hasContent) {
        many(
          commentLine,
          line(s"enum ${enumeration.name} {"),
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

    def renderMessage(message: Message): Text = {
      val hasContent  = message.reserved.nonEmpty || message.elements.nonEmpty
      val commentLine = message.comment.map(c => line(s"// $c")).getOrElse(many())
      if (hasContent) {
        many(
          commentLine,
          line(s"message ${message.name} {"),
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

    def renderOneof(oneof: Oneof): Text =
      if (oneof.fields.nonEmpty) {
        many(
          line(s"oneof ${oneof.name} {"),
          indent(oneof.fields.map(renderField(_, isOneof = true))),
          line("}")
        )
      } else line(s"oneof ${oneof.name} {}")

    def renderMessageElement(element: MessageElement): Text =
      element match {
        case MessageElement.FieldElement(field)           => renderField(field)
        case MessageElement.EnumDefElement(enumDef)       => renderEnum(enumDef)
        case MessageElement.OneofElement(oneof)           => renderOneof(oneof)
        case MessageElement.NestedMessageElement(message) => many(renderMessage(message), emptyLine)
        case MessageElement.NestedEnumElement(enumDef)    => many(renderEnum(enumDef), emptyLine)
      }

    def renderReserved(reserved: List[Reserved]): Text = {
      val numeric = reserved.collect {
        case Reserved.Number(number)    => s"$number"
        case Reserved.Range(start, end) => s"$start to $end"
      }
      val names   = reserved.collect { case Reserved.Name(name) =>
        s""""$name""""
      }

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

    def renderField(field: Field, isOneof: Boolean = false): Text = {
      val ty         = renderType(field.ty)
      val deprecated = if (field.deprecated) " [deprecated = true]" else ""
      val optional   = field.ty match {
        case _: Type.PrimitiveType | _: Type.EnumRefType | _: Type.RefType =>
          if (field.optional && !isOneof) "optional " else ""
        case _: Type.ListType | _: Type.MapType                            =>
          ""
      }
      val comment    = field.comment.map(c => s" // $c").getOrElse("")
      line(s"$optional$ty ${field.name} = ${field.number}$deprecated;$comment")
    }

    def renderService(service: Service): Text = {
      val commentLine = service.comment.map(c => line(s"// $c")).getOrElse(many())
      val hasContent  = service.rpcs.nonEmpty
      if (hasContent) {
        many(
          commentLine,
          line(s"service ${service.name} {"),
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

    def renderRpc(rpc: Rpc): Text = {
      val commentLine  = rpc.comment.map(c => line(s"// $c")).getOrElse(many())
      val requestType  = if (rpc.streamingRequest) s"stream ${rpc.request.fqn.render}" else rpc.request.fqn.render
      val responseType = if (rpc.streamingResponse) s"stream ${rpc.response.fqn.render}" else rpc.response.fqn.render
      many(
        commentLine,
        line(s"rpc ${rpc.name} ($requestType) returns ($responseType) {}")
      )
    }

    def renderType(ty: Type): String = {
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
        case RefType(fqn)                => fqn.render
        case EnumRefType(fqn)            => fqn.render
      }
    }

    def statement(string: String): Text =
      line(s"$string;")

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
}
