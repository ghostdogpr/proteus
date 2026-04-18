package proteus

import fastparse.*
import fastparse.NoWhitespace.*

import proteus.ProtoIR.*

/**
  * Parser for Protocol Buffer (proto3) definition files.
  *
  * Parses `.proto` file content into a [[ProtoIR.CompilationUnit]].
  *
  * Only proto3 syntax is supported; proto2 files will be rejected.
  *
  * {{{
  * val input = """syntax = "proto3";
  *               |package example;
  *               |message Foo {
  *               |    string name = 1;
  *               |}""".stripMargin
  *
  * ProtoParser.parse(input) match {
  *   case Right(compilationUnit) => // use compilationUnit
  *   case Left(error)            => // handle parse error
  * }
  * }}}
  */
object ProtoParser {

  /**
    * Parses a proto3 definition file into a [[ProtoIR.CompilationUnit]].
    *
    * @param input the full content of a `.proto` file.
    * @return a [[Right]] containing the parsed [[ProtoIR.CompilationUnit]] on success,
    *         or a [[Left]] containing an error message with the failure location on failure.
    */
  def parse(input: String): Either[String, CompilationUnit] =
    fastparse.parse(input, compilationUnit(_)) match {
      case Parsed.Success(value, _)        => Right(value)
      case Parsed.Failure(_, index, extra) =>
        val trace = extra.trace()
        Left(s"Parse error at index $index: ${trace.longMsg}")
    }

  private val Proto3MinFieldNumber: Long          = 1L
  private val Proto3MaxFieldNumber: Int           = 536870911
  private val ProtoReservedFieldNumberStart: Long = 19000L
  private val ProtoReservedFieldNumberEnd: Long   = 19999L
  private val Proto3MaxFieldNumberLong: Long      = Proto3MaxFieldNumber.toLong
  private val Int32MinValue: Long                 = Int.MinValue.toLong
  private val Int32MaxValue: Long                 = Int.MaxValue.toLong

  private def whitespace[$: P]: P[Unit]      = P(CharsWhileIn(" \t\r\n", 0))
  private def lineComment[$: P]: P[String]   = P("//" ~ CharsWhile(_ != '\n', 0).! ~ ("\n" | End))
  private def inlineComment[$: P]: P[String] = P(CharsWhileIn(" \t", 0) ~ lineComment)
  private def blockComment[$: P]: P[String]  = P("/*" ~ (!"*/" ~ AnyChar).rep.! ~ "*/")
  private def comment[$: P]: P[String]       = P(lineComment | blockComment)
  private def padding[$: P]: P[Unit]         = whitespace
  private def ws[$: P]: P[Unit]              = P((CharsWhileIn(" \t\r\n") | comment.map(_ => ())).rep)

  private def commentBlock[$: P]: P[Option[String]] =
    P((whitespace ~ comment).rep).map { comments =>
      if (comments.isEmpty) None
      else Some(comments.iterator.map(normalizeComment).mkString("\n"))
    }

  private def normalizeComment(raw: String): String =
    if (raw.contains("\n"))
      raw.split("\n").iterator.map(_.trim.stripPrefix("*").trim).filter(_.nonEmpty).mkString("\n")
    else
      raw.trim

  private def mergeComments(preComment: Option[String], inlineComment: Option[String]): Option[String] =
    (preComment, inlineComment.map(_.trim)) match {
      case (Some(pre), Some(inline)) => Some(s"$pre\n$inline")
      case (Some(pre), None)         => Some(pre)
      case (None, Some(inline))      => Some(inline)
      case (None, None)              => None
    }

  private def letter[$: P]: P[Unit]             = P(CharIn("a-zA-Z"))
  private def decimalDigit[$: P]: P[Unit]       = P(CharIn("0-9"))
  private def identChar[$: P]: P[Unit]          = P(letter | decimalDigit | "_")
  private def ident[$: P]: P[String]            = P((letter | "_") ~ identChar.rep).!
  private def fullIdent[$: P]: P[String]        = P(ident ~ ("." ~ ident).rep).!
  private def keyword[$: P](s: String): P[Unit] = P(s ~ !identChar)
  private def semi[$: P]: P[Unit]               = P(ws ~ ";")
  private def emptyStatement[$: P]: P[Unit]     = P(";")

  private def intLit[$: P]: P[Long] = P(
    ("-".!.? ~ (
      ("0" ~ CharIn("xX") ~ CharsWhileIn("0-9a-fA-F").!).map(s => java.lang.Long.parseLong(s, 16)) |
        ("0" ~ CharsWhileIn("0-7")).!.map(s => java.lang.Long.parseLong(s, 8)) |
        CharsWhileIn("0-9").!.map(_.toLong)
    )).map {
      case (Some(_), n) => -n
      case (_, n)       => n
    }
  )

  private def int32Lit[$: P]: P[Int] =
    intLit.filter(n => n >= Int32MinValue && n <= Int32MaxValue).map(_.toInt)

  private def isValidFieldNumber(number: Long): Boolean =
    number >= Proto3MinFieldNumber &&
      number <= Proto3MaxFieldNumberLong &&
      (number < ProtoReservedFieldNumberStart || number > ProtoReservedFieldNumberEnd)

  private def fieldNumber[$: P]: P[Int] =
    intLit.filter(isValidFieldNumber).map(_.toInt)

  private def floatLit[$: P]: P[Double] = P(
    CharIn("+\\-").!.? ~ (
      "inf".!.map(_ => Double.PositiveInfinity) |
        "nan".!.map(_ => Double.NaN) |
        (CharsWhileIn("0-9") ~ "." ~ CharsWhileIn("0-9", 0) ~ (CharIn("eE") ~ CharIn("+\\-").? ~ CharsWhileIn("0-9")).?).!.map(_.toDouble) |
        (CharsWhileIn("0-9") ~ CharIn("eE") ~ CharIn("+\\-").? ~ CharsWhileIn("0-9")).!.map(_.toDouble)
    )
  ).map {
    case (Some("-"), value) if !value.isNaN => -value
    case (_, value)                         => value
  }

  private def strLit[$: P]: P[String] = {
    val dq = "\""
    val sq = "'"
    val bs = "\\"
    P(
      (dq ~ ((bs ~ AnyChar) | (!CharPred(c => c == '"' || c == '\\') ~ AnyChar)).rep.! ~ dq) |
        (sq ~ ((bs ~ AnyChar) | (!CharPred(c => c == '\'' || c == '\\') ~ AnyChar)).rep.! ~ sq)
    )
  }

  private def boolLit[$: P]: P[Boolean] = P(
    keyword("true").map(_ => true) | keyword("false").map(_ => false)
  )

  private def primitiveType[$: P]: P[Type] = P(
    keyword("double").map(_ => Type.Double) |
      keyword("float").map(_ => Type.Float) |
      keyword("sfixed32").map(_ => Type.Sfixed32) |
      keyword("sfixed64").map(_ => Type.Sfixed64) |
      keyword("sint32").map(_ => Type.Sint32) |
      keyword("sint64").map(_ => Type.Sint64) |
      keyword("fixed32").map(_ => Type.Fixed32) |
      keyword("fixed64").map(_ => Type.Fixed64) |
      keyword("int32").map(_ => Type.Int32) |
      keyword("int64").map(_ => Type.Int64) |
      keyword("uint32").map(_ => Type.Uint32) |
      keyword("uint64").map(_ => Type.Uint64) |
      keyword("bool").map(_ => Type.Bool) |
      keyword("string").map(_ => Type.String) |
      keyword("bytes").map(_ => Type.Bytes)
  )

  private val validMapKeyTypes: Set[Type] =
    Set(
      Type.Int32,
      Type.Int64,
      Type.Uint32,
      Type.Uint64,
      Type.Sint32,
      Type.Sint64,
      Type.Fixed32,
      Type.Fixed64,
      Type.Sfixed32,
      Type.Sfixed64,
      Type.Bool,
      Type.String
    )

  private def mapKeyType[$: P]: P[Type] = primitiveType.filter(validMapKeyTypes.contains)

  private def mapType[$: P]: P[Type.MapType] = P(
    "map" ~ ws ~ "<" ~ ws ~ mapKeyType ~ ws ~ "," ~ ws ~ fieldType ~ ws ~ ">"
  ).map { case (k, v) => Type.MapType(k, v) }

  private def messageType[$: P]: P[String] = P(
    (".".? ~ fullIdent).!
  )

  private def fieldType[$: P]: P[Type] = P(
    primitiveType | mapType | messageType.map(Type.RefType(_))
  )

  private def optionName[$: P]: P[OptionName] = P(
    ("(" ~ ws ~ (".".? ~ fullIdent).! ~ ws ~ ")" ~ ("." ~ ident).rep).map { case (fi, path) =>
      OptionName.Extension(fi, path.toList)
    } |
      ident.map(OptionName.BuiltIn(_))
  )

  private def scalarOptionVal[$: P]: P[OptionVal] = P(
    boolLit.map(OptionVal.BoolLit(_)) |
      strLit.map(OptionVal.StringLit(_)) |
      floatLit.map(OptionVal.FloatLit(_)) |
      intLit.map(OptionVal.IntLit(_)) |
      fullIdent.map(OptionVal.Identifier(_))
  )

  private def listLiteral[$: P]: P[OptionVal] = P(
    "[" ~ ws ~ (
      messageValueLiteral.rep(1, sep = ws ~ "," ~ ws).map(values => OptionVal.MessageList(values.toList): OptionVal) |
        scalarOptionVal.rep(sep = ws ~ "," ~ ws).map(values => OptionVal.ScalarList(values.toList): OptionVal)
    ) ~ ws ~ "]"
  )

  private def optionVal[$: P]: P[OptionVal] = P(
    messageValueLiteral |
      listLiteral |
      scalarOptionVal
  )

  private def messageValueLiteral[$: P]: P[OptionVal.MessageValue] = P(
    "{" ~ ws ~ messageFieldEntry.rep(sep = ws ~ ",".? ~ ws) ~ ws ~ "}"
  ).map(fields => OptionVal.MessageValue(fields.toList))

  private def messageFieldEntry[$: P]: P[(String, OptionVal)] = P(
    ident ~ ws ~ ":" ~ ws ~ optionVal
  )

  private def optionValuePair[$: P]: P[OptionValue] = P(
    optionName ~ ws ~ "=" ~ ws ~ optionVal
  ).map { case (n, v) => OptionValue(n, v) }

  private def inlineOptions[$: P]: P[List[OptionValue]] = P(
    ("[" ~ ws ~ optionValuePair.rep(1, sep = ws ~ "," ~ ws) ~ ws ~ "]").map(_.toList) |
      Pass(Nil)
  )

  private def optionStatement[$: P]: P[OptionValue] = P(
    ws ~ "option" ~ ws ~ optionValuePair ~ semi
  )

  private def repeatedField[$: P]: P[Field] = P(
    keyword("repeated") ~ ws ~ fieldType ~ ws ~ ident ~ ws ~ "=" ~ ws ~ fieldNumber ~ ws ~ inlineOptions ~ semi
  ).map { case (ty, name, number, opts) =>
    Field(Type.ListType(ty), name, number, optional = false, comment = None, options = opts)
  }

  private def singularField[$: P]: P[Field] = P(
    keyword("optional").!.?.map(_.isDefined) ~ ws ~ fieldType ~ ws ~ ident ~ ws ~ "=" ~ ws ~ fieldNumber ~ ws ~ inlineOptions ~ semi
  ).map { case (opt, ty, name, number, opts) =>
    Field(ty, name, number, optional = opt, comment = None, options = opts)
  }

  private def field[$: P]: P[Field] = P(
    commentBlock ~ ws ~ (repeatedField | singularField) ~ inlineComment.?
  ).map { case (preComment, parsedField, maybeInlineComment) =>
    parsedField.copy(comment = mergeComments(preComment, maybeInlineComment))
  }

  private def oneOfField[$: P]: P[Field] = P(
    commentBlock ~ ws ~
      fieldType ~ ws ~ ident ~ ws ~ "=" ~ ws ~ fieldNumber ~ ws ~ inlineOptions ~ semi
      ~ inlineComment.?
  ).map { case (preComment, ty, name, number, opts, maybeInlineComment) =>
    Field(ty, name, number, optional = false, comment = mergeComments(preComment, maybeInlineComment), options = opts)
  }

  private def oneOfBody[$: P]: P[(List[OptionValue], List[Field])] = P(
    (padding ~ (emptyStatement.map(_ => Left(None)) | optionStatement.map(o => Left(Some(o))) | oneOfField.map(f => Right(f)))).rep
  ).map { elements =>
    val opts   = List.newBuilder[OptionValue]
    val fields = List.newBuilder[Field]
    elements.foreach {
      case Left(Some(option)) => opts += option
      case Right(field)       => fields += field
      case _                  => ()
    }
    (opts.result(), fields.result())
  }

  private def oneOf[$: P]: P[OneOf] = P(
    commentBlock ~ ws ~ "oneof" ~ ws ~ ident ~ ws ~ "{" ~ oneOfBody ~ ws ~ "}"
  ).map { case (comment, name, (options, fields)) =>
    OneOf(name, fields, comment = comment, options = options)
  }

  private def reservedRange[$: P]: P[Reserved] = P(
    fieldNumber ~ (ws ~ "to" ~ ws ~ (keyword("max").map(_ => Proto3MaxFieldNumber) | fieldNumber)).?
  ).map {
    case (start, Some(end)) => Reserved.Range(start, end)
    case (n, None)          => Reserved.Number(n)
  }.filter {
    case Reserved.Range(start, end) => start <= end
    case Reserved.Number(_)         => true
    case Reserved.Name(_)           => true
  }

  private def reservedNames[$: P]: P[List[Reserved]] = P(
    strLit.rep(1, sep = ws ~ "," ~ ws)
  ).map(_.toList.map(Reserved.Name(_)))

  private def reservedNumbers[$: P]: P[List[Reserved]] = P(
    reservedRange.rep(1, sep = ws ~ "," ~ ws)
  ).map(_.toList)

  private def reserved[$: P]: P[List[Reserved]] = P(
    commentBlock ~ ws ~ "reserved" ~ ws ~ (reservedNames | reservedNumbers) ~ semi
  ).map { case (_, res) => res }

  private def enumField[$: P]: P[EnumValue] = P(
    commentBlock ~ ws ~ ident ~ ws ~ "=" ~ ws ~ int32Lit ~ ws ~ inlineOptions ~ semi ~ inlineComment.?
  ).map { case (preComment, name, value, opts, inlineComment) =>
    val comment = mergeComments(preComment, inlineComment)
    EnumValue(name, value, comment, opts)
  }

  private type EnumBodyElement = Option[Either[Either[OptionValue, List[Reserved]], EnumValue]]

  private def enumBody[$: P]: P[(List[OptionValue], List[Reserved], List[EnumValue])] = P(
    (padding ~ (
      emptyStatement.map(_ => None: EnumBodyElement) |
        optionStatement.map(o => Some(Left(Left(o))): EnumBodyElement) |
        reserved.map(r => Some(Left(Right(r))): EnumBodyElement) |
        enumField.map(v => Some(Right(v)): EnumBodyElement)
    )).rep
  ).map { elements =>
    val opts   = List.newBuilder[OptionValue]
    val res    = List.newBuilder[Reserved]
    val values = List.newBuilder[EnumValue]
    elements.foreach {
      case Some(Left(Left(o)))  => opts += o
      case Some(Left(Right(r))) => res ++= r
      case Some(Right(v))       => values += v
      case None                 => ()
    }
    (opts.result(), res.result(), values.result())
  }

  private def enumDef[$: P]: P[Enum] = P(
    commentBlock ~ ws ~ "enum" ~ ws ~ ident ~ ws ~ "{" ~ enumBody ~ ws ~ "}"
  ).map { case (comment, name, (opts, reserved, values)) =>
    Enum(name, values, reserved, comment, opts)
  }

  private def messageElement[$: P]: P[Either[List[Reserved], MessageElement]] = P(
    reserved.map(Left(_)) |
      oneOf.map(o => Right(MessageElement.OneOfElement(o))) |
      messageDef.map(m => Right(MessageElement.NestedMessageElement(m.copy(nested = true)))) |
      enumDef.map(e => Right(MessageElement.NestedEnumElement(e.copy(nested = true)))) |
      field.map(f => Right(MessageElement.FieldElement(f)))
  )

  private type MessageBodyElement = Option[Either[OptionValue, Either[List[Reserved], MessageElement]]]

  private def messageBody[$: P]: P[(List[OptionValue], List[Reserved], List[MessageElement])] = P(
    (padding ~ (
      emptyStatement.map(_ => None: MessageBodyElement) |
        optionStatement.map(o => Some(Left(o)): MessageBodyElement) |
        messageElement.map(e => Some(Right(e)): MessageBodyElement)
    )).rep
  ).map { elements =>
    val opts  = List.newBuilder[OptionValue]
    val res   = List.newBuilder[Reserved]
    val elems = List.newBuilder[MessageElement]
    elements.foreach {
      case Some(Left(o))            => opts += o
      case Some(Right(Left(r)))     => res ++= r
      case Some(Right(Right(elem))) => elems += elem
      case None                     => ()
    }
    (opts.result(), res.result(), elems.result())
  }

  private def messageDef[$: P]: P[Message] = P(
    commentBlock ~ ws ~ "message" ~ ws ~ ident ~ ws ~ "{" ~ messageBody ~ ws ~ "}"
  ).map { case (comment, name, (opts, reserved, elements)) =>
    Message(name, elements, reserved, comment, opts)
  }

  private def rpcBody[$: P]: P[List[OptionValue]] = P(
    "{" ~ (ws ~ (emptyStatement.map(_ => None) | optionStatement.map(Some(_)))).rep ~ ws ~ "}"
  ).map(_.collect { case Some(o) => o }.toList)

  private def rpcDef[$: P]: P[Rpc] = P(
    commentBlock ~ ws ~ "rpc" ~ ws ~ ident ~ ws ~
      "(" ~ ws ~ keyword("stream").!.?.map(_.isDefined) ~ ws ~ fqn ~ ws ~ ")" ~ ws ~
      "returns" ~ ws ~ "(" ~ ws ~ keyword("stream").!.?.map(_.isDefined) ~ ws ~ fqn ~ ws ~ ")" ~ ws ~
      (rpcBody | semi.map(_ => Nil))
  ).map { case (comment, name, streamReq, reqFqn, streamResp, respFqn, options) =>
    Rpc(name, RpcMessage(reqFqn), RpcMessage(respFqn), streamReq, streamResp, comment, options)
  }

  private def fqn[$: P]: P[Fqn] = P(
    ".".!.? ~ ident ~ ("." ~ ident).rep
  ).map { case (dot, head, tail) =>
    val parts = head :: tail.toList
    if (parts.length == 1 && dot.isEmpty) Fqn(None, parts.head)
    else {
      val pkg = if (dot.isDefined) "" :: parts.init else parts.init
      Fqn(Some(pkg), parts.last)
    }
  }

  private def serviceBody[$: P]: P[(List[OptionValue], List[Rpc])] = P(
    (padding ~ (optionStatement.map(o => Left(o)) | emptyStatement.map(_ => Right(None)) | rpcDef.map(r => Right(Some(r))))).rep
  ).map { elements =>
    val opts = List.newBuilder[OptionValue]
    val rpcs = List.newBuilder[Rpc]
    elements.foreach {
      case Left(option)     => opts += option
      case Right(Some(rpc)) => rpcs += rpc
      case Right(None)      => ()
    }
    (opts.result(), rpcs.result())
  }

  private def serviceDef[$: P]: P[Service] = P(
    commentBlock ~ ws ~ "service" ~ ws ~ ident ~ ws ~ "{" ~ serviceBody ~ padding ~ "}"
  ).map { case (comment, name, (options, rpcs)) =>
    Service(name, rpcs, comment, options)
  }

  private def syntaxStatement[$: P]: P[Unit] = P(
    "syntax" ~ ws ~ "=" ~ ws ~ strLit.filter(_ == "proto3") ~ semi
  ).map(_ => ())

  private def packageStatement[$: P]: P[String] = P(
    ws ~ "package" ~ ws ~ fullIdent ~ semi
  )

  private def importStatement[$: P]: P[Statement.ImportStatement] = P(
    ws ~ "import" ~ ws ~ ("weak".! | "public".!).? ~ ws ~ strLit ~ semi
  ).map { case (modifier, path) =>
    Statement.ImportStatement(path, modifier)
  }

  private def topLevelOption[$: P]: P[TopLevelOption] = P(
    ws ~ "option" ~ ws ~ optionName ~ ws ~ "=" ~ ws ~ optionVal ~ semi
  ).map { case (name, value) => TopLevelOption(name, value) }

  private def topLevelDef[$: P]: P[TopLevelDef] = P(
    messageDef.map(TopLevelDef.MessageDef(_)) |
      enumDef.map(TopLevelDef.EnumDef(_)) |
      serviceDef.map(TopLevelDef.ServiceDef(_))
  )

  // `extend <type> { ... }` blocks are parsed but not tracked: they define custom proto options
  // (e.g. extending google.protobuf.FieldOptions) rather than schema-level types users would diff.
  private def extendDef[$: P]: P[Unit] = P(
    commentBlock ~ ws ~ "extend" ~ ws ~ (".".? ~ fullIdent).! ~ ws ~ "{" ~ messageBody ~ ws ~ "}"
  ).map(_ => ())

  private def topLevelElement[$: P]: P[Option[Either[Either[Statement.ImportStatement, TopLevelOption], TopLevelDef]]] = P(
    emptyStatement.map(_ => None) |
      extendDef.map(_ => None) |
      importStatement.map(i => Some(Left(Left(i)))) |
      topLevelOption.map(o => Some(Left(Right(o)))) |
      topLevelDef.map(d => Some(Right(d)))
  )

  private def compilationUnit[$: P]: P[CompilationUnit] = P(
    ws ~ syntaxStatement.?.map(_ => ()) ~ padding ~ packageStatement.? ~ (padding ~ topLevelElement).rep ~ ws ~ End
  ).map { case (pkg, elements) =>
    val imports   = List.newBuilder[Statement]
    val options   = List.newBuilder[TopLevelOption]
    val topLevels = List.newBuilder[Statement]
    elements.foreach {
      case Some(Left(Left(i)))  => imports += i
      case Some(Left(Right(o))) => options += o
      case Some(Right(tld))     => topLevels += Statement.TopLevelStatement(tld)
      case None                 => ()
    }
    CompilationUnit(pkg, imports.result() ++ topLevels.result(), options.result())
  }
}
