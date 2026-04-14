package proteus

// inspired by https://github.com/disneystreaming/smithy-translate/blob/main/modules/proto/src/smithytranslate/proto3/internals/ProtoIR.scala
object ProtoIR {

  final case class CompilationUnit(packageName: Option[String], statements: List[Statement], options: List[TopLevelOption])

  final case class TopLevelOption(name: OptionName, value: OptionVal)
  object TopLevelOption {
    def apply(key: String, value: String): TopLevelOption =
      TopLevelOption(OptionName.BuiltIn(key), OptionVal.StringLit(value))

    def identifier(key: String, value: String): TopLevelOption =
      TopLevelOption(OptionName.BuiltIn(key), OptionVal.Identifier(value))
  }

  sealed trait OptionVal
  object OptionVal {
    final case class Identifier(value: String)                                    extends OptionVal
    final case class IntLit(value: Long)                                          extends OptionVal
    final case class FloatLit(value: Double)                                      extends OptionVal
    final case class StringLit(value: String)                                     extends OptionVal
    final case class BoolLit(value: Boolean)                                      extends OptionVal
    final case class MessageValue(fields: List[(String, OptionVal)] = List.empty) extends OptionVal
    final case class ScalarList(values: List[OptionVal])                          extends OptionVal
    final case class MessageList(values: List[MessageValue])                      extends OptionVal
  }

  sealed trait OptionName {
    def render: String
  }
  object OptionName       {
    final case class BuiltIn(name: String)                                         extends OptionName {
      def render: String = name
    }
    final case class Extension(fullIdent: String, path: List[String] = List.empty) extends OptionName {
      def render: String =
        if (path.isEmpty) s"($fullIdent)"
        else s"($fullIdent).${path.mkString(".")}"
    }
  }

  final case class OptionValue(name: OptionName, value: OptionVal)
  object OptionValue {
    val deprecated: OptionValue = OptionValue(OptionName.BuiltIn("deprecated"), OptionVal.BoolLit(true))

    def deprecatedOpts(deprecated: Boolean): List[OptionValue] =
      if (deprecated) List(OptionValue.deprecated) else List.empty

    def identifier(name: OptionName, value: String): OptionValue = OptionValue(name, OptionVal.Identifier(value))
    def intLit(name: OptionName, value: Long): OptionValue       = OptionValue(name, OptionVal.IntLit(value))
    def floatLit(name: OptionName, value: Double): OptionValue   = OptionValue(name, OptionVal.FloatLit(value))
    def stringLit(name: OptionName, value: String): OptionValue  = OptionValue(name, OptionVal.StringLit(value))
    def boolLit(name: OptionName, value: Boolean): OptionValue   = OptionValue(name, OptionVal.BoolLit(value))
  }

  sealed trait Statement
  object Statement {
    final case class ImportStatement(path: String, modifier: Option[String] = None) extends Statement
    final case class TopLevelStatement(s: TopLevelDef)                              extends Statement
  }

  sealed trait TopLevelDef {
    def name: String
    def collectTypeReferences: Set[String]
  }
  object TopLevelDef       {
    final case class MessageDef(message: Message) extends TopLevelDef {
      def name: String                       = message.name
      def collectTypeReferences: Set[String] = message.collectTypeReferences + name
    }
    final case class EnumDef(enumValue: Enum)     extends TopLevelDef {
      def name: String                       = enumValue.name
      def collectTypeReferences: Set[String] = Set(name)
    }
    final case class ServiceDef(service: Service) extends TopLevelDef {
      def name: String                       = service.name
      def collectTypeReferences: Set[String] = service.rpcs.flatMap(rpc => Set(rpc.request.fqn.name, rpc.response.fqn.name)).toSet
    }
  }

  final case class Message(
    name: String,
    elements: List[MessageElement],
    reserved: List[Reserved],
    comment: Option[String] = None,
    options: List[OptionValue] = List.empty,
    nested: Boolean = false
  ) {
    lazy val collectTypeReferences: Set[String] = elements.toSet.flatMap(_.collectTypeReferences)
  }

  sealed trait MessageElement {
    def collectTypeReferences: Set[String]
  }
  object MessageElement       {
    final case class FieldElement(field: Field)             extends MessageElement {
      def collectTypeReferences: Set[String] = field.ty.collectTypeReferences
    }
    final case class OneOfElement(oneOf: OneOf)             extends MessageElement {
      def collectTypeReferences: Set[String] = oneOf.fields.flatMap(_.ty.collectTypeReferences).toSet
    }
    final case class NestedMessageElement(message: Message) extends MessageElement {
      def collectTypeReferences: Set[String] = message.collectTypeReferences
    }
    final case class NestedEnumElement(enumValue: Enum)     extends MessageElement {
      def collectTypeReferences: Set[String] = Set.empty
    }
  }

  final case class OneOf(name: String, fields: List[Field], comment: Option[String] = None, options: List[OptionValue] = List.empty)

  final case class Field(
    ty: Type,
    name: String,
    number: Int,
    optional: Boolean,
    comment: Option[String],
    options: List[OptionValue] = List.empty
  )

  val excludedField: Field                   = Field(Type.Bool, "", 0, optional = false, None)
  val excludedMessageElement: MessageElement = MessageElement.FieldElement(excludedField)

  sealed trait Reserved
  object Reserved {
    final case class Number(number: Int)         extends Reserved
    final case class Name(name: String)          extends Reserved
    final case class Range(start: Int, end: Int) extends Reserved
  }

  final case class EnumValue(name: String, intValue: Int, comment: Option[String] = None, options: List[OptionValue] = List.empty)
  final case class Enum(
    name: String,
    values: List[EnumValue],
    reserved: List[Reserved],
    comment: Option[String] = None,
    options: List[OptionValue] = List.empty,
    nested: Boolean = false
  )

  final case class Service(name: String, rpcs: List[Rpc], comment: Option[String] = None, options: List[OptionValue] = List.empty)

  final case class RpcMessage(fqn: Fqn)

  final case class Rpc(
    name: String,
    request: RpcMessage,
    response: RpcMessage,
    streamingRequest: Boolean,
    streamingResponse: Boolean,
    comment: Option[String],
    options: List[OptionValue] = List.empty
  )

  sealed trait Type { self =>
    def collectTypeReferences: Set[String] =
      self match {
        case _: ProtoIR.Type.PrimitiveType            => Set.empty
        case ProtoIR.Type.MapType(keyType, valueType) => keyType.collectTypeReferences ++ valueType.collectTypeReferences
        case ProtoIR.Type.ListType(valueType)         => valueType.collectTypeReferences
        case ProtoIR.Type.RefType(name)               => Set(name)
        case ProtoIR.Type.EnumRefType(name)           => Set(name)
      }
  }
  object Type       {
    sealed trait PrimitiveType extends Type

    case object Double   extends PrimitiveType
    case object Float    extends PrimitiveType
    case object Int32    extends PrimitiveType
    case object Int64    extends PrimitiveType
    case object Uint32   extends PrimitiveType
    case object Uint64   extends PrimitiveType
    case object Sint32   extends PrimitiveType
    case object Sint64   extends PrimitiveType
    case object Fixed32  extends PrimitiveType
    case object Fixed64  extends PrimitiveType
    case object Sfixed32 extends PrimitiveType
    case object Sfixed64 extends PrimitiveType
    case object Bool     extends PrimitiveType
    case object String   extends PrimitiveType
    case object Bytes    extends PrimitiveType

    final case class MapType(keyType: Type, valueType: Type) extends Type
    final case class ListType(valueType: Type)               extends Type
    final case class RefType(name: String)                   extends Type
    final case class EnumRefType(name: String)               extends Type
  }

  final case class Fqn(packageName: Option[List[String]], name: String) {
    def render: String =
      packageName.map(_.mkString(".")).map(_ + ".").getOrElse("") + name
  }
}
