package proteus

// inspired by https://github.com/disneystreaming/smithy-translate/blob/main/modules/proto/src/smithytranslate/proto3/internals/ProtoIR.scala
object ProtoIR {

  final case class CompilationUnit(packageName: Option[String], statements: List[Statement], options: List[TopLevelOption])

  final case class TopLevelOption(key: String, value: String)

  sealed trait Statement
  object Statement {
    final case class ImportStatement(path: String)     extends Statement
    final case class TopLevelStatement(s: TopLevelDef) extends Statement
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

  final case class Message(name: String, elements: List[MessageElement], reserved: List[Reserved], comment: Option[String] = None) {
    lazy val collectTypeReferences: Set[String] = elements.toSet.flatMap(_.collectTypeReferences)
  }

  sealed trait MessageElement {
    def collectTypeReferences: Set[String]
  }
  object MessageElement       {
    final case class FieldElement(field: Field)             extends MessageElement {
      def collectTypeReferences: Set[String] = field.ty.collectTypeReferences
    }
    final case class EnumDefElement(enumValue: Enum)        extends MessageElement {
      def collectTypeReferences: Set[String] = Set.empty
    }
    final case class OneofElement(oneof: Oneof)             extends MessageElement {
      def collectTypeReferences: Set[String] = oneof.fields.flatMap(_.ty.collectTypeReferences).toSet
    }
    final case class NestedMessageElement(message: Message) extends MessageElement {
      def collectTypeReferences: Set[String] = message.collectTypeReferences
    }
    final case class NestedEnumElement(enumValue: Enum)     extends MessageElement {
      def collectTypeReferences: Set[String] = Set.empty
    }
  }

  final case class Oneof(name: String, fields: List[Field])

  final case class Field(ty: Type, name: String, number: Int, deprecated: Boolean = false, optional: Boolean = false, comment: Option[String] = None)

  sealed trait Reserved
  object Reserved {
    final case class Number(number: Int)         extends Reserved
    final case class Name(name: String)          extends Reserved
    final case class Range(start: Int, end: Int) extends Reserved
  }

  case class EnumValue(name: String, intValue: Int)
  case class Enum(name: String, values: List[EnumValue], reserved: List[Reserved], comment: Option[String] = None)

  final case class Service(name: String, rpcs: List[Rpc], comment: Option[String] = None)

  final case class RpcMessage(fqn: Fqn)

  final case class Rpc(
    name: String,
    request: RpcMessage,
    response: RpcMessage,
    streamingRequest: Boolean,
    streamingResponse: Boolean,
    comment: Option[String]
  )

  sealed trait Type { self =>
    def collectTypeReferences: Set[String] =
      self match {
        case _: ProtoIR.Type.PrimitiveType            => Set.empty
        case ProtoIR.Type.MapType(keyType, valueType) => keyType.collectTypeReferences ++ valueType.collectTypeReferences
        case ProtoIR.Type.ListType(valueType)         => valueType.collectTypeReferences
        case ProtoIR.Type.RefType(fqn)                => Set(fqn.name)
        case ProtoIR.Type.EnumRefType(fqn)            => Set(fqn.name)
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
    final case class RefType(fqn: Fqn)                       extends Type
    final case class EnumRefType(fqn: Fqn)                   extends Type
  }

  final case class Fqn(packageName: Option[List[String]], name: String) {
    def render: String =
      packageName.map(_.mkString(".")).map(_ + ".").getOrElse("") + name
  }
}
