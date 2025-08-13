package proteus

object ProtoIR {

  final case class CompilationUnit(
    packageName: Option[String],
    statements: List[Statement],
    options: List[TopLevelOption]
  )

  final case class TopLevelOption(key: String, value: String)

  sealed trait Statement
  object Statement {
    final case class ImportStatement(path: String)     extends Statement
    final case class TopLevelStatement(s: TopLevelDef) extends Statement
  }

  sealed trait TopLevelDef {
    def name: String
  }
  object TopLevelDef       {
    final case class MessageDef(message: Message) extends TopLevelDef {
      def name: String = message.name
    }
    final case class EnumDef(enumValue: Enum)     extends TopLevelDef {
      def name: String = enumValue.name
    }
    final case class ServiceDef(service: Service) extends TopLevelDef {
      def name: String = service.name
    }
  }

  final case class Message(
    name: String,
    elements: List[MessageElement],
    reserved: List[Reserved]
  )

  sealed trait MessageElement
  object MessageElement {
    final case class FieldElement(field: Field)             extends MessageElement
    final case class EnumDefElement(enumValue: Enum)        extends MessageElement
    final case class OneofElement(oneof: Oneof)             extends MessageElement
    final case class NestedMessageElement(message: Message) extends MessageElement
  }

  final case class Oneof(name: String, fields: List[Field])

  final case class Field(
    ty: Type,
    name: String,
    number: Int,
    deprecated: Boolean = false,
    optional: Boolean = false
  )

  sealed trait Reserved
  object Reserved {
    final case class Number(number: Int)         extends Reserved
    final case class Name(name: String)          extends Reserved
    final case class Range(start: Int, end: Int) extends Reserved
  }

  case class EnumValue(name: String, intValue: Int)
  case class Enum(
    name: String,
    values: List[EnumValue],
    reserved: List[Reserved]
  )

  final case class Service(name: String, rpcs: List[Rpc])

  final case class RpcMessage(fqn: Fqn)

  final case class Rpc(
    name: String,
    request: RpcMessage,
    response: RpcMessage,
    streamingRequest: Boolean,
    streamingResponse: Boolean
  )

  sealed trait Type
  object Type {
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

    val Time  = RefType(Fqn(None, "Time"))
    val Empty = RefType(Fqn(None, "Empty"))
    val Dummy = RefType(Fqn(None, "Dummy"))
  }

  final case class Fqn(packageName: Option[List[String]], name: String) {
    def render: String =
      packageName.map(_.mkString(".")).map(_ + ".").getOrElse("") + name
  }
}
