package proteus

import java.io.*

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.compiletime.*
import scala.util.control.NonFatal

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import zio.blocks.schema.{Optional as _, *}
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

import proteus.ProtobufCodec.*
import proteus.ProtobufCodec.MessageField.*
import proteus.ProtoIR.{CompilationUnit, TopLevelOption}
import proteus.ProtoIR.Statement.TopLevelStatement
import proteus.internal.*

/**
  * A codec that describes how a scala type `A` is encoded and decoded to and from protobuf.
  */
sealed trait ProtobufCodec[A] {
  private[proteus] type Focus = A

  /**
    * Encodes a value of type `A` to its protobuf binary representation.
    *
    * @param value the value to encode.
    */
  def encode(value: A): Array[Byte] =
    wrapEncode(getName, prependOnExisting = false) {
      withRegisters { registers =>
        val writer = toProtoWriter(this, value, -1, registers, RegisterOffset.Zero, alwaysEncode = true)
        val size   = writer match {
          case f: ProtobufWriter.Message      => f.innerSize
          case f: ProtobufWriter.IntPrimitive => f.size
          case _                              => throw new Exception(s"Invalid root writer: $writer")
        }
        val bytes  = new Array[Byte](size)
        val output = CodedOutputStream.newInstance(bytes)
        writer.write(using output)
        bytes
      }
    }

  /**
    * Decodes a value of type `A` from its protobuf binary representation.
    *
    * @param bytes the protobuf binary representation to decode.
    */
  def decode(bytes: Array[Byte]): A =
    decode(CodedInputStream.newInstance(bytes))

  /**
    * Decodes a value of type `A` from its protobuf binary representation.
    *
    * @param inputStream the input stream to decode.
    */
  def decode(inputStream: InputStream): A =
    decode(CodedInputStream.newInstance(inputStream))

  /**
    * Creates a codec for `B` from the current codec of `A` given an isomorphism between `A` and `B`.
    *
    * @param from the function to transform from `A` to `B`.
    * @param to the function to transform from `B` to `A`.
    */
  def transform[B](from: A => B, to: B => A): ProtobufCodec[B] =
    this match {
      case c: Transform[a0, A] =>
        Transform[a0, B](a => from(c.from(a)), b => c.to(to(b)), c.codec)
      case _                   =>
        Transform(from, to, this)
    }

  /**
    * Renders the codec to a .proto file as a string.
    */
  def render(packageName: Option[String] = None, options: List[TopLevelOption] = List.empty): String =
    Renderer.render(CompilationUnit(packageName, toProtoIR(this).map(TopLevelStatement(_)), options))

  private def decode(input: CodedInputStream): A =
    wrapDecode(getName, prependOnExisting = false) {
      withRegisters { registers =>
        read(registers, RegisterOffset.Zero, this)(using input)
      }
    }

  private[proteus] def makeNested: ProtobufCodec[A] =
    this match {
      case message: Message[_]        => message.copy(nested = if (message.nested.isEmpty) Some(true) else message.nested)
      case Transform(from, to, codec) => Transform(from, to, codec.makeNested)
      case RecursiveMessage(thunk)    =>
        RecursiveMessage { () =>
          val m = thunk()
          m.copy(nested = if (m.nested.isEmpty) Some(true) else m.nested)
        }
      case Optional(codec)            => Optional(codec.makeNested)
      case _                          => this
    }

  private[proteus] def getName: String =
    this match {
      case message: Message[_]     => message.name
      case e: Enum[_]              => e.name
      case Transform(_, _, codec)  => codec.getName
      case RecursiveMessage(thunk) => thunk().name
      case Optional(codec)         => codec.getName
      case _                       => ""
    }
}

object ProtobufCodec {

  /**
    * Returns the codec for type `A` if it is already available.
    */
  def apply[A](using codec: ProtobufCodec[A]): ProtobufCodec[A] = codec

  /**
    * Derives a codec for type `A` using the given deriver.
    * If a Schema for `A` is available, it will be used, otherwise a Schema will be derived too.
    *
    * @param deriver the deriver to use.
    */
  inline def derived[A](using deriver: ProtobufDeriver): ProtobufCodec[A] = {
    val schema = summonFrom {
      case schema: Schema[A] => schema
      case _                 => Schema.derived[A]
    }
    schema.derive(deriver)
  }

  private class RegistersHolder(val registers: Registers, var inUse: Boolean)

  private val pool = new ThreadLocal[RegistersHolder] {
    override def initialValue(): RegistersHolder = new RegistersHolder(Registers(RegisterOffset.Zero), false)
  }

  private[proteus] inline def withRegisters[A](f: Registers => A): A = {
    val holder = pool.get()
    if (holder.inUse) f(Registers(RegisterOffset.Zero))
    else
      try {
        holder.inUse = true
        f(holder.registers)
      } finally holder.inUse = false
  }

  /**
    * Wraps encode failures with path enrichment.
    *
    * `segment` is by-name to ensure we allocate the string only on the exceptional path.
    */
  private[proteus] inline def wrapEncode[A](inline segment: => String, prependOnExisting: Boolean = true)(inline thunk: => A): A =
    try thunk
    catch {
      case e: ProtobufEncodeFailure =>
        if (prependOnExisting) e.prepend(segment)
        throw e
      case NonFatal(e)              =>
        val pe = ProtobufEncodeFailure.from(e)
        pe.prepend(segment)
        throw pe
    }

  /**
    * Wraps decode failures with path enrichment.
    *
    * `segment` is by-name to ensure we allocate the string only on the exceptional path.
    */
  private[proteus] inline def wrapDecode[A](inline segment: => String, prependOnExisting: Boolean = true)(inline thunk: => A): A =
    try thunk
    catch {
      case e: ProtobufDecodeFailure =>
        if (prependOnExisting) e.prepend(segment)
        throw e
      case NonFatal(e)              =>
        val pe = ProtobufDecodeFailure.from(e)
        pe.prepend(segment)
        throw pe
    }

  /**
    * Represents a field of a message. It can be a simple field, a one-of field, or an excluded field.
    */
  sealed trait MessageField[A] {

    /**
      * Converts the message field to its protobuf IR representation.
      */
    def toProtoIR: ProtoIR.MessageElement.FieldElement | ProtoIR.MessageElement.OneOfElement
  }

  object MessageField {

    /**
      * Represents a simple field of a message.
      */
    final case class SimpleField[A](
      name: String,
      id: Int,
      codec: ProtobufCodec[A],
      register: Register[Any],
      defaultValue: A,
      comment: Option[String] = None
    ) extends MessageField[A] {
      private[proteus] def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter = {
        val res = getFromRegister(registers, offset, register).asInstanceOf[A]
        wrapEncode(s"$name#$id") {
          ProtobufCodec.toProtoWriter(codec, res, id, registers, nextOffset, alwaysEncode = false)
        }
      }

      /**
        * Converts the message field to its protobuf IR representation.
        */
      def toProtoIR: ProtoIR.MessageElement.FieldElement = {
        val field = ProtoIR.Field(toProtoType(codec), name, id, deprecated = false, optional = isOptional(using codec), comment = comment)
        ProtoIR.MessageElement.FieldElement(field)
      }

      private[proteus] val mayUseBuilder: Boolean = {
        def loop[A](codec: ProtobufCodec[A]): Boolean =
          codec match {
            case _: Repeated[_, _]       => true
            case _: RepeatedMap[_, _, _] => true
            case Transform(_, _, codec)  => loop(codec)
            case _                       => false
          }
        loop(codec)
      }
    }

    /**
      * Represents a one-of field of a message.
      */
    final case class OneOfField[A](
      name: String,
      cases: Array[SimpleField[?]],
      register: Register[Any],
      discriminator: Discriminator[A],
      defaultValue: A,
      comment: Option[String] = None
    ) extends MessageField[A] {
      private[proteus] def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter = {
        val res = getFromRegister(registers, offset, register).asInstanceOf[A]
        if (res == null) null
        else {
          wrapEncode(name) {
            val field = cases(discriminator.discriminate(res))
            wrapEncode(s"${field.name}#${field.id}") {
              ProtobufCodec.toProtoWriter(field.codec, res.asInstanceOf[field.codec.Focus], field.id, registers, nextOffset, alwaysEncode = true)
            }
          }
        }
      }

      /**
        * Converts the message field to its protobuf IR representation.
        */
      def toProtoIR: ProtoIR.MessageElement.OneOfElement = {
        val fields = cases
          .map(field =>
            ProtoIR.Field(
              toProtoType(field.codec),
              field.name,
              field.id,
              deprecated = false,
              optional = isOptional(using field.codec),
              comment = field.comment
            )
          )
          .toList
          .sortBy(_.number)
        ProtoIR.MessageElement.OneOfElement(ProtoIR.OneOf(name, fields, comment))
      }
    }

    /**
      * Represents an excluded field of a message.
      */
    final case class ExcludedField[A](register: Register[Any], defaultValue: A) extends MessageField[A] {

      /**
        * Converts the message field to its protobuf IR representation.
        */
      def toProtoIR: ProtoIR.MessageElement.FieldElement =
        ProtoIR.MessageElement.FieldElement(ProtoIR.excludedField)
    }
  }

  /**
    * Represents a primitive type supported by protobuf.
    */
  final case class Primitive[A](primitiveType: PrimitiveType[A]) extends ProtobufCodec[A] {
    private[proteus] def toProtoWriter(a: A, id: Int, alwaysEncode: Boolean): ProtobufWriter =
      primitiveType match {
        case _: PrimitiveType.Int     =>
          val value: Int = a
          if (value == 0 && !alwaysEncode) null else internal.ProtobufWriter.IntPrimitive(value, id)
        case _: PrimitiveType.Long    =>
          val value: Long = a
          if (value == 0L && !alwaysEncode) null else internal.ProtobufWriter.LongPrimitive(value, id)
        case _: PrimitiveType.Boolean =>
          val value: Boolean = a
          if (!value && !alwaysEncode) null else internal.ProtobufWriter.BoolPrimitive(value, id)
        case _: PrimitiveType.String  =>
          val value: String = a
          if (value == "" && !alwaysEncode) null else internal.ProtobufWriter.StringPrimitive(value, id)
        case _: PrimitiveType.Double  =>
          val value: Double = a
          if (value == 0d && !alwaysEncode) null else internal.ProtobufWriter.DoublePrimitive(value, id)
        case _: PrimitiveType.Float   =>
          val value: Float = a
          if (value == 0f && !alwaysEncode) null else internal.ProtobufWriter.FloatPrimitive(value, id)
        case _                        => throw new Exception(s"Unsupported primitive type: $primitiveType")
      }
  }

  /**
    * Represents a value of an enum.
    */
  final case class EnumValue[A](name: String, index: Int, value: A, comment: Option[String] = None)

  /**
    * Represents an enum type.
    */
  final case class Enum[A](name: String, values: List[EnumValue[A]], reserved: List[Int], nested: Boolean, comment: Option[String] = None)
    extends ProtobufCodec[A] {
    val valuesByIndex: IntDenseMap[A]    = IntDenseMap.from(values.map(v => (v.index, v.value)))
    val indexesByValue: HashMap[A, Int]  = HashMap.from(values.map(v => (v.value, v.index)))
    val namesByValue: HashMap[A, String] = HashMap.from(values.map(v => (v.value, v.name)))

    private[proteus] def toProtoWriter(a: A, id: Int, alwaysEncode: Boolean): ProtobufWriter.IntPrimitive = {
      val index = indexesByValue(a)
      if (index == 0 && !alwaysEncode) null else internal.ProtobufWriter.IntPrimitive(index, id)
    }

    /**
      * Converts the enum to its protobuf IR representation.
      */
    def toProtoIR: ProtoIR.Enum =
      ProtoIR.Enum(
        name,
        values.sortBy(_.index).map(v => ProtoIR.EnumValue(v.name, v.index, v.comment)).toList,
        reserved = reserved.sorted.map(ProtoIR.Reserved.Number(_)),
        comment = comment
      )
  }

  /**
    * Represents a message type.
    */
  final case class Message[A](
    name: String,
    fields: Array[MessageField[?]],
    constructor: Constructor[A],
    deconstructor: Deconstructor[A],
    usedRegisters: RegisterOffset,
    reserved: Set[Int],
    inline: Boolean,
    nested: Option[Boolean],
    comment: Option[String] = None
  ) extends ProtobufCodec[A] {

    /**
      * The list of all fields of the message (oneof fields are flattened into simple fields).
      */
    val simpleFields: List[SimpleField[?]] = fields.toList.flatMap {
      case f: SimpleField[?]   => List(f)
      case f: OneOfField[?]    => f.cases.toList
      case f: ExcludedField[?] => Nil
    }

    /**
      * An optimized map of the fields by their index.
      */
    val fieldMap: IntDenseMap[IndexedField]     = IntDenseMap.from(fields.zipWithIndex.flatMap {
      case (f: SimpleField[?], idx)   => List(f.id -> IndexedField(f, idx))
      case (f: OneOfField[?], idx)    => f.cases.map(c => c.id -> IndexedField(c, idx)).toList
      case (f: ExcludedField[?], idx) => Nil
    })
    private[proteus] val mayUseBuilder: Boolean = simpleFields.exists(_.mayUseBuilder)

    private[proteus] def toProtoWriter(a: A, id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter.Message =
      wrapEncode(name) {
        deconstructor.deconstruct(registers, offset, a)
        val nextOffset = offset + usedRegisters
        val builder    = List.newBuilder[ProtobufWriter]
        var i          = 0
        var size       = 0
        while (i < fields.length) {
          val res = fields(i) match {
            case field: SimpleField[?]   => field.toProtoWriter(registers, offset, nextOffset)
            case field: OneOfField[?]    => field.toProtoWriter(registers, offset, nextOffset)
            case field: ExcludedField[?] => null
          }
          if (res ne null) {
            builder += res
            size += res.size
          }
          i += 1
        }
        internal.ProtobufWriter.Message(id, builder.result(), size)
      }

    /**
      * Converts the message to its protobuf IR representation.
      */
    def toProtoIR: ProtoIR.Message = {
      val elements = fields.map(_.toProtoIR)

      def findNested[A](codec: ProtobufCodec[A], goDeep: Boolean = false): List[ProtoIR.MessageElement] =
        codec match {
          case c: Message[_]           =>
            if (c.nested.getOrElse(false)) List(ProtoIR.MessageElement.NestedMessageElement(c.toProtoIR))
            else if (goDeep) c.simpleFields.collect(field => findNested(field.codec)).flatten.distinct
            else Nil
          case c: Enum[_]              => if (c.nested) List(ProtoIR.MessageElement.NestedEnumElement(c.toProtoIR)) else Nil
          case c: Transform[_, _]      => findNested(c.codec, goDeep)
          case c: Optional[_]          => findNested(c.codec, goDeep)
          case c: Repeated[_, _]       => findNested(c.element, goDeep)
          case c: RepeatedMap[_, _, _] => findNested(c.element, goDeep = true)
          case c: RecursiveMessage[_]  => findNested(c.codec)
          case _                       => Nil
        }

      val nestedMessageElements = simpleFields.collect(field => findNested(field.codec)).flatten.distinct

      val sortedAllElements = elements.sortBy {
        case c: ProtoIR.MessageElement.OneOfElement => c.oneOf.fields.head.number
        case e: ProtoIR.MessageElement.FieldElement => e.field.number
      }
      ProtoIR.Message(
        name,
        nestedMessageElements ++ sortedAllElements,
        reserved = reserved.toList.sorted.map(ProtoIR.Reserved.Number(_)),
        comment = comment
      )
    }
  }

  /**
    * Represents a repeated type.
    */
  final case class Repeated[C[_], E](
    element: ProtobufCodec[E],
    constructor: SeqConstructor[C],
    deconstructor: SeqDeconstructor[C],
    packed: Boolean
  ) extends ProtobufCodec[C[E]] {
    private def toElementProtoWriter[A](codec: ProtobufCodec[A]): (Int, Registers, RegisterOffset) => A => ProtobufWriter =
      codec match {
        case c: Primitive[_]        =>
          c.primitiveType match {
            case _: PrimitiveType.Int     =>
              (id, _, _) => internal.ProtobufWriter.IntPrimitive(_, id)
            case _: PrimitiveType.Long    =>
              (id, _, _) => internal.ProtobufWriter.LongPrimitive(_, id)
            case _: PrimitiveType.Boolean =>
              (id, _, _) => internal.ProtobufWriter.BoolPrimitive(_, id)
            case _: PrimitiveType.String  =>
              (id, _, _) => internal.ProtobufWriter.StringPrimitive(_, id)
            case _: PrimitiveType.Double  =>
              (id, _, _) => internal.ProtobufWriter.DoublePrimitive(_, id)
            case _: PrimitiveType.Float   =>
              (id, _, _) => internal.ProtobufWriter.FloatPrimitive(_, id)
            case _                        =>
              throw new Exception(s"Unsupported primitive type: ${c.primitiveType}")
          }
        case c: Message[_]          => (id, registers, offset) => c.toProtoWriter(_, id, registers, offset)
        case c: Enum[_]             => (id, _, _) => c.toProtoWriter(_, id, alwaysEncode = true)
        case c: Transform[_, _]     =>
          (id, registers, offset) =>
            val makeWriter = toElementProtoWriter(c.codec)(id, registers, offset)
            a => makeWriter(c.to(a))
        case c: Optional[_]         =>
          (id, registers, offset) => {
            case None        => null
            case Some(value) => toElementProtoWriter(c.codec)(id, registers, offset)(value)
          }
        case c: RecursiveMessage[_] => (id, registers, offset) => c.codec.toProtoWriter(_, id, registers, offset)
        case _                      => throw new Exception(s"Invalid codec inside repeated: $codec")
      }

    private val elementProtoWriter: (Int, Registers, RegisterOffset) => E => ProtobufWriter =
      toElementProtoWriter(element)

    private[proteus] def toProtoWriter(a: C[E], id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter.Repeated = {
      val it = deconstructor.deconstruct(a)
      if (it.isEmpty) null
      else {
        val builder         = List.newBuilder[ProtobufWriter]
        val makeProtoWriter = elementProtoWriter(if (packed) -1 else id, registers, offset)
        var size            = 0
        while (it.hasNext) {
          val res = makeProtoWriter(it.next)
          if (res ne null) {
            builder += res
            size += res.size
          }
        }
        internal.ProtobufWriter.Repeated(builder.result(), id, packed, size)
      }
    }
  }

  /**
    * Represents a map type.
    */
  final case class RepeatedMap[C[_, _], K, V](
    element: Message[(K, V)],
    constructor: MapConstructor[C],
    deconstructor: MapDeconstructor[C]
  ) extends ProtobufCodec[C[K, V]] {
    private[proteus] def toProtoWriter(
      a: C[K, V],
      id: Int,
      registers: Registers,
      offset: RegisterOffset
    ): ProtobufWriter.Repeated = {
      val it = deconstructor.deconstruct(a)
      if (it.isEmpty) null
      else {
        val builder = List.newBuilder[ProtobufWriter]
        var size    = 0
        while (it.hasNext) {
          val kv  = it.next
          val res = element.toProtoWriter((deconstructor.getKey(kv), deconstructor.getValue(kv)), id, registers, offset)
          if (res ne null) {
            builder += res
            size += res.size
          }
        }
        internal.ProtobufWriter.Repeated(builder.result(), id, packed = false, size)
      }
    }
  }

  /**
    * Represents a bytes type.
    */
  case object Bytes extends ProtobufCodec[Array[Byte]] {
    private[proteus] def toProtoWriter(a: Array[Byte], id: Int, alwaysEncode: Boolean): ProtobufWriter.Bytes =
      if (a.isEmpty && !alwaysEncode) null else internal.ProtobufWriter.Bytes(a, id)
  }

  /**
    * Represents a transformed type.
    */
  final case class Transform[A, B](from: A => B, to: B => A, codec: ProtobufCodec[A]) extends ProtobufCodec[B] {
    private[proteus] type Origin = A

    private[proteus] def toProtoWriter(b: B, id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter =
      ProtobufCodec.toProtoWriter(codec, to(b), id, registers, offset, alwaysEncode)
  }

  /**
    * Represents a recursive message type.
    */
  final case class RecursiveMessage[A](thunk: () => Message[A]) extends ProtobufCodec[A] {
    lazy val codec = thunk()

    private[proteus] def toProtoWriter(a: A, id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter =
      codec.toProtoWriter(a, id, registers, offset)
  }

  /**
    * Represents an optional type.
    */
  final case class Optional[A](codec: ProtobufCodec[A]) extends ProtobufCodec[Option[A]] {
    private[proteus] def toProtoWriter(a: Option[A], id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter = a match {
      case None        => null
      case Some(value) => ProtobufCodec.toProtoWriter(codec, value, id, registers, offset, alwaysEncode = true)
    }
  }

  private[proteus] def toProtoWriter[A](
    codec: ProtobufCodec[A],
    a: A,
    id: Int,
    registers: Registers,
    offset: RegisterOffset,
    alwaysEncode: Boolean
  ): ProtobufWriter =
    codec match {
      case c: Primitive[_]         => c.toProtoWriter(a, id, alwaysEncode)
      case c: Message[_]           => c.toProtoWriter(a, id, registers, offset)
      case c: Repeated[c, e]       => c.toProtoWriter(a, id, registers, offset)
      case c: RepeatedMap[c, k, v] => c.toProtoWriter(a, id, registers, offset)
      case c: Enum[_]              => c.toProtoWriter(a, id, alwaysEncode)
      case c: Transform[_, _]      => c.toProtoWriter(a, id, registers, offset, alwaysEncode)
      case c: Optional[_]          => c.toProtoWriter(a, id, registers, offset)
      case c: Bytes.type           => c.toProtoWriter(a, id, alwaysEncode)
      case c: RecursiveMessage[_]  => c.toProtoWriter(a, id, registers, offset)
    }

  private def setDefaults[A](m: Message[A], registers: Registers, offset: RegisterOffset, visited: Array[Boolean]): Unit = {
    var i = 0
    while (i < visited.length) {
      if (!visited(i)) {
        m.fields(i) match {
          case field: SimpleField[?]   => setToRegister(registers, offset, field.register, field.defaultValue)
          case field: OneOfField[?]    =>
            if (field.defaultValue != null) setToRegister(registers, offset, field.register, field.defaultValue)
            else throw new Exception(s"OneOf field ${field.name} in message ${m.name} is absent and has no default value")
          case field: ExcludedField[?] => setToRegister(registers, offset, field.register, field.defaultValue)
        }
      } else if (m.mayUseBuilder) {
        // unpacked repeated fields use a builder that we need to convert to the final object
        val register = m.fields(i) match {
          case field: SimpleField[_] if field.mayUseBuilder =>
            def loop[A](codec: ProtobufCodec[A]): A =
              codec match {
                case c: Repeated[_, _]         =>
                  val v = getFromRegister(registers, offset, field.register)
                  // we need this check to do nothing in case it was packed
                  if (v.isInstanceOf[scala.collection.mutable.Builder[?, ?]])
                    c.constructor.resultObject(v.asInstanceOf[c.constructor.ObjectBuilder[Any]]).asInstanceOf[A]
                  else null.asInstanceOf[A]
                case c: RepeatedMap[_, _, _]   =>
                  val v = getFromRegister(registers, offset, field.register)
                  c.constructor.resultObject(v.asInstanceOf[c.constructor.ObjectBuilder[Any, Any]]).asInstanceOf[A]
                case Transform(from, _, codec) =>
                  val res = loop(codec)
                  // need to transform the result to the final type
                  if (res != null) from(res) else null.asInstanceOf[A]
                case _                         => null.asInstanceOf[A]
              }

            val res = loop(field.codec)
            if (res != null) setToRegister(registers, offset, field.register, res)
          case _                                            =>
        }
      }
      i += 1
    }
  }

  private def handleMessage[A](m: Message[A], registers: Registers, offset: RegisterOffset)(using input: CodedInputStream): A =
    wrapDecode(m.name) {
      val visited    = new Array[Boolean](m.fields.length)
      val nextOffset = offset + m.constructor.usedRegisters

      def handleRepeated[C[_], E](r: Repeated[C, E], field: IndexedField, tag: Int): C[E] = {
        val register = field.field.register.asInstanceOf[Register.Object[_ <: AnyRef]]
        val builder  =
          if (!visited(field.index)) {
            val builder = r.constructor.newObjectBuilder[E]()
            register.set(registers, offset, builder.asInstanceOf[register.Boxed])
            builder
          } else register.get(registers, offset).asInstanceOf[r.constructor.ObjectBuilder[E]]
        r.constructor.addObject(builder, loop(r.element, field, tag))
        null.asInstanceOf[C[E]]
      }

      def handleRepeatedMap[M[_, _], K, V](r: RepeatedMap[M, K, V], field: IndexedField): M[K, V] = {
        val register = field.field.register.asInstanceOf[Register.Object[_ <: AnyRef]]
        val builder  =
          if (!visited(field.index)) {
            val builder = r.constructor.newObjectBuilder[K, V]()
            register.set(registers, offset, builder.asInstanceOf[register.Boxed])
            builder
          } else register.get(registers, offset).asInstanceOf[r.constructor.ObjectBuilder[K, V]]
        val (k, v)   = withLimit(handleMessage(r.element, registers, nextOffset))
        r.constructor.addObject(builder, k, v)
        null.asInstanceOf[M[K, V]]
      }

      def loop[A](codec: ProtobufCodec[A], field: IndexedField, tag: Int): A =
        codec match {
          case c: Message[_]           => withLimit(handleMessage(c, registers, nextOffset))
          case c: Primitive[_]         => handlePrimitive(c)
          case c: Enum[_]              => c.valuesByIndex(input.readEnum())
          case c: Transform[_, _]      =>
            val res = loop(c.codec, field, tag)
            if (res == null) null.asInstanceOf[A] else c.from(res)
          case c: Optional[_]          => Some(loop(c.codec, field, tag))
          case c: Repeated[c, e]       =>
            if (c.packed && (tag & 0x7) == 2) handlePackedRepeated(c)
            else handleRepeated(c, field, tag)
          case c: RepeatedMap[m, k, v] => handleRepeatedMap(c, field)
          case Bytes                   => input.readByteArray()
          case c: RecursiveMessage[_]  => withLimit(handleMessage(c.codec, registers, nextOffset))
        }

      var done = false
      while (!done) {
        val tag = input.readTag()
        if (tag == 0) done = true
        else {
          val fieldId = tag >>> 3
          val field   = m.fieldMap(fieldId)
          if (field ne null) {
            val value = wrapDecode(s"${field.field.name}#$fieldId") {
              loop(field.field.codec, field, tag)
            }
            visited(field.index) = true
            if (value != null) setToRegister(registers, offset, field.field.register, value)
          } else input.skipField(tag): Unit
        }
      }
      setDefaults(m, registers, offset, visited)
      m.constructor.construct(registers, offset)
    }

  private def handlePrimitive[A](p: Primitive[A])(using input: CodedInputStream): A =
    p.primitiveType match {
      case _: PrimitiveType.Int     => input.readInt32()
      case _: PrimitiveType.Long    => input.readInt64()
      case _: PrimitiveType.Boolean => input.readBool()
      case _: PrimitiveType.String  => input.readStringRequireUtf8()
      case _: PrimitiveType.Double  => input.readDouble()
      case _: PrimitiveType.Float   => input.readFloat()
      case _                        => throw new Exception(s"Unsupported primitive type: $p")
    }

  private def handlePackedRepeated[C[_], E](r: Repeated[C, E])(using input: CodedInputStream): C[E] = {
    def loop[A](codec: ProtobufCodec[A]): () => A =
      codec match {
        case c: Primitive[_]    =>
          c.primitiveType match {
            case _: PrimitiveType.Int     => () => input.readInt32()
            case _: PrimitiveType.Long    => () => input.readInt64()
            case _: PrimitiveType.Boolean => () => input.readBool()
            case _: PrimitiveType.Double  => () => input.readDouble()
            case _: PrimitiveType.Float   => () => input.readFloat()
            case _                        => throw new Exception(s"Unsupported packed primitive type: $c")
          }
        case c: Enum[_]         => () => c.valuesByIndex(input.readEnum())
        case c: Transform[_, _] => () => c.from(loop(c.codec)())
        case _                  => throw new Exception(s"Invalid packed type: $codec")
      }

    val builder = r.constructor.newObjectBuilder[E]()
    withLimit {
      val getElement = loop(r.element)
      while (input.getBytesUntilLimit > 0)
        r.constructor.addObject(builder, getElement())
    }
    r.constructor.resultObject(builder)
  }

  private def read[A](registers: Registers, offset: RegisterOffset, codec: ProtobufCodec[A])(using input: CodedInputStream): A = {
    def loop[A](codec: ProtobufCodec[A], offset: RegisterOffset): A =
      codec match {
        case c: Message[_]          => handleMessage(c, registers, offset)
        case c: RecursiveMessage[_] => handleMessage(c.codec, registers, offset)
        case c: Transform[_, _]     => c.from(loop(c.codec, offset))
        case c: Enum[_]             => c.valuesByIndex(input.readEnum())
        case _                      => throw new Exception(s"Invalid root codec: $codec")
      }

    loop(codec, offset)
  }

  private inline def withLimit[A](inline computeResult: => A)(using input: CodedInputStream): A = {
    val length   = input.readRawVarint32()
    val oldLimit = input.pushLimit(length)
    val result   = computeResult
    input.popLimit(oldLimit)
    result
  }

  private[proteus] def isOptional[A](using codec: ProtobufCodec[A]): Boolean =
    codec match {
      case c: Transform[_, _] => isOptional(using c.codec)
      case _: Optional[_]     => true
      case _                  => false
    }

  private[proteus] def isRepeated[A](using codec: ProtobufCodec[A]): Boolean =
    codec match {
      case c: Transform[_, _]      => isRepeated(using c.codec)
      case _: Repeated[_, _]       => true
      case _: RepeatedMap[_, _, _] => true
      case _                       => false
    }

  /**
    * Converts the given codec to its protobuf IR representation.
    */
  def toProtoIR(codec: ProtobufCodec[?]): List[ProtoIR.TopLevelDef] = {
    val visited = new mutable.HashSet[ProtobufCodec[?]]()

    def findTopLevelDefs[A](codec: ProtobufCodec[A]): List[ProtoIR.TopLevelDef] =
      codec match {
        case c: Message[_]           =>
          if (visited.contains(c)) Nil
          else {
            if (!c.name.isEmpty) {
              visited.add(c): Unit
              if (c.nested.getOrElse(false)) c.simpleFields.map(_.codec).flatMap(findTopLevelDefs)
              else ProtoIR.TopLevelDef.MessageDef(c.toProtoIR) :: c.simpleFields.map(_.codec).flatMap(findTopLevelDefs)
            } else c.simpleFields.map(_.codec).flatMap(findTopLevelDefs)
          }
        case c: Transform[_, _]      => findTopLevelDefs(c.codec)
        case c: Optional[_]          => findTopLevelDefs(c.codec)
        case c: Repeated[_, _]       => findTopLevelDefs(c.element)
        case c: RepeatedMap[_, _, _] => findTopLevelDefs(c.element)
        case c: RecursiveMessage[_]  => findTopLevelDefs(c.codec)
        case c: Enum[_]              =>
          if (visited.contains(c)) Nil
          else {
            visited.add(c)
            if (c.nested) Nil else List(ProtoIR.TopLevelDef.EnumDef(c.toProtoIR))
          }
        case _                       => Nil
      }

    findTopLevelDefs(codec)
  }

  private def toProtoType(codec: ProtobufCodec[?]): ProtoIR.Type =
    codec match {
      case c: Transform[_, _]      => toProtoType(c.codec)
      case c: Optional[_]          => toProtoType(c.codec)
      case c: RecursiveMessage[_]  => toProtoType(c.codec)
      case c: Primitive[_]         =>
        c.primitiveType match {
          case _: PrimitiveType.Int     => ProtoIR.Type.Int32
          case _: PrimitiveType.Long    => ProtoIR.Type.Int64
          case _: PrimitiveType.Boolean => ProtoIR.Type.Bool
          case _: PrimitiveType.String  => ProtoIR.Type.String
          case _: PrimitiveType.Double  => ProtoIR.Type.Double
          case _: PrimitiveType.Float   => ProtoIR.Type.Float
          case _                        => throw new Exception(s"Unsupported primitive type: $c")
        }
      case c: Message[_]           => ProtoIR.Type.RefType(c.name)
      case c: Enum[_]              => ProtoIR.Type.EnumRefType(c.name)
      case c: Repeated[_, _]       => ProtoIR.Type.ListType(toProtoType(c.element))
      case c: RepeatedMap[_, _, _] =>
        ProtoIR.Type.MapType(toProtoType(c.element.simpleFields(0).codec), toProtoType(c.element.simpleFields(1).codec))
      case Bytes                   => ProtoIR.Type.Bytes
    }
}
