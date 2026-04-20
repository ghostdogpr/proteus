package proteus

import java.io.*

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.compiletime.*
import scala.reflect.ClassTag
import scala.util.control.NonFatal

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import zio.blocks.schema.{Optional as _, *}
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset
import zio.blocks.typeid.TypeId

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
  final def encode(value: A): Array[Byte] =
    wrapEncode(getName, prependOnExisting = false) {
      withRegistersAndCache { (registers, cache) =>
        cache.reset()
        val size   = computeRootSize(this, value, registers, cache)
        val bytes  = new Array[Byte](size)
        val output = CodedOutputStream.newInstance(bytes)
        cache.reset()
        write(this, value, -1, registers, alwaysEncode = true, cache)(using output)
        bytes
      }
    }

  /**
    * Decodes a value of type `A` from its protobuf binary representation.
    *
    * @param bytes the protobuf binary representation to decode.
    */
  final def decode(bytes: Array[Byte]): A =
    decode(CodedInputStream.newInstance(bytes))

  /**
    * Decodes a value of type `A` from its protobuf binary representation.
    *
    * @param inputStream the input stream to decode.
    */
  final def decode(inputStream: InputStream): A =
    decode(CodedInputStream.newInstance(inputStream))

  /**
    * Creates a codec for `B` from the current codec of `A` given an isomorphism between `A` and `B`.
    *
    * @param from the function to transform from `A` to `B`.
    * @param to the function to transform from `B` to `A`.
    */
  final def transform[B](from: A => B, to: B => A): ProtobufCodec[B] =
    this match {
      case c: Transform[a0, A] =>
        Transform[a0, B](a => from(c.from(a)), b => c.to(to(b)), c.codec)
      case _                   =>
        Transform(from, to, this)
    }

  /**
    * Renders the codec to a .proto file as a string.
    */
  final def render(packageName: Option[String] = None, options: List[TopLevelOption] = List.empty): String = {
    val rawDefs   = findTopLevelDefs(this)
    val paths     = nestedInPaths(rawDefs)
    val relocated = relocateNestedIn(rawDefs)
    Renderer.render(CompilationUnit(packageName, qualifyReferences(relocated, paths).map(TopLevelStatement(_)), options))
  }

  private def decode(input: CodedInputStream): A =
    wrapDecode(getName, prependOnExisting = false) {
      withRegisters { registers =>
        read(registers, RegisterOffset.Zero, this)(using input)
      }
    }

  final private[proteus] def makeNested: ProtobufCodec[A] =
    this match {
      case message: Message[_]        => message.copy(nested = if (message.nested.isEmpty) Some(NestedPlacement.Auto) else message.nested)
      case Transform(from, to, codec) => Transform(from, to, codec.makeNested)
      case RecursiveMessage(thunk)    =>
        RecursiveMessage { () =>
          val m = thunk()
          m.copy(nested = if (m.nested.isEmpty) Some(NestedPlacement.Auto) else m.nested)
        }
      case Optional(codec, oneof)     => Optional(codec.makeNested, oneof)
      case _                          => this
    }

  @tailrec
  final private[proteus] def getName: String =
    this match {
      case message: Message[_]     => message.name
      case e: Enum[_]              => e.name
      case Transform(_, _, codec)  => codec.getName
      case RecursiveMessage(thunk) => thunk().name
      case Optional(codec, _)      => codec.getName
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

  /**
    * Describes where a nested type should be placed relative to its surrounding types.
    */
  sealed trait NestedPlacement
  object NestedPlacement {

    /**
      * Nest the type inside its direct parent message.
      */
    case object Auto extends NestedPlacement

    /**
      * Force the type to be defined at the root level.
      */
    case object Unnested extends NestedPlacement

    /**
      * Nest the type inside a specific ancestor identified by its fully-qualified type name.
      * The target name is resolved at rendering time against the actual rendered name of the target type
      * (so renames on the target are honored).
      */
    final case class In(targetFullName: String) extends NestedPlacement

    private[proteus] def toPlacement(opt: Option[NestedPlacement]): ProtoIR.Placement =
      opt match {
        case Some(Auto)            => ProtoIR.Placement.Nested
        case Some(In(fullName))    => ProtoIR.Placement.NestedIn(fullName)
        case Some(Unnested) | None => ProtoIR.Placement.TopLevel
      }
  }

  private class RegistersHolder(val registers: Registers, val cache: WriterCache, var inUse: Boolean)

  private val initialRegisterOffset = RegisterOffset(objects = 64, ints = 64)

  private val pool = new ThreadLocal[RegistersHolder] {
    override def initialValue(): RegistersHolder = new RegistersHolder(Registers(initialRegisterOffset), new WriterCache(), false)
  }

  private[proteus] inline def withRegisters[A](inline f: Registers => A): A = {
    val holder = pool.get()
    if (holder.inUse) f(Registers(initialRegisterOffset))
    else
      try {
        holder.inUse = true
        f(holder.registers)
      } finally holder.inUse = false
  }

  private[proteus] inline def withRegistersAndCache[A](inline f: (Registers, WriterCache) => A): A = {
    val holder = pool.get()
    if (holder.inUse) f(Registers(initialRegisterOffset), new WriterCache())
    else
      try {
        holder.inUse = true
        f(holder.registers, holder.cache)
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
      comment: Option[String] = None,
      deprecated: Boolean = false
    ) extends MessageField[A] {

      /**
        * Converts the message field to its protobuf IR representation.
        */
      def toProtoIR: ProtoIR.MessageElement.FieldElement = {
        val opts  = ProtoIR.OptionValue.deprecatedOpts(deprecated)
        val field = ProtoIR.Field(toProtoType(codec), name, id, optional = isOptional(using codec), comment = comment, options = opts)
        ProtoIR.MessageElement.FieldElement(field)
      }

      private[proteus] val useBuilder: Boolean = {
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
      cases: IArray[SimpleField[?] | ExcludedField[?]],
      register: Register[Any],
      discriminator: Discriminator[A],
      defaultValue: A,
      comment: Option[String] = None
    ) extends MessageField[A] {

      /**
        * Converts the message field to its protobuf IR representation.
        */
      def toProtoIR: ProtoIR.MessageElement.OneOfElement = {
        val fields = cases
          .flatMap {
            case field: SimpleField[?]   =>
              val opts = ProtoIR.OptionValue.deprecatedOpts(field.deprecated)
              Some(
                ProtoIR.Field(
                  toProtoType(field.codec),
                  field.name,
                  field.id,
                  optional = isOptional(using field.codec),
                  comment = field.comment,
                  options = opts
                )
              )
            case field: ExcludedField[?] => None
          }
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
    private[proteus] def computeSizeFromRegisters(
      register: Register[A],
      id: Int,
      registers: Registers,
      offset: RegisterOffset,
      alwaysEncode: Boolean
    ): Int =
      primitiveType match {
        case _: PrimitiveType.Int     =>
          val value = register.asInstanceOf[Register.Int].get(registers, offset)
          if (value == 0 && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeInt32SizeNoTag(value)
          else CodedOutputStream.computeInt32Size(id, value)
        case _: PrimitiveType.Long    =>
          val value = register.asInstanceOf[Register.Long].get(registers, offset)
          if (value == 0L && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeInt64SizeNoTag(value)
          else CodedOutputStream.computeInt64Size(id, value)
        case _: PrimitiveType.Boolean =>
          val value = register.asInstanceOf[Register.Boolean].get(registers, offset)
          if (!value && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeBoolSizeNoTag(value)
          else CodedOutputStream.computeBoolSize(id, value)
        case _: PrimitiveType.String  =>
          val value = register.asInstanceOf[Register.Object[String]].get(registers, offset)
          if (value.isEmpty && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeStringSizeNoTag(value)
          else CodedOutputStream.computeStringSize(id, value)
        case _: PrimitiveType.Double  =>
          val value = register.asInstanceOf[Register.Double].get(registers, offset)
          if (value == 0d && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeDoubleSizeNoTag(value)
          else CodedOutputStream.computeDoubleSize(id, value)
        case _: PrimitiveType.Float   =>
          val value = register.asInstanceOf[Register.Float].get(registers, offset)
          if (value == 0f && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeFloatSizeNoTag(value)
          else CodedOutputStream.computeFloatSize(id, value)
        case _                        => throw new ProteusException(s"Unsupported primitive type: $primitiveType")
      }

    private[proteus] def computeSize(a: A, id: Int, alwaysEncode: Boolean): Int =
      primitiveType match {
        case _: PrimitiveType.Int     =>
          val value: Int = a
          if (value == 0 && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeInt32SizeNoTag(value)
          else CodedOutputStream.computeInt32Size(id, value)
        case _: PrimitiveType.Long    =>
          val value: Long = a
          if (value == 0L && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeInt64SizeNoTag(value)
          else CodedOutputStream.computeInt64Size(id, value)
        case _: PrimitiveType.Boolean =>
          val value: Boolean = a
          if (!value && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeBoolSizeNoTag(value)
          else CodedOutputStream.computeBoolSize(id, value)
        case _: PrimitiveType.String  =>
          val value: String = a
          if (value.isEmpty && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeStringSizeNoTag(value)
          else CodedOutputStream.computeStringSize(id, value)
        case _: PrimitiveType.Double  =>
          val value: Double = a
          if (value == 0d && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeDoubleSizeNoTag(value)
          else CodedOutputStream.computeDoubleSize(id, value)
        case _: PrimitiveType.Float   =>
          val value: Float = a
          if (value == 0f && !alwaysEncode) 0
          else if (id == -1) CodedOutputStream.computeFloatSizeNoTag(value)
          else CodedOutputStream.computeFloatSize(id, value)
        case _                        => throw new ProteusException(s"Unsupported primitive type: $primitiveType")
      }

    private[proteus] def writeFromRegisters(register: Register[A], id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean)(
      using output: CodedOutputStream
    ): Unit =
      primitiveType match {
        case _: PrimitiveType.Int     =>
          val value = register.asInstanceOf[Register.Int].get(registers, offset)
          if (value != 0 || alwaysEncode) {
            if (id == -1) output.writeInt32NoTag(value) else output.writeInt32(id, value)
          }
        case _: PrimitiveType.Long    =>
          val value = register.asInstanceOf[Register.Long].get(registers, offset)
          if (value != 0L || alwaysEncode) {
            if (id == -1) output.writeInt64NoTag(value) else output.writeInt64(id, value)
          }
        case _: PrimitiveType.Boolean =>
          val value = register.asInstanceOf[Register.Boolean].get(registers, offset)
          if (value || alwaysEncode) {
            if (id == -1) output.writeBoolNoTag(value) else output.writeBool(id, value)
          }
        case _: PrimitiveType.String  =>
          val value = register.asInstanceOf[Register.Object[String]].get(registers, offset)
          if (value.nonEmpty || alwaysEncode) {
            if (id == -1) output.writeStringNoTag(value) else output.writeString(id, value)
          }
        case _: PrimitiveType.Double  =>
          val value = register.asInstanceOf[Register.Double].get(registers, offset)
          if (value != 0d || alwaysEncode) {
            if (id == -1) output.writeDoubleNoTag(value) else output.writeDouble(id, value)
          }
        case _: PrimitiveType.Float   =>
          val value = register.asInstanceOf[Register.Float].get(registers, offset)
          if (value != 0f || alwaysEncode) {
            if (id == -1) output.writeFloatNoTag(value) else output.writeFloat(id, value)
          }
        case _                        => throw new ProteusException(s"Unsupported primitive type: $primitiveType")
      }

    private[proteus] def write(a: A, id: Int, alwaysEncode: Boolean)(using output: CodedOutputStream): Unit =
      primitiveType match {
        case _: PrimitiveType.Int     =>
          val value: Int = a
          if (value != 0 || alwaysEncode) {
            if (id == -1) output.writeInt32NoTag(value) else output.writeInt32(id, value)
          }
        case _: PrimitiveType.Long    =>
          val value: Long = a
          if (value != 0L || alwaysEncode) {
            if (id == -1) output.writeInt64NoTag(value) else output.writeInt64(id, value)
          }
        case _: PrimitiveType.Boolean =>
          val value: Boolean = a
          if (value || alwaysEncode) {
            if (id == -1) output.writeBoolNoTag(value) else output.writeBool(id, value)
          }
        case _: PrimitiveType.String  =>
          val value: String = a
          if (value.nonEmpty || alwaysEncode) {
            if (id == -1) output.writeStringNoTag(value) else output.writeString(id, value)
          }
        case _: PrimitiveType.Double  =>
          val value: Double = a
          if (value != 0d || alwaysEncode) {
            if (id == -1) output.writeDoubleNoTag(value) else output.writeDouble(id, value)
          }
        case _: PrimitiveType.Float   =>
          val value: Float = a
          if (value != 0f || alwaysEncode) {
            if (id == -1) output.writeFloatNoTag(value) else output.writeFloat(id, value)
          }
        case _                        => throw new ProteusException(s"Unsupported primitive type: $primitiveType")
      }
  }

  /**
    * Represents a value of an enum.
    */
  final case class EnumValue[A](name: String, index: Int, value: A, comment: Option[String] = None, deprecated: Boolean = false)

  /**
    * Represents an enum type.
    */
  final case class Enum[A](
    name: String,
    values: List[EnumValue[A]],
    reserved: List[Int],
    nested: Option[NestedPlacement],
    comment: Option[String] = None,
    typeId: Option[TypeId[?]] = None,
    customizeIR: ProtoIR.Enum => ProtoIR.Enum = identity
  ) extends ProtobufCodec[A] {
    private val valuesByIndex: IntDenseMap[A] = IntDenseMap.from(values.map(v => (v.index, v.value)))
    val indexesByValue: HashMap[A, Int]       = HashMap.from(values.map(v => (v.value, v.index)))
    val namesByValue: HashMap[A, String]      = HashMap.from(values.map(v => (v.value, v.name)))

    def valueOrThrow(raw: Int): A = {
      val value = valuesByIndex(raw)
      if (value == null) throw new ProteusException(s"Unknown enum value $raw for enum $name")
      value
    }

    private[proteus] def computeSize(a: A, id: Int, alwaysEncode: Boolean, cache: WriterCache): Int = {
      val index = indexesByValue(a)
      cache.recordSize(index)
      if (index == 0 && !alwaysEncode) 0
      else if (id == -1) CodedOutputStream.computeInt32SizeNoTag(index)
      else CodedOutputStream.computeInt32Size(id, index)
    }

    private[proteus] def write(id: Int, alwaysEncode: Boolean, cache: WriterCache)(using output: CodedOutputStream): Unit = {
      val index = cache.nextSize()
      if (index != 0 || alwaysEncode) {
        if (id == -1) output.writeInt32NoTag(index) else output.writeInt32(id, index)
      }
    }

    /**
      * Converts the enum to its protobuf IR representation.
      */
    def toProtoIR: ProtoIR.Enum =
      customizeIR(
        ProtoIR.Enum(
          name,
          values.sortBy(_.index).map { v =>
            val opts = ProtoIR.OptionValue.deprecatedOpts(v.deprecated)
            ProtoIR.EnumValue(v.name, v.index, v.comment, opts)
          },
          reserved = reserved.sorted.map(ProtoIR.Reserved.Number(_)),
          comment = comment,
          placement = NestedPlacement.toPlacement(nested),
          typeId = typeId.map(_.fullName)
        )
      )
  }

  /**
    * Represents a message type.
    */
  final case class Message[A](
    name: String,
    fields: IArray[MessageField[?]],
    constructor: Constructor[A],
    deconstructor: Deconstructor[A],
    usedRegisters: RegisterOffset,
    reserved: Set[Int],
    inline: Boolean,
    nested: Option[NestedPlacement],
    comment: Option[String] = None,
    typeId: Option[TypeId[?]] = None,
    customizeIR: ProtoIR.Message => ProtoIR.Message = identity
  ) extends ProtobufCodec[A] {

    /**
      * The list of all fields of the message (oneof fields are flattened into simple fields).
      */
    private[proteus] val simpleFields: List[SimpleField[?]] = fields.toList.flatMap {
      case f: SimpleField[?]   => List(f)
      case f: OneOfField[?]    => f.cases.toList.collect { case field: SimpleField[?] => field }
      case f: ExcludedField[?] => Nil
    }

    /**
      * An optimized map of the fields by their index.
      */
    private[proteus] val fieldMap: IntDenseMap[IndexedField] = IntDenseMap.from(fields.zipWithIndex.flatMap {
      case (f: SimpleField[?], idx)   => List(f.id -> IndexedField(f, idx))
      case (f: OneOfField[?], idx)    => f.cases.collect { case field: SimpleField[?] => field.id -> IndexedField(field, idx) }.toList
      case (f: ExcludedField[?], idx) => Nil
    })
    private[proteus] val useBuilder: Boolean                 = simpleFields.exists(_.useBuilder)
    private[proteus] val useBitmask: Boolean                 = fields.length <= 64

    private[proteus] def computeSize(a: A, id: Int, registers: Registers, cache: WriterCache): Int =
      wrapEncode(name) {
        val offset    = cache.getOffset()
        deconstructor.deconstruct(registers, offset, a)
        cache.addOffset(usedRegisters)
        val cacheSlot = if (id != -1) cache.reserveSize() else -1
        var i         = 0
        var innerSize = 0
        while (i < fields.length) {
          val fieldSize = fields(i) match {
            case field: SimpleField[?]   =>
              wrapEncode(s"${field.name}#${field.id}") {
                ProtobufCodec.computeSizeFromRegisters(
                  field.codec,
                  field.register.asInstanceOf[Register[field.codec.Focus]],
                  field.id,
                  registers,
                  offset,
                  alwaysEncode = false,
                  cache
                )
              }
            case field: OneOfField[a]    =>
              val res = getFromRegister(registers, offset, field.register).asInstanceOf[a]
              if (res == null) 0
              else {
                wrapEncode(field.name) {
                  field.cases(field.discriminator.discriminate(res)) match {
                    case simpleField: SimpleField[?] =>
                      wrapEncode(s"${simpleField.name}#${simpleField.id}") {
                        ProtobufCodec.computeSize(
                          simpleField.codec,
                          res.asInstanceOf[simpleField.codec.Focus],
                          simpleField.id,
                          registers,
                          alwaysEncode = true,
                          cache
                        )
                      }
                    case field: ExcludedField[?]     => 0
                  }
                }
              }
            case field: ExcludedField[?] => 0
          }
          innerSize += fieldSize
          i += 1
        }
        if (id == -1) {
          innerSize
        } else {
          cache.fillSize(cacheSlot, innerSize)
          CodedOutputStream.computeUInt32Size(id, innerSize) + innerSize
        }
      }

    private[proteus] def write(id: Int, registers: Registers, cache: WriterCache)(using output: CodedOutputStream): Unit =
      wrapEncode(name) {
        val offset = cache.getOffset()
        cache.addOffset(usedRegisters)
        if (id != -1) {
          val innerSize = cache.nextSize()
          output.writeTag(id, 2)
          output.writeUInt32NoTag(innerSize)
        }
        var i      = 0
        while (i < fields.length) {
          fields(i) match {
            case field: SimpleField[?]   =>
              wrapEncode(s"${field.name}#${field.id}") {
                ProtobufCodec.writeFromRegisters(
                  field.codec,
                  field.register.asInstanceOf[Register[field.codec.Focus]],
                  field.id,
                  registers,
                  offset,
                  alwaysEncode = false,
                  cache
                )
              }
            case field: OneOfField[a]    =>
              val res = getFromRegister(registers, offset, field.register).asInstanceOf[a]
              if (res != null) {
                wrapEncode(field.name) {
                  field.cases(field.discriminator.discriminate(res)) match {
                    case simpleField: SimpleField[?] =>
                      wrapEncode(s"${simpleField.name}#${simpleField.id}") {
                        ProtobufCodec.write(
                          simpleField.codec,
                          res.asInstanceOf[simpleField.codec.Focus],
                          simpleField.id,
                          registers,
                          alwaysEncode = true,
                          cache
                        )
                      }
                    case field: ExcludedField[?]     => ()
                  }
                }
              }
            case field: ExcludedField[?] => ()
          }
          i += 1
        }
      }

    /**
      * Converts the message to its protobuf IR representation.
      */
    def toProtoIR: ProtoIR.Message = {
      val elements = fields.map(_.toProtoIR)

      def findNested[A](codec: ProtobufCodec[A], goDeep: Boolean = false): List[ProtoIR.MessageElement] =
        codec match {
          case c: Message[_]           =>
            if (c.nested.contains(NestedPlacement.Auto)) List(ProtoIR.MessageElement.NestedMessageElement(c.toProtoIR))
            else if (goDeep) c.simpleFields.collect(field => findNested(field.codec)).flatten.distinct
            else Nil
          case c: Enum[_]              =>
            if (c.nested.contains(NestedPlacement.Auto)) List(ProtoIR.MessageElement.NestedEnumElement(c.toProtoIR)) else Nil
          case c: Transform[_, _]      => findNested(c.codec, goDeep)
          case c: Optional[_]          => findNested(c.codec, goDeep)
          case c: Repeated[_, _]       => findNested(c.element, goDeep)
          case c: RepeatedMap[_, _, _] => if (c.mapInProto) findNested(c.element, goDeep = true) else findNested(c.element, goDeep)
          case c: RecursiveMessage[_]  => findNested(c.codec)
          case _                       => Nil
        }

      val nestedMessageElements = simpleFields.collect(field => findNested(field.codec)).flatten.distinct

      val sortedAllElements = elements.sortBy {
        case c: ProtoIR.MessageElement.OneOfElement => c.oneOf.fields.head.number
        case e: ProtoIR.MessageElement.FieldElement => e.field.number
      }
      customizeIR(
        ProtoIR.Message(
          name,
          nestedMessageElements ++ sortedAllElements,
          reserved = reserved.toList.sorted.map(ProtoIR.Reserved.Number(_)),
          comment = comment,
          placement = NestedPlacement.toPlacement(nested),
          typeId = typeId.map(_.fullName)
        )
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
    packed: Boolean,
    elementClassTag: ClassTag[E]
  ) extends ProtobufCodec[C[E]] {
    private[proteus] def computeSize(a: C[E], id: Int, registers: Registers, cache: WriterCache): Int = {
      val it = deconstructor.deconstruct(a)
      if (it.isEmpty) 0
      else {
        val effectiveId = if (packed) -1 else id
        var innerSize   = 0
        val sizeIndex   = if (packed) cache.reserveSize() else -1
        while (it.hasNext)
          innerSize += ProtobufCodec.computeSize(element, it.next, effectiveId, registers, alwaysEncode = true, cache)
        if (packed) {
          cache.fillSize(sizeIndex, innerSize)
          CodedOutputStream.computeUInt32Size(id, innerSize) + innerSize
        } else {
          innerSize
        }
      }
    }

    private[proteus] def write(a: C[E], id: Int, registers: Registers, cache: WriterCache)(using output: CodedOutputStream): Unit = {
      val it = deconstructor.deconstruct(a)
      if (!it.isEmpty) {
        val effectiveId = if (packed) -1 else id
        if (packed) {
          val innerSize = cache.nextSize()
          output.writeTag(id, 2)
          output.writeUInt32NoTag(innerSize)
        }
        while (it.hasNext)
          ProtobufCodec.write(element, it.next, effectiveId, registers, alwaysEncode = true, cache)
      }
    }
  }

  /**
    * Represents a map type.
    */
  final case class RepeatedMap[C[_, _], K, V](
    element: Message[(K, V)],
    constructor: MapConstructor[C],
    deconstructor: MapDeconstructor[C],
    mapInProto: Boolean
  ) extends ProtobufCodec[C[K, V]] {
    private[proteus] def computeSize(a: C[K, V], id: Int, registers: Registers, cache: WriterCache): Int = {
      val it = deconstructor.deconstruct(a)
      if (it.isEmpty) 0
      else {
        var size = 0
        while (it.hasNext) {
          val kv    = it.next
          val tuple = deconstructor.getKeyValue(kv)
          size += element.computeSize(tuple, id, registers, cache)
        }
        size
      }
    }

    private[proteus] def write(a: C[K, V], id: Int, registers: Registers, cache: WriterCache)(using output: CodedOutputStream): Unit = {
      val it = deconstructor.deconstruct(a)
      if (!it.isEmpty) {
        while (it.hasNext) {
          it.next: Unit
          element.write(id, registers, cache)
        }
      }
    }
  }

  /**
    * Represents a bytes type.
    */
  case object Bytes extends ProtobufCodec[Array[Byte]] {
    final private[proteus] def computeSize(a: Array[Byte], id: Int, alwaysEncode: Boolean): Int =
      if (a.isEmpty && !alwaysEncode) 0 else CodedOutputStream.computeByteArraySize(id, a)

    final private[proteus] def write(a: Array[Byte], id: Int, alwaysEncode: Boolean)(using output: CodedOutputStream): Unit =
      if (a.nonEmpty || alwaysEncode) output.writeByteArray(id, a)
  }

  /**
    * Represents a transformed type.
    */
  final case class Transform[A, B](from: A => B, to: B => A, codec: ProtobufCodec[A]) extends ProtobufCodec[B] {
    private[proteus] type Origin = A
  }

  /**
    * Represents a recursive message type.
    */
  final case class RecursiveMessage[A](thunk: () => Message[A]) extends ProtobufCodec[A] {
    lazy val codec: Message[A] = thunk()
  }

  /**
    * Represents an optional type.
    */
  final case class Optional[A](codec: ProtobufCodec[A], oneof: Boolean) extends ProtobufCodec[Option[A]] {
    private[proteus] def computeSize(a: Option[A], id: Int, registers: Registers, cache: WriterCache): Int =
      a match {
        case None        => 0
        case Some(value) => ProtobufCodec.computeSize(codec, value, id, registers, alwaysEncode = true, cache)
      }

    private[proteus] def write(a: Option[A], id: Int, registers: Registers, cache: WriterCache)(using output: CodedOutputStream): Unit =
      a match {
        case None        => ()
        case Some(value) => ProtobufCodec.write(codec, value, id, registers, alwaysEncode = true, cache)
      }
  }

  final private[proteus] def computeSizeFromRegisters[A](
    codec: ProtobufCodec[A],
    register: Register[A],
    id: Int,
    registers: Registers,
    offset: RegisterOffset,
    alwaysEncode: Boolean,
    cache: WriterCache
  ): Int =
    codec match {
      case c: Primitive[_] =>
        c.computeSizeFromRegisters(register, id, registers, offset, alwaysEncode)
      case _               =>
        val res = getFromRegister(registers, offset, register)
        computeSize(codec, res, id, registers, alwaysEncode, cache)
    }

  final private[proteus] def computeSize[A](
    codec: ProtobufCodec[A],
    a: A,
    id: Int,
    registers: Registers,
    alwaysEncode: Boolean,
    cache: WriterCache
  ): Int =
    codec match {
      case c: Primitive[_]         => c.computeSize(a, id, alwaysEncode)
      case c: Message[_]           => c.computeSize(a, id, registers, cache)
      case c: Transform[_, _]      =>
        val v = c.to(a)
        cache.recordValue(v.asInstanceOf[AnyRef])
        computeSize(c.codec, v, id, registers, alwaysEncode, cache)
      case c: Enum[_]              => c.computeSize(a, id, alwaysEncode, cache)
      case c: Optional[_]          => c.computeSize(a, id, registers, cache)
      case c: Repeated[c, e]       => c.computeSize(a, id, registers, cache)
      case c: RepeatedMap[c, k, v] => c.computeSize(a, id, registers, cache)
      case c: Bytes.type           => c.computeSize(a, id, alwaysEncode)
      case c: RecursiveMessage[_]  => c.codec.computeSize(a, id, registers, cache)
    }

  final private[proteus] def writeFromRegisters[A](
    codec: ProtobufCodec[A],
    register: Register[A],
    id: Int,
    registers: Registers,
    offset: RegisterOffset,
    alwaysEncode: Boolean,
    cache: WriterCache
  )(using output: CodedOutputStream): Unit =
    codec match {
      case c: Primitive[_]    => c.writeFromRegisters(register, id, registers, offset, alwaysEncode)
      case c: Transform[_, _] =>
        val v = cache.nextValue()
        write(c.codec, v.asInstanceOf[c.codec.Focus], id, registers, alwaysEncode, cache)
      case _                  =>
        val res = getFromRegister(registers, offset, register)
        write(codec, res, id, registers, alwaysEncode, cache)
    }

  final private[proteus] def write[A](codec: ProtobufCodec[A], a: A, id: Int, registers: Registers, alwaysEncode: Boolean, cache: WriterCache)(
    using output: CodedOutputStream
  ): Unit =
    codec match {
      case c: Primitive[_]         => c.write(a, id, alwaysEncode)
      case c: Message[_]           => c.write(id, registers, cache)
      case c: Transform[_, _]      =>
        val v = cache.nextValue()
        write(c.codec, v.asInstanceOf[c.codec.Focus], id, registers, alwaysEncode, cache)
      case c: Enum[_]              => c.write(id, alwaysEncode, cache)
      case c: Optional[_]          => c.write(a, id, registers, cache)
      case c: Repeated[c, e]       => c.write(a, id, registers, cache)
      case c: RepeatedMap[c, k, v] => c.write(a, id, registers, cache)
      case c: Bytes.type           => c.write(a, id, alwaysEncode)
      case c: RecursiveMessage[_]  => c.codec.write(id, registers, cache)
    }

  private def finalize[A](m: Message[A], registers: Registers, offset: RegisterOffset, visitedBits: Long, visitedArray: Array[Boolean]): Unit = {
    var i = 0
    while (i < m.fields.length) {
      val wasVisited = if (m.useBitmask) (visitedBits & (1L << i)) != 0L else visitedArray(i)
      if (!wasVisited) {
        // set default values for not visited fields
        m.fields(i) match {
          case field: SimpleField[?]   =>
            if (field.defaultValue != null) setToRegister(registers, offset, field.register, field.defaultValue)
            else throw new ProteusException(s"Field ${field.name} in message ${m.name} is absent and has no default value")
          case field: OneOfField[?]    =>
            if (field.defaultValue != null) setToRegister(registers, offset, field.register, field.defaultValue)
            else throw new ProteusException(s"OneOf field ${field.name} in message ${m.name} is absent and has no default value")
          case field: ExcludedField[?] => setToRegister(registers, offset, field.register, field.defaultValue)
        }
      } else if (m.useBuilder) {
        // unpacked repeated fields use a builder that we need to convert to the final object
        m.fields(i) match {
          case field: SimpleField[_] if field.useBuilder =>
            def loop[A](codec: ProtobufCodec[A]): A =
              codec match {
                case c: Repeated[_, _]         =>
                  val v = field.register.asInstanceOf[Register.Object[_ <: AnyRef]].get(registers, offset)
                  c.constructor.result(v.asInstanceOf[c.constructor.Builder[Any]]).asInstanceOf[A]
                case c: RepeatedMap[_, _, _]   =>
                  val v = field.register.asInstanceOf[Register.Object[_ <: AnyRef]].get(registers, offset)
                  c.constructor.resultObject(v.asInstanceOf[c.constructor.ObjectBuilder[Any, Any]]).asInstanceOf[A]
                case Transform(from, _, codec) =>
                  val res = loop(codec)
                  // need to transform the result to the final type
                  if (res != null) from(res) else null.asInstanceOf[A]
                case _                         => null.asInstanceOf[A]
              }

            val res = loop(field.codec)
            if (res != null) setToRegister(registers, offset, field.register, res)
          case _                                         =>
        }
      }
      i += 1
    }
  }

  private def handleMessage[A](m: Message[A], registers: Registers, offset: RegisterOffset)(using input: CodedInputStream): A =
    wrapDecode(m.name) {
      var visitedBits: Long            = 0L
      val visitedArray: Array[Boolean] = if (m.useBitmask) null else new Array[Boolean](m.fields.length)
      val nextOffset                   = offset + m.constructor.usedRegisters

      def handleRepeated[C[_], E](r: Repeated[C, E], field: IndexedField, tag: Int, packed: Boolean, alreadyVisited: Boolean): C[E] = {
        val register = field.field.register.asInstanceOf[Register.Object[_ <: AnyRef]]
        val builder  =
          if (!alreadyVisited) {
            val builder = r.constructor.newBuilder[E]()(using r.elementClassTag)
            register.set(registers, offset, builder.asInstanceOf[register.Boxed])
            builder
          } else register.get(registers, offset).asInstanceOf[r.constructor.Builder[E]]
        if (packed) handlePackedRepeated(r, builder)
        else r.constructor.add(builder, loop(r.element, field, tag, alreadyVisited))
        null.asInstanceOf[C[E]]
      }

      def handleRepeatedMap[M[_, _], K, V](r: RepeatedMap[M, K, V], field: IndexedField, alreadyVisited: Boolean): M[K, V] = {
        val register = field.field.register.asInstanceOf[Register.Object[_ <: AnyRef]]
        val builder  =
          if (!alreadyVisited) {
            val builder = r.constructor.newObjectBuilder[K, V]()
            register.set(registers, offset, builder.asInstanceOf[register.Boxed])
            builder
          } else register.get(registers, offset).asInstanceOf[r.constructor.ObjectBuilder[K, V]]
        val (k, v)   = withLimit(handleMessage(r.element, registers, nextOffset))
        r.constructor.addObject(builder, k, v)
        null.asInstanceOf[M[K, V]]
      }

      def loop[A](codec: ProtobufCodec[A], field: IndexedField, tag: Int, alreadyVisited: Boolean): A =
        codec match {
          case c: Message[_]           => withLimit(handleMessage(c, registers, nextOffset))
          case c: Primitive[_]         => handlePrimitive(c)
          case c: Transform[_, _]      =>
            val res = loop(c.codec, field, tag, alreadyVisited)
            if (res == null) null.asInstanceOf[A] else c.from(res)
          case c: Enum[_]              => c.valueOrThrow(input.readEnum())
          case c: Optional[_]          => Some(loop(c.codec, field, tag, alreadyVisited))
          case c: Repeated[c, e]       => handleRepeated(c, field, tag, packed = c.packed && (tag & 0x7) == 2, alreadyVisited)
          case c: RepeatedMap[m, k, v] => handleRepeatedMap(c, field, alreadyVisited)
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
            val alreadyVisited = if (m.useBitmask) (visitedBits & (1L << field.index)) != 0L else visitedArray(field.index)
            val value          = wrapDecode(s"${field.field.name}#$fieldId") {
              loop(field.field.codec, field, tag, alreadyVisited)
            }
            if (m.useBitmask) visitedBits |= (1L << field.index) else visitedArray(field.index) = true
            if (value != null) setToRegister(registers, offset, field.field.register, value)
          } else input.skipField(tag): Unit
        }
      }
      finalize(m, registers, offset, visitedBits, visitedArray)
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
      case _                        => throw new ProteusException(s"Unsupported primitive type: $p")
    }

  private def handlePackedRepeated[C[_], E](r: Repeated[C, E], builder: r.constructor.Builder[E])(using input: CodedInputStream): Unit =
    withLimit {
      val getElement = r.element match {
        case c: Primitive[_]    =>
          c.primitiveType match {
            case _: PrimitiveType.Int     => () => r.constructor.addInt(builder, input.readInt32())
            case _: PrimitiveType.Long    => () => r.constructor.addLong(builder, input.readInt64())
            case _: PrimitiveType.Boolean => () => r.constructor.addBoolean(builder, input.readBool())
            case _: PrimitiveType.Double  => () => r.constructor.addDouble(builder, input.readDouble())
            case _: PrimitiveType.Float   => () => r.constructor.addFloat(builder, input.readFloat())
            case _                        => throw new ProteusException(s"Unsupported packed primitive type: $c")
          }
        case c: Enum[_]         => () => r.constructor.add(builder, c.valueOrThrow(input.readEnum()))
        case c: Transform[_, _] =>
          def loop[A](codec: ProtobufCodec[A]): () => A =
            codec match {
              case c: Primitive[_]    =>
                c.primitiveType match {
                  case _: PrimitiveType.Int     => () => input.readInt32()
                  case _: PrimitiveType.Long    => () => input.readInt64()
                  case _: PrimitiveType.Boolean => () => input.readBool()
                  case _: PrimitiveType.Double  => () => input.readDouble()
                  case _: PrimitiveType.Float   => () => input.readFloat()
                  case _                        => throw new ProteusException(s"Unsupported packed primitive type: $c")
                }
              case c: Enum[_]         => () => c.valueOrThrow(input.readEnum())
              case c: Transform[_, _] => () => c.from(loop(c.codec)())
              case _                  => throw new ProteusException(s"Invalid packed type: $c")
            }

          val getElement = loop(r.element)
          () => r.constructor.add(builder, getElement())
        case _                  => throw new ProteusException(s"Invalid packed type: ${r.element}")
      }
      while (input.getBytesUntilLimit > 0)
        getElement()
    }

  private def computeRootSize[A](codec: ProtobufCodec[A], a: A, registers: Registers, cache: WriterCache): Int =
    codec match {
      case c: Message[_]          => c.computeSize(a, -1, registers, cache)
      case c: RecursiveMessage[_] => c.codec.computeSize(a, -1, registers, cache)
      case c: Transform[_, _]     =>
        val v = c.to(a)
        cache.recordValue(v.asInstanceOf[AnyRef])
        computeRootSize(c.codec, v, registers, cache)
      case c: Enum[_]             => c.computeSize(a, -1, alwaysEncode = true, cache)
      case _                      => throw new ProteusException(s"Invalid root codec: $codec")
    }

  private def read[A](registers: Registers, offset: RegisterOffset, codec: ProtobufCodec[A])(using input: CodedInputStream): A = {
    def loop[A](codec: ProtobufCodec[A], offset: RegisterOffset): A =
      codec match {
        case c: Message[_]          => handleMessage(c, registers, offset)
        case c: RecursiveMessage[_] => handleMessage(c.codec, registers, offset)
        case c: Transform[_, _]     => c.from(loop(c.codec, offset))
        case c: Enum[_]             => c.valueOrThrow(input.readEnum())
        case _                      => throw new ProteusException(s"Invalid root codec: $codec")
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

  final private[proteus] def isOptional[A](using codec: ProtobufCodec[A]): Boolean =
    codec match {
      case c: Transform[_, _] => isOptional(using c.codec)
      case _: Optional[_]     => true
      case _                  => false
    }

  final private[proteus] def isRepeated[A](using codec: ProtobufCodec[A]): Boolean =
    codec match {
      case c: Transform[_, _]      => isRepeated(using c.codec)
      case _: Repeated[_, _]       => true
      case _: RepeatedMap[_, _, _] => true
      case _                       => false
    }

  /**
    * Converts the given codec to its protobuf IR representation.
    */
  final def toProtoIR(codec: ProtobufCodec[?]): List[ProtoIR.TopLevelDef] =
    relocateNestedIn(findTopLevelDefs(codec))

  private[proteus] def findTopLevelDefs[A](codec: ProtobufCodec[A]): List[ProtoIR.TopLevelDef] = {
    val visited = new mutable.HashSet[ProtobufCodec[?]]()

    def loop[A](codec: ProtobufCodec[A]): List[ProtoIR.TopLevelDef] =
      codec match {
        case c: Message[_]           =>
          if (visited.contains(c)) Nil
          else {
            if (!c.name.isEmpty) {
              visited.add(c): Unit
              if (c.nested.contains(NestedPlacement.Auto)) c.simpleFields.map(_.codec).flatMap(loop)
              else ProtoIR.TopLevelDef.MessageDef(c.toProtoIR) :: c.simpleFields.map(_.codec).flatMap(loop)
            } else c.simpleFields.map(_.codec).flatMap(loop)
          }
        case c: Transform[_, _]      => loop(c.codec)
        case c: Optional[_]          => loop(c.codec)
        case c: Repeated[_, _]       => loop(c.element)
        case c: RepeatedMap[_, _, _] => loop(c.element)
        case c: RecursiveMessage[_]  => loop(c.codec)
        case c: Enum[_]              =>
          if (visited.contains(c)) Nil
          else {
            visited.add(c)
            if (c.nested.contains(NestedPlacement.Auto)) Nil else List(ProtoIR.TopLevelDef.EnumDef(c.toProtoIR))
          }
        case _                       => Nil
      }

    loop(codec)
  }

  // Groups defs by name and returns those whose definitions render to more than one distinct shape.
  // Should be called on relocated defs so types nested in different parents (and only same by simple name)
  // aren't reported as conflicts.
  private[proteus] def conflictsOf(defs: Iterable[ProtoIR.TopLevelDef]): Map[String, List[String]] =
    defs
      .groupBy(_.name)
      .view
      .mapValues(_.map(Renderer.renderTopLevelDef).map(Text.renderText).toList.distinct)
      .toMap
      .filter((_, values) => values.length > 1)

  // Strips the message-level typeId so two structurally-identical defs that differ only by the
  // owning Scala type's TypeId (e.g. two case classes renamed to the same proto name) dedupe to one.
  private[proteus] def dedupKey(d: ProtoIR.TopLevelDef): ProtoIR.TopLevelDef =
    d match {
      case ProtoIR.TopLevelDef.MessageDef(m) => ProtoIR.TopLevelDef.MessageDef(m.copy(typeId = None))
      case ProtoIR.TopLevelDef.EnumDef(e)    => ProtoIR.TopLevelDef.EnumDef(e.copy(typeId = None))
      case other                             => other
    }

  private def resolvableNestedIn(d: ProtoIR.TopLevelDef, knownTypeIds: collection.Set[String]): Option[(String, String)] =
    for {
      childTid  <- d.typeId
      parentTid <- d.nestedIn if knownTypeIds.contains(parentTid)
    } yield childTid -> parentTid

  private[proteus] def relocateNestedIn(defs: List[ProtoIR.TopLevelDef]): List[ProtoIR.TopLevelDef] = {
    val typeIds          = defs.iterator.flatMap(_.typeId).toSet
    val childrenByTarget = mutable.Map.empty[String, mutable.ListBuffer[ProtoIR.TopLevelDef]]
    val childRefs        = mutable.Set.empty[ProtoIR.TopLevelDef]
    defs.foreach(d =>
      resolvableNestedIn(d, typeIds).foreach { case (_, parentTid) =>
        childrenByTarget.getOrElseUpdate(parentTid, mutable.ListBuffer.empty) += d
        childRefs += d
      }
    )

    def finalize(d: ProtoIR.TopLevelDef): ProtoIR.TopLevelDef = {
      val added = d.typeId.flatMap(childrenByTarget.get).toList.flatten.distinctBy(_.typeId).map(finalize).map {
        case ProtoIR.TopLevelDef.MessageDef(cm) =>
          ProtoIR.MessageElement.NestedMessageElement(cm.copy(placement = ProtoIR.Placement.Nested))
        case ProtoIR.TopLevelDef.EnumDef(ce)    =>
          ProtoIR.MessageElement.NestedEnumElement(ce.copy(placement = ProtoIR.Placement.Nested))
        case other                              =>
          throw new ProteusException(s"Cannot nest top-level def of kind $other")
      }
      d match {
        case ProtoIR.TopLevelDef.MessageDef(m) =>
          ProtoIR.TopLevelDef.MessageDef(m.copy(elements = added ++ m.elements))
        case other                             => other
      }
    }

    defs.flatMap(d => if (childRefs.contains(d)) None else Some(finalize(d)))
  }

  // Must be called BEFORE `relocateNestedIn`: once children are spliced, their `nestedIn` metadata is cleared.
  private[proteus] def nestedInPaths(defs: List[ProtoIR.TopLevelDef]): Map[String, String] = {
    val nameByTypeId     = defs.iterator.flatMap(d => d.typeId.map(_ -> d.name)).toMap
    val childToParentTid = defs.flatMap(resolvableNestedIn(_, nameByTypeId.keySet)).toMap

    def resolve(tid: String, chain: List[String] = Nil): String = {
      val name = nameByTypeId.getOrElse(tid, tid)
      childToParentTid.get(tid) match {
        case Some(parentTid) if chain.contains(parentTid) =>
          throw new ProteusException(s"Cyclic `nestedIn` detected: ${(chain :+ tid :+ parentTid).mkString(" -> ")}")
        case Some(parentTid)                              => s"${resolve(parentTid, chain :+ tid)}.$name"
        case None                                         => name
      }
    }

    childToParentTid.map { case (childTid, _) => childTid -> resolve(childTid) }
  }

  private[proteus] def qualifyReferences(
    defs: List[ProtoIR.TopLevelDef],
    paths: Map[String, String]
  ): List[ProtoIR.TopLevelDef] = {

    def qualify(name: String, refTypeId: Option[String], scope: String): String =
      refTypeId.flatMap(paths.get) match {
        case Some(path) =>
          val lastDot = path.lastIndexOf('.')
          if (lastDot < 0) name
          else {
            val parent  = path.substring(0, lastDot)
            val pl      = parent.length
            val inScope = scope == parent ||
              (scope.length > pl && scope.charAt(pl) == '.' && scope.startsWith(parent))
            if (inScope) name else path
          }
        case None       => name
      }

    def rewriteType(t: ProtoIR.Type, scope: String): ProtoIR.Type =
      t match {
        case r: ProtoIR.Type.RefType     =>
          val q = qualify(r.name, r.typeId, scope)
          if (q eq r.name) t else r.copy(name = q)
        case r: ProtoIR.Type.EnumRefType =>
          val q = qualify(r.name, r.typeId, scope)
          if (q eq r.name) t else r.copy(name = q)
        case ProtoIR.Type.ListType(v)    =>
          val nv = rewriteType(v, scope)
          if (nv eq v) t else ProtoIR.Type.ListType(nv)
        case ProtoIR.Type.MapType(k, v)  =>
          val nk = rewriteType(k, scope)
          val nv = rewriteType(v, scope)
          if ((nk eq k) && (nv eq v)) t else ProtoIR.Type.MapType(nk, nv)
        case _                           => t
      }

    def rewriteField(f: ProtoIR.Field, scope: String): ProtoIR.Field = {
      val nt = rewriteType(f.ty, scope)
      if (nt eq f.ty) f else f.copy(ty = nt)
    }

    def rewriteMessage(m: ProtoIR.Message, prefix: String): ProtoIR.Message = {
      val scope       = if (prefix.isEmpty) m.name else s"$prefix.${m.name}"
      val newElements = m.elements.map {
        case ProtoIR.MessageElement.FieldElement(f)          =>
          ProtoIR.MessageElement.FieldElement(rewriteField(f, scope))
        case ProtoIR.MessageElement.OneOfElement(o)          =>
          ProtoIR.MessageElement.OneOfElement(o.copy(fields = o.fields.map(rewriteField(_, scope))))
        case ProtoIR.MessageElement.NestedMessageElement(nm) =>
          ProtoIR.MessageElement.NestedMessageElement(rewriteMessage(nm, scope))
        case ne: ProtoIR.MessageElement.NestedEnumElement    => ne
      }
      m.copy(elements = newElements)
    }

    defs.map {
      case ProtoIR.TopLevelDef.MessageDef(m) => ProtoIR.TopLevelDef.MessageDef(rewriteMessage(m, ""))
      case other                             => other
    }
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
          case _                        => throw new ProteusException(s"Unsupported primitive type: $c")
        }
      case c: Message[_]           => ProtoIR.Type.RefType(c.name, c.typeId.map(_.fullName))
      case c: Enum[_]              => ProtoIR.Type.EnumRefType(c.name, c.typeId.map(_.fullName))
      case c: Repeated[_, _]       => ProtoIR.Type.ListType(toProtoType(c.element))
      case c: RepeatedMap[_, _, _] =>
        if (c.mapInProto) ProtoIR.Type.MapType(toProtoType(c.element.simpleFields.head.codec), toProtoType(c.element.simpleFields(1).codec))
        else ProtoIR.Type.ListType(toProtoType(c.element))
      case Bytes                   => ProtoIR.Type.Bytes
    }
}
