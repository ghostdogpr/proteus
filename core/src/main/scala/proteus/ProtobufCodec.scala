package proteus

import java.io.*
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.immutable.HashMap
import scala.collection.mutable

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

import proteus.ProtobufCodec.MessageField.*
import proteus.internal.*
import proteus.internal.FieldMap.FieldMapEntry

sealed trait ProtobufCodec[A] {
  type Focus = A

  import ProtobufCodec.*

  def encode(value: A): Array[Byte] =
    withRegisters { registers =>
      val writer = toProtoWriter(this, value, -1, registers, RegisterOffset.Zero, alwaysEncode = true)
      val bytes  = new Array[Byte](internal.ProtobufWriter.innerSize(writer))
      val output = CodedOutputStream.newInstance(bytes)
      internal.ProtobufWriter.write(writer)(using output)
      bytes
    }

  def decode(bytes: Array[Byte]): A =
    decode(CodedInputStream.newInstance(bytes))

  def decode(inputStream: InputStream): A =
    decode(CodedInputStream.newInstance(inputStream))

  def decode(input: CodedInputStream): A =
    withRegisters { registers =>
      read(registers, RegisterOffset.Zero, this)(using input)
    }

  def transform[B](from: A => B, to: B => A): ProtobufCodec[B] =
    this match {
      case c: Transform[a0, A] =>
        Transform[a0, B](a => from(c.from(a)), b => c.to(to(b)), c.codec)
      case _                   =>
        Transform(from, to, this)
    }

  private[proteus] def makeNested: ProtobufCodec[A] =
    this match {
      case message: Message[_]        => message.copy(nested = true)
      case Transform(from, to, codec) => Transform(from, to, codec.makeNested)
      case RecursiveMessage(thunk)    => RecursiveMessage(() => thunk().copy(nested = true))
      case Optional(codec)            => Optional(codec.makeNested)
      case _                          => this
    }

  private[proteus] def getName: Option[String] =
    this match {
      case message: Message[_]     => Some(message.name)
      case Transform(_, _, codec)  => codec.getName
      case RecursiveMessage(thunk) => Some(thunk().name)
      case Optional(codec)         => codec.getName
      case _                       => None
    }
}

object ProtobufCodec {
  def apply[A](using codec: ProtobufCodec[A]): ProtobufCodec[A] = codec

  inline def derived[T](using deriver: ProtobufDeriver, schema: Schema[T]): ProtobufCodec[T] =
    schema.derive(deriver)

  private val pool = new ThreadLocal[(Registers, AtomicBoolean)] {
    override def initialValue(): (Registers, AtomicBoolean) = (Registers(RegisterOffset.Zero), new AtomicBoolean(false))
  }

  private[proteus] inline def withRegisters[A](f: Registers => A): A = {
    val (registers, running) = pool.get()
    if (running.compareAndSet(false, true))
      try f(registers)
      finally running.set(false)
    else f(Registers(RegisterOffset.Zero))
  }

  sealed trait MessageField[A] {
    def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter
    def toProtoIR: ProtoIR.MessageElement.FieldElement | ProtoIR.MessageElement.OneofElement
  }

  object MessageField {
    final case class SimpleField[A](
      name: String,
      id: Int,
      codec: ProtobufCodec[A],
      register: Register[Any],
      defaultValue: Any,
      comment: Option[String] = None
    ) extends MessageField[A] {
      def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter = {
        val res = getFromRegister(registers, offset, register).asInstanceOf[A]
        ProtobufCodec.toProtoWriter(codec, res, id, registers, nextOffset, alwaysEncode = false)
      }

      def toProtoIR: ProtoIR.MessageElement.FieldElement = {
        val field = ProtoIR.Field(toProtoType(codec), name, id, deprecated = false, optional = isOptional(using codec), comment = comment)
        ProtoIR.MessageElement.FieldElement(field)
      }

      val mayUseBuilder: Boolean = {
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

    final case class OneofField[A](name: String, cases: Array[SimpleField[?]], register: Register[Any], discriminator: Discriminator[A])
      extends MessageField[A] {
      def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter = {
        val res   = getFromRegister(registers, offset, register).asInstanceOf[A]
        val field = cases(discriminator.discriminate(res))
        ProtobufCodec.toProtoWriter(field.codec, res.asInstanceOf[field.codec.Focus], field.id, registers, nextOffset, alwaysEncode = true)
      }

      def toProtoIR: ProtoIR.MessageElement.OneofElement = {
        val fields = cases
          .map(field => ProtoIR.Field(toProtoType(field.codec), field.name, field.id, deprecated = false, optional = isOptional(using field.codec)))
          .toList
          .sortBy(_.number)
        ProtoIR.MessageElement.OneofElement(ProtoIR.Oneof(name, fields))
      }
    }

    final case class ExcludedField[A](register: Register[Any], defaultValue: Any) extends MessageField[A] {
      def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter = null
      def toProtoIR: ProtoIR.MessageElement.FieldElement                                                          =
        ProtoIR.MessageElement.FieldElement(ProtoIR.excludedField)
    }
  }

  final case class Primitive[A](primitiveType: PrimitiveType[A]) extends ProtobufCodec[A] {
    def toProtoWriter(a: A, id: Int, alwaysEncode: Boolean): ProtobufWriter =
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

  case class EnumValue[A](name: String, index: Int, value: A)

  final case class Enum[A](name: String, values: List[EnumValue[A]], reserved: List[Int], comment: Option[String] = None, nested: Boolean)
    extends ProtobufCodec[A] {
    val valuesByIndex: HashMap[Int, A]   = HashMap.from(values.map(v => (v.index, v.value)))
    val indexesByValue: HashMap[A, Int]  = HashMap.from(values.map(v => (v.value, v.index)))
    val namesByValue: HashMap[A, String] = HashMap.from(values.map(v => (v.value, v.name)))

    def toProtoWriter(a: A, id: Int, alwaysEncode: Boolean): ProtobufWriter = {
      val index = indexesByValue(a)
      if (index == 0 && !alwaysEncode) null else internal.ProtobufWriter.IntPrimitive(index, id)
    }

    def toProtoIR: ProtoIR.Enum =
      ProtoIR.Enum(
        name,
        values.sortBy(_.index).map(v => ProtoIR.EnumValue(v.name, v.index)).toList,
        reserved = reserved.toList.sorted.map(ProtoIR.Reserved.Number(_)),
        comment = comment
      )
  }

  final case class Message[A](
    name: String,
    fields: Array[MessageField[?]],
    constructor: Constructor[A],
    deconstructor: Deconstructor[A],
    usedRegisters: RegisterOffset,
    reserved: Set[Int],
    inline: Boolean,
    nested: Boolean,
    comment: Option[String] = None
  ) extends ProtobufCodec[A] {
    val simpleFields: List[SimpleField[?]] = fields.toList.flatMap {
      case f: SimpleField[?]   => List(f)
      case f: OneofField[?]    => f.cases.toList
      case f: ExcludedField[?] => Nil
    }
    val fieldMap: FieldMap                 = FieldMap(HashMap.from(fields.zipWithIndex.flatMap {
      case (f: SimpleField[?], idx)   => List(f.id -> FieldMapEntry(f, idx))
      case (f: OneofField[?], idx)    => f.cases.map(c => c.id -> FieldMapEntry(c, idx)).toList
      case (f: ExcludedField[?], idx) => Nil
    }))
    val mayUseBuilder: Boolean             = simpleFields.exists(_.mayUseBuilder)

    def toProtoWriter(a: A, id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter = {
      deconstructor.deconstruct(registers, offset, a)
      val nextOffset = RegisterOffset.add(offset, usedRegisters)
      val builder    = List.newBuilder[ProtobufWriter]
      var i          = 0
      while (i < fields.length) {
        val res = fields(i).toProtoWriter(registers, offset, nextOffset)
        if (res ne null) builder += res
        i += 1
      }
      internal.ProtobufWriter.Message(id, builder.result())
    }

    def toProtoIR: ProtoIR.Message = {
      val elements = fields.map(_.toProtoIR)

      def findNested[A](codec: ProtobufCodec[A], goDeep: Boolean = false): List[ProtoIR.MessageElement] =
        codec match {
          case c: Message[_]           =>
            if (c.nested) List(ProtoIR.MessageElement.NestedMessageElement(c.toProtoIR))
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
        case c: ProtoIR.MessageElement.OneofElement => c.oneof.fields.head.number
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

  final case class Repeated[C[_], E](
    element: ProtobufCodec[E],
    constructor: SeqConstructor[C],
    deconstructor: SeqDeconstructor[C],
    packed: Boolean
  ) extends ProtobufCodec[C[E]] {
    def toElementProtoWriter[A](codec: ProtobufCodec[A]): (Int, Registers, RegisterOffset) => A => ProtobufWriter =
      codec match {
        case c: Primitive[_]        => (id, _, _) => c.toProtoWriter(_, id, alwaysEncode = true)
        case c: Message[_]          => (id, registers, offset) => c.toProtoWriter(_, id, registers, offset)
        case c: Enum[_]             => (id, _, _) => c.toProtoWriter(_, id, alwaysEncode = true)
        case c: Transform[_, _]     => (id, registers, offset) => a => toElementProtoWriter(c.codec)(id, registers, offset)(c.to(a))
        case c: Optional[_]         =>
          (id, registers, offset) => {
            case None        => null
            case Some(value) => toElementProtoWriter(c.codec)(id, registers, offset)(value)
          }
        case c: RecursiveMessage[_] => (id, registers, offset) => c.codec.toProtoWriter(_, id, registers, offset)
        case _                      => throw new Exception(s"Invalid codec inside repeated: $codec")
      }

    val elementProtoWriter: (Int, Registers, RegisterOffset) => E => ProtobufWriter =
      toElementProtoWriter(element)

    def toProtoWriter(a: C[E], id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter = {
      val it = deconstructor.deconstruct(a)
      if (it.isEmpty && !alwaysEncode) null
      else {
        val builder         = List.newBuilder[ProtobufWriter]
        val makeProtoWriter = elementProtoWriter(if (packed) -1 else id, registers, offset)
        while (it.hasNext) {
          val res = makeProtoWriter(it.next)
          if (res ne null) builder += res
        }
        internal.ProtobufWriter.Repeated(builder.result(), id, packed)
      }
    }
  }

  final case class RepeatedMap[C[_, _], K, V](
    element: Message[(K, V)],
    constructor: MapConstructor[C],
    deconstructor: MapDeconstructor[C]
  ) extends ProtobufCodec[C[K, V]] {
    def toProtoWriter(a: C[K, V], id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter = {
      val it = deconstructor.deconstruct(a)
      if (it.isEmpty && !alwaysEncode) null
      else {
        val builder = List.newBuilder[ProtobufWriter]
        while (it.hasNext) {
          val kv  = it.next
          val res = element.toProtoWriter((deconstructor.getKey(kv), deconstructor.getValue(kv)), id, registers, offset)
          if (res ne null) builder += res
        }
        internal.ProtobufWriter.Repeated(builder.result(), id, packed = false)
      }
    }
  }

  case object Bytes extends ProtobufCodec[Array[Byte]] {
    def toProtoWriter(a: Array[Byte], id: Int, alwaysEncode: Boolean): ProtobufWriter =
      if (a.isEmpty && !alwaysEncode) null else internal.ProtobufWriter.Bytes(a, id)
  }

  final case class Transform[A, B](from: A => B, to: B => A, codec: ProtobufCodec[A]) extends ProtobufCodec[B] {
    type Origin = A

    def toProtoWriter(b: B, id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter =
      ProtobufCodec.toProtoWriter(codec, to(b), id, registers, offset, alwaysEncode)
  }

  final case class RecursiveMessage[A](thunk: () => Message[A]) extends ProtobufCodec[A] {
    lazy val codec = thunk()

    def toProtoWriter(a: A, id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter =
      codec.toProtoWriter(a, id, registers, offset)
  }

  final case class Optional[A](codec: ProtobufCodec[A]) extends ProtobufCodec[Option[A]] {
    def toProtoWriter(a: Option[A], id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter = a match {
      case None        => null
      case Some(value) => ProtobufCodec.toProtoWriter(codec, value, id, registers, offset, alwaysEncode = true)
    }
  }

  def toProtoWriter[A](codec: ProtobufCodec[A], a: A, id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter =
    codec match {
      case c: Primitive[_]         => c.toProtoWriter(a, id, alwaysEncode)
      case c: Message[_]           => c.toProtoWriter(a, id, registers, offset)
      case c: Repeated[c, e]       => c.toProtoWriter(a, id, registers, offset, alwaysEncode)
      case c: RepeatedMap[c, k, v] => c.toProtoWriter(a, id, registers, offset, alwaysEncode)
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
          case field: OneofField[?]    => setToRegister(registers, offset, field.register, null)
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

  private def handleMessage[A](m: Message[A], registers: Registers, offset: RegisterOffset)(using input: CodedInputStream): A = {
    val visited    = new Array[Boolean](m.fields.length)
    val nextOffset = RegisterOffset.add(offset, m.constructor.usedRegisters)

    def handleRepeated[C[_], E](r: Repeated[C, E], field: FieldMapEntry, tag: Int): C[E] = {
      val register = field.field.register
      val builder  =
        if (!visited(field.index)) {
          val builder = r.constructor.newObjectBuilder[E]()
          setToRegister(registers, offset, register, builder)
          builder
        } else getFromRegister(registers, offset, register).asInstanceOf[r.constructor.ObjectBuilder[E]]
      r.constructor.addObject(builder, loop(r.element, field, tag))
      null.asInstanceOf[C[E]]
    }

    def handleRepeatedMap[M[_, _], K, V](r: RepeatedMap[M, K, V], field: FieldMapEntry): M[K, V] = {
      val register = field.field.register
      val builder  =
        if (!visited(field.index)) {
          val builder = r.constructor.newObjectBuilder[K, V]()
          setToRegister(registers, offset, register, builder)
          builder
        } else getFromRegister(registers, offset, register).asInstanceOf[r.constructor.ObjectBuilder[K, V]]
      val (k, v)   = withLimit(handleMessage(r.element, registers, nextOffset))
      r.constructor.addObject(builder, k, v)
      null.asInstanceOf[M[K, V]]
    }

    def loop[A](codec: ProtobufCodec[A], field: FieldMapEntry, tag: Int): A =
      codec match {
        case c: Message[_]           => withLimit(handleMessage(c, registers, nextOffset))
        case c: Primitive[_]         => handlePrimitive(c)
        case c: Enum[_]              => c.valuesByIndex(input.readEnum())
        case c: Transform[_, _]      =>
          val res = loop(c.codec, field, tag)
          if (res == null) null.asInstanceOf[A] else c.from(res)
        case c: Optional[_]          => Some(loop(c.codec, field, tag))
        case c: Repeated[c, e]       =>
          if (c.packed && (tag & 0x7) == 2) handlePackedRepeated(c, nextOffset)
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
        val field   = m.fieldMap.get(fieldId)
        if (field ne null) {
          val value = loop(field.field.codec, field, tag)
          visited(field.index) = true
          if (value != null) setToRegister(registers, offset, field.field.register.asInstanceOf[Register[Any]], value)
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

  private def handlePackedRepeated[C[_], E](r: Repeated[C, E], offset: RegisterOffset)(using input: CodedInputStream): C[E] = {
    def loop[A](codec: ProtobufCodec[A], offset: RegisterOffset): () => A =
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
        case c: Transform[_, _] => () => c.from(loop(c.codec, offset)())
        case _                  => throw new Exception(s"Invalid packed type: $codec")
      }

    val builder = r.constructor.newObjectBuilder[E]()
    withLimit {
      val getElement = loop(r.element, offset)
      while (input.getBytesUntilLimit > 0)
        r.constructor.addObject(builder, getElement())
    }
    r.constructor.resultObject(builder)
  }

  def read[A](registers: Registers, offset: RegisterOffset, codec: ProtobufCodec[A])(using input: CodedInputStream): A = {
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

  private def isOptional[A](using codec: ProtobufCodec[A]): Boolean =
    codec match {
      case c: Transform[_, _] => isOptional(using c.codec)
      case _: Optional[_]     => true
      case _                  => false
    }

  def toProtoIR(codec: ProtobufCodec[?]): List[ProtoIR.TopLevelDef] = {
    val visited = new mutable.HashSet[ProtobufCodec[?]]()

    def findTopLevelDefs[A](codec: ProtobufCodec[A]): List[ProtoIR.TopLevelDef] =
      codec match {
        case c: Message[_]           =>
          if (visited.contains(c)) Nil
          else {
            if (!c.name.isEmpty) {
              visited.add(c): Unit
              if (c.nested) c.simpleFields.map(_.codec).flatMap(findTopLevelDefs)
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
