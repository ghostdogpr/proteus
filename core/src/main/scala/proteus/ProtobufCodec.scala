package proteus

import java.io.*
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.immutable.HashMap
import scala.collection.mutable

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

import proteus.internal.*

sealed trait ProtobufCodec[A] {
  import ProtobufCodec.*

  def encode(value: A): Array[Byte] =
    withRegisters { registers =>
      val writer = ProtobufCodec.toProtoWriter(this, value, -1, registers, RegisterOffset.Zero, alwaysEncode = true)
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
      ProtobufCodec.read(registers, RegisterOffset.Zero, this)(using input)
    }

  def transform[B](from: A => B, to: B => A): ProtobufCodec[B] =
    this match {
      case t: ProtobufCodec.Transform[a0, A] =>
        ProtobufCodec.Transform[a0, B](a => from(t.from(a)), b => t.to(to(b)), t.codec)
      case _                                 =>
        ProtobufCodec.Transform(from, to, this)
    }
}

object ProtobufCodec {
  private val pool = new ThreadLocal[(Registers, AtomicBoolean)] {
    override def initialValue(): (Registers, AtomicBoolean) = (Registers(RegisterOffset.Zero), new AtomicBoolean(false))
  }

  private def withRegisters[A](f: Registers => A): A = {
    val (registers, running) = pool.get()
    if (running.compareAndSet(false, true))
      try f(registers)
      finally running.set(false)
    else f(Registers(RegisterOffset.Zero))
  }

  final case class MessageField[A](
    name: String,
    id: Int,
    codec: ProtobufCodec[A],
    register: Register[Any],
    refine: Any => A,
    defaultValue: Any,
    oneOfName: Option[String]
  ) {
    def toProtoWriter(registers: Registers, offset: RegisterOffset, nextOffset: RegisterOffset): ProtobufWriter = {
      val res = refine(getFromRegister(registers, offset, register))
      if (res == null) null else ProtobufCodec.toProtoWriter(codec, res, id, registers, nextOffset, alwaysEncode = oneOfName.isDefined)
    }

    def toProtoIR: ProtoIR.MessageElement.OneofElement | ProtoIR.MessageElement.FieldElement = {
      val field = ProtoIR.Field(toProtoType(codec), name, id, deprecated = false, optional = isOptional(using codec))
      oneOfName match {
        case Some(oneOfName) => ProtoIR.MessageElement.OneofElement(ProtoIR.Oneof(oneOfName, List(field)))
        case None            => ProtoIR.MessageElement.FieldElement(field)
      }
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

  final case class Enum[A](name: String, values: List[EnumValue[A]], reserved: List[Int]) extends ProtobufCodec[A] {
    val valuesByIndex: HashMap[Int, A]  = HashMap.from(values.map(v => (v.index, v.value)))
    val indexesByValue: HashMap[A, Int] = HashMap.from(values.map(v => (v.value, v.index)))

    def toProtoWriter(a: A, id: Int, alwaysEncode: Boolean): ProtobufWriter = {
      val index = indexesByValue(a)
      if (index == 0 && !alwaysEncode) null else internal.ProtobufWriter.IntPrimitive(index, id)
    }

    def toProtoIR: ProtoIR.Enum =
      ProtoIR.Enum(
        name,
        values.map(v => ProtoIR.EnumValue(v.name, v.index)).toList,
        reserved = reserved.toList.sorted.map(ProtoIR.Reserved.Number(_))
      )
  }

  final case class Message[A](
    name: String,
    fields: List[MessageField[?]],
    constructor: Constructor[A],
    deconstructor: Deconstructor[A],
    usedRegisters: RegisterOffset,
    reserved: Set[Int],
    inline: Boolean,
    nested: Boolean
  ) extends ProtobufCodec[A] {
    val fieldMap: FieldMap = FieldMap(HashMap.from(fields.map(f => f.id -> f)))

    def toProtoWriter(a: A, id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter = {
      deconstructor.deconstruct(registers, offset, a)
      val nextOffset = RegisterOffset.add(offset, usedRegisters)
      val builder    = List.newBuilder[ProtobufWriter]
      var remaining  = fields
      while (remaining ne Nil) {
        val res = remaining.head.toProtoWriter(registers, offset, nextOffset)
        if (res ne null) builder += res
        remaining = remaining.tail
      }
      internal.ProtobufWriter.Message(id, builder.result())
    }

    def toProtoIR: ProtoIR.Message = {
      val elements = fields.map(_.toProtoIR)

      val (fieldElements, oneofs) = elements.partitionMap {
        case e: ProtoIR.MessageElement.FieldElement     => Left(e)
        case ProtoIR.MessageElement.OneofElement(oneof) => Right(oneof)
      }
      val oneofElements           =
        oneofs
          .groupMap(_.name)(_.fields)
          .map { case (name, fields) =>
            ProtoIR.MessageElement.OneofElement(ProtoIR.Oneof(name, fields.flatten.sortBy(_.number)))
          }
          .toList

      def findNested[A](codec: ProtobufCodec[A]): List[ProtoIR.MessageElement.NestedMessageElement] =
        codec match {
          case m: ProtobufCodec.Message[_]           => if (m.nested) List(ProtoIR.MessageElement.NestedMessageElement(m.toProtoIR)) else Nil
          case t: ProtobufCodec.Transform[_, _]      => findNested(t.codec)
          case o: ProtobufCodec.Optional[_]          => findNested(o.codec)
          case r: ProtobufCodec.Repeated[_, _]       => findNested(r.element)
          case r: ProtobufCodec.RepeatedMap[_, _, _] => findNested(r.element)
          case d: ProtobufCodec.Deferred[_]          => findNested(d.codec)
          case _                                     => Nil
        }

      val nestedMessageElements = fields.collect(field => findNested(field.codec)).flatten.distinct

      val allElements: List[ProtoIR.MessageElement.OneofElement | ProtoIR.MessageElement.FieldElement] = fieldElements ++ oneofElements
      val sortedAllElements                                                                            = allElements.sortBy {
        case o: ProtoIR.MessageElement.OneofElement => o.oneof.fields.head.number
        case f: ProtoIR.MessageElement.FieldElement => f.field.number
      }
      ProtoIR.Message(name, nestedMessageElements ++ sortedAllElements, reserved = reserved.toList.sorted.map(ProtoIR.Reserved.Number(_)))
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
        case p: ProtobufCodec.Primitive[_]    => (id, _, _) => p.toProtoWriter(_, id, alwaysEncode = true)
        case p: ProtobufCodec.Message[_]      => (id, registers, offset) => p.toProtoWriter(_, id, registers, offset)
        case p: ProtobufCodec.Enum[_]         => (id, _, _) => p.toProtoWriter(_, id, alwaysEncode = true)
        case p: ProtobufCodec.Transform[_, _] => (id, registers, offset) => a => toElementProtoWriter(p.codec)(id, registers, offset)(p.to(a))
        case p: ProtobufCodec.Optional[_]     =>
          (id, registers, offset) => {
            case None        => null
            case Some(value) => toElementProtoWriter(p.codec)(id, registers, offset)(value)
          }
        case d: ProtobufCodec.Deferred[_]     => toElementProtoWriter(d.codec)
        case _                                => throw new Exception(s"Invalid codec inside repeated: $codec")
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
    element: ProtobufCodec.Message[(K, V)],
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

  final case class Deferred[A](thunk: () => ProtobufCodec[A]) extends ProtobufCodec[A] {
    lazy val codec = thunk()

    def toProtoWriter(a: A, id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter =
      ProtobufCodec.toProtoWriter(codec, a, id, registers, offset, alwaysEncode)
  }

  final case class Optional[A](codec: ProtobufCodec[A]) extends ProtobufCodec[Option[A]] {
    def toProtoWriter(a: Option[A], id: Int, registers: Registers, offset: RegisterOffset): ProtobufWriter = a match {
      case None        => null
      case Some(value) => ProtobufCodec.toProtoWriter(codec, value, id, registers, offset, alwaysEncode = true)
    }
  }

  def toProtoWriter[A](codec: ProtobufCodec[A], a: A, id: Int, registers: Registers, offset: RegisterOffset, alwaysEncode: Boolean): ProtobufWriter =
    codec match {
      case p: ProtobufCodec.Primitive[_]         => p.toProtoWriter(a, id, alwaysEncode)
      case p: ProtobufCodec.Message[_]           => p.toProtoWriter(a, id, registers, offset)
      case p: ProtobufCodec.Repeated[c, e]       => p.toProtoWriter(a, id, registers, offset, alwaysEncode)
      case p: ProtobufCodec.RepeatedMap[c, k, v] => p.toProtoWriter(a, id, registers, offset, alwaysEncode)
      case p: ProtobufCodec.Enum[_]              => p.toProtoWriter(a, id, alwaysEncode)
      case p: ProtobufCodec.Transform[_, _]      => p.toProtoWriter(a, id, registers, offset, alwaysEncode)
      case p: ProtobufCodec.Optional[_]          => p.toProtoWriter(a, id, registers, offset)
      case p: ProtobufCodec.Bytes.type           => p.toProtoWriter(a, id, alwaysEncode)
      case d: ProtobufCodec.Deferred[_]          => d.toProtoWriter(a, id, registers, offset, alwaysEncode)
    }

  private val defaultCompleteBuilders: () => Unit = () => ()

  def read[A](registers: Registers, offset: RegisterOffset, codec: ProtobufCodec[A])(using input: CodedInputStream): A = {
    def handlePrimitive[A](p: Primitive[A])(using input: CodedInputStream): A =
      p.primitiveType match {
        case _: PrimitiveType.Int     => input.readInt32()
        case _: PrimitiveType.Long    => input.readInt64()
        case _: PrimitiveType.Boolean => input.readBool()
        case _: PrimitiveType.String  => input.readStringRequireUtf8()
        case _: PrimitiveType.Double  => input.readDouble()
        case _: PrimitiveType.Float   => input.readFloat()
        case _                        => throw new Exception(s"Unsupported primitive type: $p")
      }

    def setDefaults[A](m: Message[A], offset: RegisterOffset): Unit = {
      var fields = m.fields
      while (fields ne Nil) {
        val field = fields.head
        setToRegister(registers, offset, field.register, field.defaultValue)
        fields = fields.tail
      }
    }

    def handleMessage[A](m: Message[A], offset: RegisterOffset): A = {
      setDefaults(m, offset)

      val nextOffset       = RegisterOffset.add(offset, m.constructor.usedRegisters)
      var completeBuilders = defaultCompleteBuilders

      def handleRepeated[C[_], E](r: Repeated[C, E], field: MessageField[?]): C[E] = {
        val register     = field.register.asInstanceOf[Register[Any]]
        val currentValue = getFromRegister(registers, offset, register)
        val builder =
          // safe cast because both the default value and a builder are objects
          if (currentValue.asInstanceOf[AnyRef] eq field.defaultValue.asInstanceOf[AnyRef]) {
            val builder  = r.constructor.newObjectBuilder[E]()
            setToRegister(registers, offset, register, builder)
            val previous = completeBuilders
            completeBuilders = () => { previous(); setToRegister(registers, offset, register, r.constructor.resultObject(builder)) }
            builder
          } else currentValue.asInstanceOf[r.constructor.ObjectBuilder[E]]
        r.constructor.addObject(builder, loop(r.element, field))
        null.asInstanceOf[C[E]]
      }

      def handleRepeatedMap[M[_, _], K, V](r: RepeatedMap[M, K, V], field: MessageField[?]): M[K, V] = {
        val register     = field.register.asInstanceOf[Register[Any]]
        val currentValue = getFromRegister(registers, offset, register)
        val builder =
          // safe cast because both the default value and a builder are objects
          if (currentValue.asInstanceOf[AnyRef] eq field.defaultValue.asInstanceOf[AnyRef]) {
            val builder  = r.constructor.newObjectBuilder[K, V]()
            setToRegister(registers, offset, register, builder)
            val previous = completeBuilders
            completeBuilders = () => { previous(); setToRegister(registers, offset, register, r.constructor.resultObject(builder)) }
            builder
          } else currentValue.asInstanceOf[r.constructor.ObjectBuilder[K, V]]
        val (k, v)       = withLimit(handleMessage(r.element, nextOffset))
        r.constructor.addObject(builder, k, v)
        null.asInstanceOf[M[K, V]]
      }

      def loop[A](codec: ProtobufCodec[A], field: MessageField[?]): A =
        codec match {
          case m: Message[_]           => withLimit(handleMessage(m, nextOffset))
          case p: Primitive[_]         => handlePrimitive(p)
          case e: Enum[_]              => e.valuesByIndex(input.readEnum())
          case m: Transform[_, _]      => m.from(loop(m.codec, field))
          case o: Optional[_]          => Some(loop(o.codec, field))
          case r: Repeated[c, e]       =>
            if (r.packed && (input.getLastTag() & 0x7) == 2) handlePackedRepeated(r, nextOffset)
            else handleRepeated(r, field)
          case r: RepeatedMap[m, k, v] => handleRepeatedMap(r, field)
          case b: Bytes.type           => input.readByteArray()
          case d: Deferred[_]          => loop(d.codec, field)
        }

      var done = false
      while (!done) {
        val tag = input.readTag()
        if (tag == 0) done = true
        else {
          val fieldId = tag >>> 3
          val field   = m.fieldMap.get(fieldId)
          if (field ne null) {
            val value = loop(field.codec, field)
            if (value != null) setToRegister(registers, offset, field.register.asInstanceOf[Register[Any]], value)
          } else input.skipField(tag): Unit
        }
      }
      completeBuilders()
      m.constructor.construct(registers, offset)
    }

    def handlePackedRepeated[C[_], E](r: Repeated[C, E], offset: RegisterOffset): C[E] = {
      def loop[A](codec: ProtobufCodec[A], offset: RegisterOffset): () => A =
        codec match {
          case p: Primitive[_]    =>
            p.primitiveType match {
              case _: PrimitiveType.Int     => () => input.readInt32()
              case _: PrimitiveType.Long    => () => input.readInt64()
              case _: PrimitiveType.Boolean => () => input.readBool()
              case _: PrimitiveType.Double  => () => input.readDouble()
              case _: PrimitiveType.Float   => () => input.readFloat()
              case _                        => throw new Exception(s"Unsupported packed primitive type: $p")
            }
          case e: Enum[_]         => () => e.valuesByIndex(input.readEnum())
          case m: Transform[_, _] =>
            val getElement = loop(m.codec, offset)
            () => m.from(getElement())
          case d: Deferred[_]     =>
            val getElement = loop(d.codec, offset)
            () => getElement()
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

    def loop[A](codec: ProtobufCodec[A], offset: RegisterOffset): A =
      codec match {
        case m: Message[_]      => handleMessage(m, offset)
        case m: Transform[_, _] => m.from(loop(m.codec, offset))
        case d: Deferred[_]     => loop(d.codec, offset)
        case _                  => throw new Exception(s"Invalid root codec: $codec")
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
      case t: ProtobufCodec.Transform[_, _] => isOptional(using t.codec)
      case _: ProtobufCodec.Optional[_]     => true
      case d: ProtobufCodec.Deferred[_]     => isOptional(using d.codec)
      case _                                => false
    }

  def toProtoIR(codec: ProtobufCodec[?]): List[ProtoIR.TopLevelDef] = {
    val visited = new mutable.HashSet[String]()

    def findTopLevelDefs[A](codec: ProtobufCodec[A]): List[ProtoIR.TopLevelDef] =
      codec match {
        case m: ProtobufCodec.Message[_]           =>
          if (m.nested || visited.contains(m.name)) Nil
          else {
            if (!m.name.isEmpty) {
              visited.add(m.name): Unit
              ProtoIR.TopLevelDef.MessageDef(m.toProtoIR) :: m.fields.map(_.codec).flatMap(findTopLevelDefs)
            } else m.fields.map(_.codec).flatMap(findTopLevelDefs)
          }
        case t: ProtobufCodec.Transform[_, _]      => findTopLevelDefs(t.codec)
        case o: ProtobufCodec.Optional[_]          => findTopLevelDefs(o.codec)
        case r: ProtobufCodec.Repeated[_, _]       => findTopLevelDefs(r.element)
        case r: ProtobufCodec.RepeatedMap[_, _, _] => findTopLevelDefs(r.element)
        case d: ProtobufCodec.Deferred[_]          => findTopLevelDefs(d.codec)
        case e: ProtobufCodec.Enum[_]              =>
          if (visited.contains(e.name)) Nil
          else {
            visited.add(e.name)
            List(ProtoIR.TopLevelDef.EnumDef(e.toProtoIR))
          }
        case _                                     => Nil
      }

    findTopLevelDefs(codec)
  }

  private def toProtoType(codec: ProtobufCodec[?]): ProtoIR.Type =
    codec match {
      case t: ProtobufCodec.Transform[_, _]      => toProtoType(t.codec)
      case o: ProtobufCodec.Optional[_]          => toProtoType(o.codec)
      case d: ProtobufCodec.Deferred[_]          => toProtoType(d.codec)
      case p: ProtobufCodec.Primitive[_]         =>
        p.primitiveType match {
          case _: PrimitiveType.Int     => ProtoIR.Type.Int32
          case _: PrimitiveType.Long    => ProtoIR.Type.Int64
          case _: PrimitiveType.Boolean => ProtoIR.Type.Bool
          case _: PrimitiveType.String  => ProtoIR.Type.String
          case _: PrimitiveType.Double  => ProtoIR.Type.Double
          case _: PrimitiveType.Float   => ProtoIR.Type.Float
          case _                        => throw new Exception(s"Unsupported primitive type: $p")
        }
      case m: ProtobufCodec.Message[_]           => ProtoIR.Type.RefType(ProtoIR.Fqn(None, m.name))
      case e: ProtobufCodec.Enum[_]              => ProtoIR.Type.EnumRefType(ProtoIR.Fqn(None, e.name))
      case r: ProtobufCodec.Repeated[_, _]       => ProtoIR.Type.ListType(toProtoType(r.element))
      case m: ProtobufCodec.RepeatedMap[_, _, _] =>
        ProtoIR.Type.MapType(toProtoType(m.element.fields(0).codec), toProtoType(m.element.fields(1).codec))
      case p: ProtobufCodec.Bytes.type           => ProtoIR.Type.Bytes
    }
}
