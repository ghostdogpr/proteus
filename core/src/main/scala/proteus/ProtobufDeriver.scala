package proteus

import scala.util.boundary
import scala.util.boundary.break

import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.*
import zio.blocks.schema.derive.*

import proteus.internal.*

object ProtobufDeriver extends Deriver[ProtobufCodec] {

  def derivePrimitive[F[_, _], A](
    primitiveType: PrimitiveType[A],
    typeName: TypeName[A],
    binding: Binding[BindingType.Primitive, A],
    doc: Doc,
    modifiers: Seq[Modifier.Primitive]
  ): Lazy[ProtobufCodec[A]] =
    Lazy(ProtobufCodec.Primitive(primitiveType))

  def deriveRecord[F[_, _], A](
    fields: IndexedSeq[Term[F, A, ?]],
    typeName: TypeName[A],
    binding: Binding[BindingType.Record, A],
    doc: Doc,
    modifiers: Seq[Modifier.Record]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] = {
    val shouldUnwrap  = false // record.annotations.collectFirst { case unwrap() => true }.getOrElse(false) && record.fields.length == 1
    val recordBinding = binding.asInstanceOf[Binding.Record[A]]
    val registers     = Reflect.Record.registers(fields.map(_.value).toArray)
    val offset        = Reflect.Record.usedRegisters(registers)
    if (shouldUnwrap) {
      val field = fields.head
      D.instance(field.value.metadata)
        .map(
          _.transformWith(
            (in, offset, res) => {
              setToRegister(in, offset, registers(0), res)
              recordBinding.constructor.construct(in, offset)
            },
            (out, offset, res) => {
              recordBinding.deconstructor.deconstruct(out, offset, res)
              getFromRegister(out, offset, registers(0).asInstanceOf[Register[field.Focus]])
            }
          )
        )
    } else {
      Lazy
        .collectAll(fields.map(field => D.instance(field.value.metadata).map(TermInstance(field, _))).toVector)
        .map { fieldsWithInstances =>
          val reservedIndexes = Set.empty[Int] // getReservedIndexes(record)
          val builder         = List.newBuilder[ProtobufCodec.MessageField[?]]
          var id              = 0

          def getField[A](index: Int, field: TermInstance[F, A] /*, annotations: List[Any]*/ ): Unit =
            field.instance match {
              case ProtobufCodec.Message(_, fields, _, _, _, _, true, _) =>
                val idIterator = Iterator.empty // annotations.collectFirst { case reserved(numbers*) => numbers.iterator }.getOrElse(Iterator.empty)
                fields.foreach { f =>
                  val caseId =
                    if (idIterator.hasNext) idIterator.next
                    else {
                      id += 1
                      while (reservedIndexes.contains(id)) id += 1
                      id
                    }
                  builder += f.copy(id = id, register = registers(index), oneOfName = Some(toSnakeCase(field.term.name)))
                }
              case instance                                              =>
                id += 1
                while (reservedIndexes.contains(id)) id += 1
                builder += ProtobufCodec.MessageField(
                  toSnakeCase(field.term.name),
                  id,
                  instance,
                  registers(index),
                  _.asInstanceOf[A],
                  getDefaultValue(using field.instance),
                  None
                )
            }

          var idx = 0
          val len = fields.length
          while (idx < len) {
            getField(idx, fieldsWithInstances(idx) /*, field.annotations*/ )
            idx += 1
          }
          ProtobufCodec.Message(
            typeName.name,
            builder.result(),
            recordBinding.constructor,
            recordBinding.deconstructor,
            offset,
            reservedIndexes,
            inline = false,
            nested = false // TODO
          )
        }
    }
  }

  def deriveVariant[F[_, _], A](
    cases: IndexedSeq[Term[F, A, ?]],
    typeName: TypeName[A],
    binding: Binding[BindingType.Variant, A],
    doc: Doc,
    modifiers: Seq[Modifier.Variant]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] =
    if (typeName == TypeName.option)
      D.instance(cases.find(_.name == TypeName.some.name).get.value.asRecord.get.fields.head.value.metadata)
        .map(ProtobufCodec.Optional(_).asInstanceOf[ProtobufCodec[A]])
    else if (isEnum(cases)) {
      val reservedIndexes = List.empty // TODO
      val builder         = List.newBuilder[ProtobufCodec.EnumValue[A]]
      var index           = 0
      cases.foreach { c =>
        while (reservedIndexes.contains(index)) index += 1
        val a = constructEnumCase(c).asInstanceOf[A]
        builder += ProtobufCodec.EnumValue(toUpperSnakeCase(c.name), index, a) // TODO: rename, prefix, etc.
        index += 1
      }
      Lazy(ProtobufCodec.Enum(typeName.name, builder.result(), reservedIndexes))
    } else {
      val inline        = false // TODO
      val discriminator = binding.asInstanceOf[Binding.Variant[A]].discriminator
      Lazy.collectAll(cases.map(c => D.instance(c.value.metadata).map(TermInstance(c, _))).toVector).map { casesWithInstances =>
        val reservedIndexes = Set.empty[Int] // TODO
        val builder         = List.newBuilder[ProtobufCodec.MessageField[?]]
        var id              = 1
        val register        = Register.Object(0)
        casesWithInstances.zipWithIndex.foreach { case (c, index) =>
          while (reservedIndexes.contains(id)) id += 1
          builder += ProtobufCodec.MessageField(
            toSnakeCase(c.term.name),
            id,
            c.instance,
            register.asInstanceOf[Register[Any]],
            a => if (discriminator.discriminate(a.asInstanceOf[A]) == index) a.asInstanceOf[c.term.Focus] else null.asInstanceOf[c.term.Focus],
            null,
            Some("value")
          )
          id += 1
        }
        ProtobufCodec.Message(
          typeName.name,
          builder.result(),
          new Constructor[A]   {
            def usedRegisters: RegisterOffset                           = register.usedRegisters
            def construct(in: Registers, baseOffset: RegisterOffset): A = register.get(in, baseOffset).asInstanceOf[A]
          },
          new Deconstructor[A] {
            def usedRegisters: RegisterOffset                                         = register.usedRegisters
            def deconstruct(registers: Registers, offset: RegisterOffset, a: A): Unit = register.set(registers, offset, a.asInstanceOf[AnyRef])
          },
          register.usedRegisters,
          reservedIndexes,
          inline = inline,
          nested = false // TODO
        )
      }
    }

  def deriveSequence[F[_, _], C[_], A](
    element: Reflect[F, A],
    typeName: TypeName[C[A]],
    binding: Binding[BindingType.Seq[C], C[A]],
    doc: Doc,
    modifiers: Seq[Modifier.Seq]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[C[A]]] = {
    val seqBinding = binding.asInstanceOf[Binding.Seq[C, A]]
    D.instance(element.metadata).map { instance =>
      ProtobufCodec.Repeated[C, A](
        instance,
        seqBinding.constructor,
        seqBinding.deconstructor,
        isPacked(element)
      )
    }
  }

  def deriveMap[F[_, _], M[_, _], K, V](
    key: Reflect[F, K],
    value: Reflect[F, V],
    typeName: TypeName[M[K, V]],
    binding: Binding[BindingType.Map[M], M[K, V]],
    doc: Doc,
    modifiers: Seq[Modifier.Map]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[M[K, V]]] = {
    val mapBinding    = binding.asInstanceOf[Binding.Map[M, K, V]]
    val registers     = Reflect.Record.registers(Array(key, value))
    val keyRegister   = registers(0)
    val valueRegister = registers(1)
    val offset        = Reflect.Record.usedRegisters(registers)
    val constructor   = new Constructor[(K, V)] {
      def usedRegisters: RegisterOffset                                = offset
      def construct(in: Registers, baseOffset: RegisterOffset): (K, V) =
        (
          getFromRegister(in, baseOffset, keyRegister.asInstanceOf[Register[K]]),
          getFromRegister(in, baseOffset, valueRegister.asInstanceOf[Register[V]])
        )
    }
    val deconstructor = new Deconstructor[(K, V)] {
      def usedRegisters: RegisterOffset                                             = offset
      def deconstruct(out: Registers, baseOffset: RegisterOffset, in: (K, V)): Unit = {
        setToRegister(out, baseOffset, keyRegister, in._1)
        setToRegister(out, baseOffset, valueRegister, in._2)
      }
    }
    D.instance(key.metadata).flatMap { keyInstance =>
      D.instance(value.metadata).map { valueInstance =>
        ProtobufCodec.RepeatedMap[M, K, V](
          ProtobufCodec.Message(
            "",
            List(
              ProtobufCodec.MessageField("key", 1, keyInstance, keyRegister, _.asInstanceOf[K], getDefaultValue(using keyInstance), None),
              ProtobufCodec.MessageField("value", 2, valueInstance, valueRegister, _.asInstanceOf[V], getDefaultValue(using valueInstance), None)
            ),
            constructor,
            deconstructor,
            offset,
            Set.empty,
            inline = false,
            nested = false
          ),
          mapBinding.constructor,
          mapBinding.deconstructor
        )
      }
    }
  }

  def deriveDynamic[F[_, _]](binding: Binding[BindingType.Dynamic, DynamicValue], doc: Doc, modifiers: Seq[Modifier.Dynamic])(
    implicit F: HasBinding[F],
    D: HasInstance[F]
  ): Lazy[ProtobufCodec[DynamicValue]] = ???

  private def isPacked(schema: Reflect[?, ?]): Boolean =
    schema match {
      case p: Reflect.Primitive[?, ?] =>
        p.primitiveType match {
          case _: PrimitiveType.Boolean | _: PrimitiveType.Float | _: PrimitiveType.Double | _: PrimitiveType.Int | _: PrimitiveType.Long =>
            true
          case _                                                                                                                          => false
        }
      case e: Reflect.Variant[?, ?]   => isEnum(e.cases)
      case _                          => false
    }

  private def isEnum(cases: IndexedSeq[Term[?, ?, ?]]): Boolean =
    cases.forall(c =>
      innerSchema(c.value) match {
        case record: Reflect.Record[?, ?] => record.fields.length == 0
        case _                            => false
      }
    ) // && !e.annotations.exists { case oneof() => true; case _ => false }

  private def innerSchema[F[_, _], A](schema: Reflect[F, A]): Reflect[F, A] =
    schema match {
      case Reflect.Deferred(value) => innerSchema(value())
      case _                       => schema
    }

  private def getDefaultValue[A](using codec: ProtobufCodec[A]): A =
    codec match {
      case ProtobufCodec.Primitive(primitiveType)                       =>
        primitiveType match {
          case _: PrimitiveType.Boolean => false
          case _: PrimitiveType.Float   => 0.0f
          case _: PrimitiveType.Double  => 0.0
          case _: PrimitiveType.Int     => 0
          case _: PrimitiveType.Long    => 0L
          case _: PrimitiveType.String  => ""
          case _                        => throw new Exception(s"Unsupported primitive type: $primitiveType")
        }
      case ProtobufCodec.Message(_, fields, constructor, _, _, _, _, _) =>
        boundary {
          val registers = Registers(constructor.usedRegisters)
          fields.foreach { field =>
            val defaultValue = getDefaultValue(using field.codec)
            if (defaultValue == null) break(null.asInstanceOf[A])
            setToRegister(registers, RegisterOffset.Zero, field.register, defaultValue)
          }
          constructor.construct(registers, RegisterOffset.Zero)
        }
      case ProtobufCodec.Optional(_)                                    =>
        None.asInstanceOf[A]
      case e: ProtobufCodec.Enum[A]                                     =>
        e.valuesByIndex.getOrElse(0, null.asInstanceOf[A])
      case ProtobufCodec.RepeatedMap(_, constructor, _)                 =>
        constructor.resultObject(constructor.newObjectBuilder()).asInstanceOf[A]
      case ProtobufCodec.Repeated(_, constructor, _, _)                 =>
        constructor.resultObject(constructor.newObjectBuilder()).asInstanceOf[A]
      case ProtobufCodec.Transform(from, _, codec)                      =>
        val offset    = RegisterOffset.Zero
        val registers = Registers(offset)
        from(registers, offset, getDefaultValue(using codec))
    }

  private def constructEnumCase[F[_, _], A](c: Term[F, ?, A])(using hasBinding: HasBinding[F]): A =
    innerSchema(c.value) match {
      case r: Reflect.Record[F, _] if r.fields.isEmpty =>
        hasBinding.record(r.metadata).constructor.construct(Registers(RegisterOffset.Zero), RegisterOffset.Zero)
      case _                                           => throw new Exception(s"Unsupported enum case: $c")
    }

  private def toSnakeCase(s: String): String =
    s.split("(?=[A-Z])").map(_.toLowerCase).mkString("_")

  private def toUpperSnakeCase(s: String): String =
    s.split("(?=[A-Z])").map(_.toUpperCase).mkString("_")

  case class TermInstance[F[_, _], A](term: Term[F, ?, A], instance: ProtobufCodec[A])
}
