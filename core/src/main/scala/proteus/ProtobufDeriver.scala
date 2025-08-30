package proteus

import scala.util.boundary
import scala.util.boundary.break

import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.*
import zio.blocks.schema.binding.SeqConstructor.*
import zio.blocks.schema.derive.*

import proteus.ProtobufDeriver.*
import proteus.internal.*

class ProtobufDeriver(flags: Set[DerivationFlag] = Set.empty) extends Deriver[ProtobufCodec] {

  val unwrapModifier = Modifier.config("proteus.unwrap", "true")
  val inlineModifier = Modifier.config("proteus.inline", "true")
  val nestedModifier = Modifier.config("proteus.nested", "true")
  val oneofModifier  = Modifier.config("proteus.oneof", "true")

  private val instanceCache  = java.util.concurrent.ConcurrentHashMap[TypeName[?], ProtobufCodec[Any]]()
  private val visitedCache   = new ThreadLocal[java.util.IdentityHashMap[TypeName[?], Unit]] {
    override def initialValue: java.util.IdentityHashMap[TypeName[?], Unit] = new java.util.IdentityHashMap[TypeName[?], Unit]
  }
  private val recursiveCache = new ThreadLocal[java.util.IdentityHashMap[TypeName[?], Unit]] {
    override def initialValue: java.util.IdentityHashMap[TypeName[?], Unit] = new java.util.IdentityHashMap[TypeName[?], Unit]
  }

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
    val visited   = visitedCache.get
    val recursive = recursiveCache.get
    if (visited.containsKey(typeName)) {
      recursive.put(typeName, ())
      Lazy(ProtobufCodec.Deferred(() => instanceCache.get(typeName)).asInstanceOf[ProtobufCodec[A]])
    } else {
      visited.put(typeName, ())
      val shouldUnwrap  = modifiers.collectFirst { case `unwrapModifier` => true }.getOrElse(false) && fields.length == 1
      val recordBinding = binding.asInstanceOf[Binding.Record[A]]
      val registers     = Reflect.Record.registers(fields.map(_.value).toArray)
      val offset        = Reflect.Record.usedRegisters(registers)
      (if (shouldUnwrap) {
         val field  = fields.head
         val offset = RegisterOffset.Zero
         val reg    = Registers(offset)
         D.instance(field.value.metadata)
           .map(
             _.transform(
               a => {
                 setToRegister(reg, offset, registers(0), a)
                 recordBinding.constructor.construct(reg, offset)
               },
               b => {
                 recordBinding.deconstructor.deconstruct(reg, offset, b)
                 getFromRegister(reg, offset, registers(0).asInstanceOf[Register[field.Focus]])
               }
             )
           )
       } else {
         Lazy
           .collectAll(fields.map(field => D.instance(field.value.metadata).map(TermInstance(field.name, field.modifiers, _))).toVector)
           .map { fieldsWithInstances =>
             val reservedIndexes = getReservedIndexes(modifiers)
             val nested          = modifiers.collectFirst { case `nestedModifier` => true }.getOrElse(false)
             val builder         = List.newBuilder[ProtobufCodec.MessageField[?]]
             var id              = 0

             def getField[A](index: Int, field: TermInstance[A]): Unit =
               field.instance match {
                 case t @ ProtobufCodec.Transform(from, to, ProtobufCodec.Message(_, fields, _, _, _, _, true, _)) =>
                   val idIterator = getReservedIndexes(field.modifiers).iterator
                   fields.foreach { f =>
                     val caseId =
                       if (idIterator.hasNext) idIterator.next
                       else {
                         id += 1
                         while (reservedIndexes.contains(id)) id += 1
                         id
                       }
                     builder += ProtobufCodec.MessageField[A](
                       f.name,
                       caseId,
                       ProtobufCodec.Transform[t.Origin, A](t.from, t.to, f.codec.asInstanceOf[ProtobufCodec[t.Origin]]),
                       registers(index),
                       any => {
                         val a2 = f.refine(to(any.asInstanceOf[A])).asInstanceOf[t.Origin]
                         if (a2 == null.asInstanceOf[t.Origin]) null.asInstanceOf[A] else from(a2)
                       },
                       f.defaultValue,
                       Some(toSnakeCase(field.name))
                     )
                   }
                 case ProtobufCodec.Message(_, fields, _, _, _, _, true, _)                                        =>
                   val idIterator = getReservedIndexes(field.modifiers).iterator
                   fields.foreach { f =>
                     val caseId =
                       if (idIterator.hasNext) idIterator.next
                       else {
                         id += 1
                         while (reservedIndexes.contains(id)) id += 1
                         id
                       }
                     builder += f.copy(id = caseId, register = registers(index), oneOfName = Some(toSnakeCase(field.name)))
                   }
                 case ProtobufCodec.Optional(codec) if flags.contains(DerivationFlag.OptionalAsOneOf)              =>
                   // add empty case
                   id += 1
                   while (reservedIndexes.contains(id)) id += 1
                   builder += ProtobufCodec.MessageField[A](
                     s"no_${toSnakeCase(field.name)}",
                     id,
                     Empty.emptyCodec.transform(_ => None, _ => Empty()),
                     registers(index),
                     v => if (v == None) None else null,
                     null,
                     Some(toSnakeCase(field.name))
                   )
                   // add value case
                   id += 1
                   while (reservedIndexes.contains(id)) id += 1
                   builder += ProtobufCodec.MessageField[A](
                     s"${toSnakeCase(field.name)}_value",
                     id,
                     codec.transform(Some(_), _.get),
                     registers(index),
                     v => if (v.isInstanceOf[Some[?]]) v.asInstanceOf[A] else null.asInstanceOf[A],
                     null,
                     Some(toSnakeCase(field.name))
                   )
                 case instance                                                                                     =>
                   id += 1
                   while (reservedIndexes.contains(id)) id += 1
                   builder += ProtobufCodec.MessageField(
                     toSnakeCase(field.name),
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
               getField(idx, fieldsWithInstances(idx))
               idx += 1
             }
             ProtobufCodec.Message(
               getTypeName(typeName.name, modifiers),
               builder.result(),
               recordBinding.constructor,
               recordBinding.deconstructor,
               offset,
               reservedIndexes,
               inline = false,
               nested = nested
             )
           }
       }).map { codec =>
        visited.remove(typeName)
        if (recursive.containsKey(typeName)) instanceCache.put(typeName, codec.asInstanceOf[ProtobufCodec[Any]]): Unit
        codec
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
    if (typeName.name == unitOption.name && typeName.namespace == unitOption.namespace)
      D.instance(cases.find(c => c.name == unitSome.name).get.value.asRecord.get.fields.head.value.metadata)
        .map(ProtobufCodec.Optional(_).asInstanceOf[ProtobufCodec[A]])
    else if (isEnum(cases, modifiers)) {
      val reservedIndexes = getReservedIndexes(modifiers).toList
      val builder         = List.newBuilder[ProtobufCodec.EnumValue[A]]
      var index           = 0
      cases.foreach { c =>
        while (reservedIndexes.contains(index)) index += 1
        val a        = constructEnumCase(c).asInstanceOf[A]
        val prefix   = getEnumPrefix(modifiers)
        val enumName = if (prefix.nonEmpty) s"${prefix.toUpperCase}_${toUpperSnakeCase(c.name)}" else toUpperSnakeCase(c.name)
        builder += ProtobufCodec.EnumValue(enumName, index, a)
        index += 1
      }
      Lazy(ProtobufCodec.Enum(getTypeName(typeName.name, modifiers), builder.result(), reservedIndexes))
    } else {
      val inline        = modifiers.collectFirst { case `inlineModifier` => true }.getOrElse(false)
      val nested        = modifiers.collectFirst { case `nestedModifier` => true }.getOrElse(false)
      val discriminator = binding.asInstanceOf[Binding.Variant[A]].discriminator
      Lazy.collectAll(cases.map(c => D.instance(c.value.metadata).map(TermInstance(c.name, c.modifiers, _))).toVector).map { casesWithInstances =>
        val reservedIndexes = getReservedIndexes(modifiers)
        val builder         = List.newBuilder[ProtobufCodec.MessageField[?]]
        var id              = 1
        val register        = Register.Object(0)
        casesWithInstances.zipWithIndex.foreach { case (c, index) =>
          while (reservedIndexes.contains(id)) id += 1
          builder += ProtobufCodec.MessageField(
            toSnakeCase(c.name),
            id,
            c.instance,
            register.asInstanceOf[Register[Any]],
            a => if (discriminator.discriminate(a.asInstanceOf[A]) == index) a.asInstanceOf[c.Focus] else null.asInstanceOf[c.Focus],
            null,
            Some("value")
          )
          id += 1
        }
        ProtobufCodec.Message(
          getTypeName(typeName.name, modifiers),
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
          nested = nested
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
      val isByteArray = seqBinding.constructor match {
        case _: ArrayConstructor =>
          instance match {
            case ProtobufCodec.Primitive(_: PrimitiveType.Byte) => true
            case _                                              => false
          }
        case _                   => false
      }
      if (isByteArray) ProtobufCodec.Bytes.asInstanceOf[ProtobufCodec[C[A]]]
      else ProtobufCodec.Repeated[C, A](instance, seqBinding.constructor, seqBinding.deconstructor, isPacked(element))
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
  ): Lazy[ProtobufCodec[DynamicValue]] = Lazy.fail(new Exception("Dynamic is not supported"))

  def deriveWrapper[F[_, _], A, B](
    wrapped: Reflect[F, B],
    typeName: TypeName[A],
    binding: Binding[BindingType.Wrapper[A, B], A],
    doc: Doc,
    modifiers: Seq[Modifier.Wrapper]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] = {
    val wrapperBinding = binding.asInstanceOf[Binding.Wrapper[A, B]]
    D.instance(wrapped.metadata).map { wrappedCodec =>
      wrappedCodec.transform(
        (b: B) =>
          wrapperBinding.wrap(b) match {
            case Right(a)    => a
            case Left(error) => throw new Exception(s"Wrapper conversion failed: $error")
          },
        (a: A) => wrapperBinding.unwrap(a)
      )
    }
  }

  private def isPacked(schema: Reflect[?, ?]): Boolean =
    schema match {
      case p: Reflect.Primitive[?, ?] =>
        p.primitiveType match {
          case _: PrimitiveType.Boolean | _: PrimitiveType.Float | _: PrimitiveType.Double | _: PrimitiveType.Int | _: PrimitiveType.Long =>
            true
          case _                                                                                                                          => false
        }
      case e: Reflect.Variant[?, ?]   => isEnum(e.cases, e.modifiers)
      case _                          => false
    }

  private def getTypeName(originalName: String, modifiers: Seq[Modifier]): String =
    modifiers
      .collectFirst { case Modifier.config("proteus.rename", newName) => newName }
      .getOrElse(originalName)

  private def getReservedIndexes(modifiers: Seq[Modifier]): Set[Int] =
    modifiers
      .collectFirst { case Modifier.config("proteus.reserved", value) => value.split(",").map(_.trim.toInt).toSet }
      .getOrElse(Set.empty)

  private def getEnumPrefix(modifiers: Seq[Modifier]): String =
    modifiers
      .collectFirst { case Modifier.config("proteus.enum.prefix", prefix) => prefix }
      .getOrElse("")

  private def isEnum(cases: IndexedSeq[Term[?, ?, ?]], modifiers: Seq[Modifier]): Boolean =
    cases.forall(c =>
      innerSchema(c.value) match {
        case record: Reflect.Record[?, ?] => record.fields.length == 0
        case _                            => false
      }
    ) && !modifiers.exists { case `oneofModifier` => true; case _ => false }

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
        from(getDefaultValue(using codec))
      case d: ProtobufCodec.Deferred[A]                                 =>
        getDefaultValue(using d.codec)
      case ProtobufCodec.Bytes                                          =>
        Array.empty[Byte]
    }

  private def constructEnumCase[F[_, _], A](c: Term[F, ?, A])(using hasBinding: HasBinding[F]): A =
    innerSchema(c.value) match {
      case r: Reflect.Record[F, _] if r.fields.isEmpty =>
        hasBinding.record(r.metadata).constructor.construct(Registers(RegisterOffset.Zero), RegisterOffset.Zero)
      case _                                           => throw new Exception(s"Unsupported enum case: $c")
    }

  private val unitOption = TypeName.option(TypeName.unit)
  private val unitSome   = TypeName.some(TypeName.unit)
}

object ProtobufDeriver {
  enum DerivationFlag {
    case OptionalAsOneOf
  }

  case class TermInstance[A](name: String, modifiers: Seq[Modifier], instance: ProtobufCodec[A]) {
    type Focus = A
  }
}
