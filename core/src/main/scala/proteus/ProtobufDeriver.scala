package proteus

import scala.compiletime.*
import scala.deriving.Mirror

import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.*
import zio.blocks.schema.binding.SeqConstructor.*
import zio.blocks.schema.derive.*

import proteus.ProtobufCodec.MessageField.*
import proteus.ProtobufDeriver.*
import proteus.internal.*

/**
  * A deriver for creating protobuf codecs from Scala types.
  *
  * @param flags the flags to use for the derivation.
  * @param instances custom codec instances
  * @param modifiers custom modifiers
  */
case class ProtobufDeriver private (flags: Set[DerivationFlag], instances: Vector[InstanceOverride], modifiers: Vector[ModifierOverride])
  extends Deriver[ProtobufCodec] {

  /**
    * Adds a custom codec instance for the given type.
    */
  def instance[B: TypeName](instance: => ProtobufCodec[B]): ProtobufDeriver =
    copy(instances = instances :+ InstanceOverrideByType(summon[TypeName[B]], Lazy(instance)))

  /**
    * Adds a custom modifier for the given type.
    */
  def modifier[B: TypeName](modifier: Modifier.Reflect): ProtobufDeriver =
    copy(modifiers = modifiers :+ ModifierReflectOverrideByType(summon[TypeName[B]], modifier))

  /**
    * Adds a custom modifier for a term (case class field or enum member) of the given type.
    *
    * @param termName the name of the term to apply the modifier to (there will be a compile error if the term does not exist)
    * @param modifier the modifier to apply.
    */
  inline def modifier[B: TypeName](termName: String, modifier: Modifier.Term): ProtobufDeriver = {
    inline summonInline[Mirror.Of[B]] match {
      case m: Mirror.ProductOf[B] =>
        inline if (!constValue[proteus.Tuple.Contains[m.MirroredElemLabels, termName.type]]) {
          error("Field " + constValue[termName.type] + " does not exist in class " + constValue[m.MirroredLabel] + ".")
        }
      case m: Mirror.SumOf[B]     =>
        inline if (!constValue[proteus.Tuple.Contains[m.MirroredElemLabels, termName.type]]) {
          error("Case " + constValue[termName.type] + " does not exist in sealed trait or enum " + constValue[m.MirroredLabel] + ".")
        }
    }
    copy(modifiers = modifiers :+ ModifierTermOverrideByType(summon[TypeName[B]], termName, modifier))
  }

  /**
    * Enables a derivation flag.
    */
  def enable(flag: DerivationFlag): ProtobufDeriver =
    copy(flags = flags + flag)

  /**
    * Disables a derivation flag.
    */
  def disable(flag: DerivationFlag): ProtobufDeriver =
    copy(flags = flags - flag)

  override def instanceOverrides: IndexedSeq[InstanceOverride] = instances

  override def modifierOverrides: IndexedSeq[ModifierOverride] = modifiers

  private val instanceCache  = java.util.concurrent.ConcurrentHashMap[TypeName[?], ProtobufCodec.Message[Any]]()
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
    modifiers: Seq[Modifier.Reflect]
  ): Lazy[ProtobufCodec[A]] =
    Lazy(ProtobufCodec.Primitive(primitiveType))

  def deriveRecord[F[_, _], A](
    fields: IndexedSeq[Term[F, A, ?]],
    typeName: TypeName[A],
    binding: Binding[BindingType.Record, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] =
    Lazy {
      val visited   = visitedCache.get
      val recursive = recursiveCache.get
      if (visited.containsKey(typeName)) {
        recursive.put(typeName, ())
        Lazy(ProtobufCodec.RecursiveMessage(() => instanceCache.get(typeName).asInstanceOf[ProtobufCodec.Message[A]]))
      } else {
        visited.put(typeName, ())
        val recordBinding = binding.asInstanceOf[Binding.Record[A]]
        val registers     = Reflect.Record.registers(fields.map(_.value).toArray)
        val offset        = Reflect.Record.usedRegisters(registers)

        Lazy
          .collectAll(fields.map(field => D.instance(field.value.metadata).map(TermInstance(field, _))).toVector)
          .map { fieldsWithInstances =>
            val reservedIndexes    = getReservedIndexes(modifiers).toSet
            val allReservedIndexes = reservedIndexes ++ getReservedIndexes(fieldsWithInstances.flatMap(_.term.modifiers)).toSet
            val nested             = modifiers.collectFirst { case Modifier.config(`nestedModifier`, value) => value.toBooleanOption }.flatten
            val builder            = Array.newBuilder[ProtobufCodec.MessageField[?]]
            var id                 = 0

            def getField[A](index: Int, field: TermInstance[F, A]): Unit =
              if (!field.term.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false }) {
                val name     = getFieldName(field.term.name, field.term.modifiers)
                val register = registers(index)
                field.instance match {
                  case t @ ProtobufCodec.Transform(from, to, ProtobufCodec.Message(_, Array(o: OneOfField[inner]), _, _, _, _, true, _, _)) =>
                    val idIterator = getReservedIndexes(field.term.modifiers).iterator
                    builder += OneOfField(
                      name,
                      o.cases.map { field =>
                        val caseId =
                          if (idIterator.hasNext) idIterator.next
                          else {
                            id += 1
                            while (allReservedIndexes.contains(id)) id += 1
                            id
                          }
                        field.copy(
                          id = caseId,
                          codec = ProtobufCodec.Transform(from, to, field.codec.asInstanceOf[ProtobufCodec[t.Origin]]),
                          register = register,
                          defaultValue =
                            try from(field.defaultValue.asInstanceOf[t.Origin])
                            catch { case _: Exception => null.asInstanceOf[A] }
                        )
                      },
                      register,
                      new Discriminator[A] {
                        def discriminate(a: A): Int = o.discriminator.discriminate(to(a).asInstanceOf[inner])
                      },
                      try from(o.defaultValue.asInstanceOf[t.Origin])
                      catch { case _: Exception => null.asInstanceOf[A] },
                      o.comment
                    )
                  case ProtobufCodec.Message(_, Array(o: OneOfField[?]), _, _, _, _, true, _, _)                                            =>
                    val idIterator = getReservedIndexes(field.term.modifiers).iterator
                    builder += OneOfField(
                      name,
                      o.cases.map { field =>
                        val caseId =
                          if (idIterator.hasNext) idIterator.next
                          else {
                            id += 1
                            while (allReservedIndexes.contains(id)) id += 1
                            id
                          }
                        field.copy(id = caseId, register = register)
                      },
                      register,
                      o.discriminator,
                      o.defaultValue,
                      o.comment
                    )
                  case ProtobufCodec.Optional(codec) if flags.contains(DerivationFlag.OptionalAsOneOf)                                      =>
                    id += 1
                    while (allReservedIndexes.contains(id)) id += 1
                    val emptyId = id
                    id += 1
                    while (allReservedIndexes.contains(id)) id += 1
                    val valueId = id
                    val empty   = Empty()
                    builder += OneOfField(
                      getFieldName(field.term.name, field.term.modifiers),
                      Array(
                        SimpleField(s"no_$name", emptyId, Empty.emptyCodec.transform(_ => None, _ => empty), register, None, None),
                        SimpleField(s"${name}_value", valueId, codec.transform(Some(_), _.get), register, null, None)
                      ),
                      register,
                      new Discriminator[A] {
                        def discriminate(a: A): Int = a match {
                          case None    => 0
                          case Some(_) => 1
                        }
                      },
                      None,
                      getComment(field.term.modifiers)
                    )
                  case instance                                                                                                             =>
                    val fieldId = getReservedIndex(field.term.modifiers) match {
                      case Some(reservedIndex) => reservedIndex
                      case None                =>
                        id += 1
                        while (allReservedIndexes.contains(id)) id += 1
                        id
                    }
                    builder += SimpleField(
                      name,
                      fieldId,
                      instance,
                      register,
                      getDefaultValue(field.term.value).getOrElse(getDefaultValue(using field.instance)),
                      getComment(field.term.modifiers)
                    )
                }
              } else
                builder += ExcludedField(registers(index), getDefaultValue(field.term.value).getOrElse(getDefaultValue(using field.instance)))

            var idx   = 0
            val len   = fields.length
            while (idx < len) {
              try
                getField(idx, fieldsWithInstances(idx))
              catch {
                case e: Exception =>
                  throw new Exception(s"Error deriving field ${fieldsWithInstances(idx).term.name} of type ${typeName.name}", e)
              }
              idx += 1
            }
            val codec = ProtobufCodec.Message(
              getTypeName(typeName, modifiers),
              builder.result(),
              recordBinding.constructor,
              recordBinding.deconstructor,
              offset,
              reservedIndexes,
              inline = false,
              nested = nested,
              comment = getComment(modifiers)
            )

            visited.remove(typeName)
            if (recursive.containsKey(typeName)) instanceCache.put(typeName, codec.asInstanceOf[ProtobufCodec.Message[Any]]): Unit
            codec
          }
      }
    }.flatten

  def deriveVariant[F[_, _], A](
    cases: IndexedSeq[Term[F, A, ?]],
    typeName: TypeName[A],
    binding: Binding[BindingType.Variant, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] = {
    val filteredCases = cases.filterNot(c =>
      c.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false } ||
        c.value.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false }
    )
    if (typeName.name == unitOption.name && typeName.namespace == unitOption.namespace)
      D.instance(filteredCases.find(c => c.name == unitSome.name).get.value.asRecord.get.fields.head.value.metadata)
        .map(ProtobufCodec.Optional(_).asInstanceOf[ProtobufCodec[A]])
    else if (isEnum(filteredCases, modifiers)) {
      val nested             = modifiers.collectFirst { case Modifier.config(`nestedModifier`, "true") => true }.getOrElse(false)
      val reservedIndexes    = getReservedIndexes(modifiers).toSet
      val allReservedIndexes = reservedIndexes ++ filteredCases.flatMap(c => getReservedIndexes(c.modifiers).toSet)
      val builder            = List.newBuilder[ProtobufCodec.EnumValue[A]]
      var index              = 0
      filteredCases.foreach { c =>
        val fieldIndex = getReservedIndex(c.modifiers) match {
          case Some(id) => id
          case None     =>
            while (allReservedIndexes.contains(index)) index += 1
            val current = index
            index += 1
            current
        }
        val a          = constructEnumCase(c).asInstanceOf[A]
        val prefix     = getEnumPrefix(modifiers, flags, typeName)
        val suffix     = getEnumSuffix(modifiers, flags, typeName)
        val enumName   = getEnumMemberName(c.name, c.modifiers, prefix, suffix)
        builder += ProtobufCodec.EnumValue(enumName, fieldIndex, a, getComment(c.modifiers))
      }
      Lazy(ProtobufCodec.Enum(getTypeName(typeName, modifiers), builder.result(), reservedIndexes.toList, nested = nested, getComment(modifiers)))
    } else
      Lazy {
        val visited   = visitedCache.get
        val recursive = recursiveCache.get
        if (visited.containsKey(typeName)) {
          recursive.put(typeName, ())
          Lazy(ProtobufCodec.RecursiveMessage(() => instanceCache.get(typeName).asInstanceOf[ProtobufCodec.Message[A]]))
        } else {
          visited.put(typeName, ())

          val nested        = modifiers.collectFirst { case Modifier.config(`nestedModifier`, value) => value.toBooleanOption }.flatten
          val inlineOneOf   = modifiers.collectFirst { case Modifier.config(`oneOfModifier`, value) => value.contains("inline") }.getOrElse(false)
          val nestedOneOf   = modifiers
            .collectFirst { case Modifier.config(`oneOfModifier`, value) => value.contains("nested") }
            .getOrElse(flags.contains(DerivationFlag.NestedOneOf))
          val variant       = binding.asInstanceOf[Binding.Variant[A]]
          val discriminator = variant.discriminator
          Lazy.collectAll(filteredCases.map(c => D.instance(c.value.metadata).map(TermInstance(c, _))).toVector).map { casesWithInstances =>
            val reservedIndexes    = getReservedIndexes(modifiers).toSet
            val allReservedIndexes = reservedIndexes ++ getReservedIndexes(casesWithInstances.flatMap(_.term.modifiers)).toSet
            val builder            = Array.newBuilder[ProtobufCodec.MessageField[?]]
            var id                 = 1
            val register           = Register.Object(0)
            builder += OneOfField(
              "value",
              casesWithInstances.zipWithIndex.map { case (c, index) =>
                val fieldId = getReservedIndex(c.term.modifiers) match {
                  case Some(reservedIndex) => reservedIndex
                  case None                =>
                    while (allReservedIndexes.contains(id)) id += 1
                    val current = id
                    id += 1
                    current
                }
                val item    = SimpleField(
                  getFieldName(c.term.name, c.term.modifiers),
                  fieldId,
                  if (nestedOneOf) c.instance.makeNested else c.instance,
                  register.asInstanceOf[Register[Any]],
                  null.asInstanceOf[c.instance.Focus],
                  getComment(c.term.modifiers)
                )
                item
              }.toArray,
              register.asInstanceOf[Register[Any]],
              discriminator,
              variant.defaultValue.fold(null.asInstanceOf[A])(_()),
              getComment(modifiers)
            )
            val codec              = ProtobufCodec.Message(
              getTypeName(typeName, modifiers),
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
              inline = inlineOneOf,
              nested = nested,
              comment = getComment(modifiers)
            )

            visited.remove(typeName)
            if (recursive.containsKey(typeName)) instanceCache.put(typeName, codec.asInstanceOf[ProtobufCodec.Message[Any]]): Unit
            codec
          }
        }
      }.flatten
  }

  def deriveSequence[F[_, _], C[_], A](
    element: Reflect[F, A],
    typeName: TypeName[C[A]],
    binding: Binding[BindingType.Seq[C], C[A]],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect]
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
      else {
        if (ProtobufCodec.isOptional(using instance))
          throw new Exception(s"Unsupported usage of optional inside repeated type $typeName")
        if (ProtobufCodec.isRepeated(using instance))
          throw new Exception(s"Unsupported usage of repeated inside repeated type $typeName")
        ProtobufCodec.Repeated[C, A](instance, seqBinding.constructor, seqBinding.deconstructor, isPacked(instance))
      }
    }
  }

  def deriveMap[F[_, _], M[_, _], K, V](
    key: Reflect[F, K],
    value: Reflect[F, V],
    typeName: TypeName[M[K, V]],
    binding: Binding[BindingType.Map[M], M[K, V]],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect]
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
        val mapInProto = isValidKeyType(keyInstance) && isValidValueTypeForMap(valueInstance)
        ProtobufCodec.RepeatedMap[M, K, V](
          ProtobufCodec.Message(
            if (mapInProto) "" else s"${getTypeName(key.typeName, Nil)}${getTypeName(value.typeName, Nil)}Entry",
            Array(
              SimpleField("key", 1, keyInstance, keyRegister, getDefaultValue(key).getOrElse(getDefaultValue(using keyInstance)), None),
              SimpleField("value", 2, valueInstance, valueRegister, getDefaultValue(value).getOrElse(getDefaultValue(using valueInstance)), None)
            ),
            constructor,
            deconstructor,
            offset,
            Set.empty,
            inline = false,
            nested = if (mapInProto) None else Some(true),
            comment = None
          ),
          mapBinding.constructor,
          mapBinding.deconstructor,
          mapInProto
        )
      }
    }
  }

  def deriveDynamic[F[_, _]](binding: Binding[BindingType.Dynamic, DynamicValue], doc: Doc, modifiers: Seq[Modifier.Reflect])(
    implicit F: HasBinding[F],
    D: HasInstance[F]
  ): Lazy[ProtobufCodec[DynamicValue]] = Lazy.fail(new Exception("Dynamic is not supported"))

  def deriveWrapper[F[_, _], A, B](
    wrapped: Reflect[F, B],
    typeName: TypeName[A],
    wrapperPrimitiveType: Option[PrimitiveType[A]],
    binding: Binding[BindingType.Wrapper[A, B], A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] = {
    val wrapperBinding = binding.asInstanceOf[Binding.Wrapper[A, B]]
    D.instance(wrapped.metadata).map { wrappedCodec =>
      wrappedCodec.transform(
        (b: B) =>
          wrapperBinding.wrap(b) match {
            case Right(a)    => a
            case Left(error) => throw new Exception(s"Wrapper conversion failed for type ${typeName.name} (value: $b): $error")
          },
        (a: A) => wrapperBinding.unwrap(a)
      )
    }
  }

  private def isPacked(instance: ProtobufCodec[?]): Boolean =
    instance match {
      case p: ProtobufCodec.Primitive[?]    =>
        p.primitiveType match {
          case _: PrimitiveType.Boolean | _: PrimitiveType.Float | _: PrimitiveType.Double | _: PrimitiveType.Int | _: PrimitiveType.Long =>
            true
          case _                                                                                                                          => false
        }
      case e: ProtobufCodec.Enum[?]         => true
      case t: ProtobufCodec.Transform[?, ?] => isPacked(t.codec)
      case _                                => false
    }

  private def getTypeName(typeName: TypeName[?], modifiers: Seq[Modifier]): String =
    modifiers
      .collectFirst { case Modifier.config(`renameModifier`, newName) => newName }
      .getOrElse(s"${typeName.name}${typeName.params.map(getTypeName(_, Nil)).mkString}")

  private def getFieldName(fieldName: String, modifiers: Seq[Modifier]): String =
    modifiers
      .collectFirst { case Modifier.config(`renameModifier`, newName) => newName }
      .getOrElse(toSnakeCase(fieldName))

  private def getEnumMemberName(memberName: String, modifiers: Seq[Modifier], enumPrefix: String, enumSuffix: String): String =
    modifiers
      .collectFirst { case Modifier.config(`renameModifier`, newName) => newName }
      .getOrElse {
        val prefix = if (enumPrefix.nonEmpty) s"${enumPrefix.toUpperCase}_" else ""
        val suffix = if (enumSuffix.nonEmpty) s"_${enumSuffix.toUpperCase}" else ""
        s"$prefix${toUpperSnakeCase(memberName)}$suffix"
      }

  private def getReservedIndexes(modifiers: Seq[Modifier]): List[Int] =
    modifiers.collect { case Modifier.config(`reservedModifier`, value) => value.split(",").map(_.trim.toInt).toList }.flatten.toSet.toList.sorted

  private def getReservedIndex(modifiers: Seq[Modifier]): Option[Int] =
    modifiers.collectFirst { case Modifier.config(`reservedModifier`, value) => value.toIntOption }.flatten

  private def getEnumPrefix(modifiers: Seq[Modifier], flags: Set[DerivationFlag], typeName: TypeName[?]): String =
    modifiers
      .collectFirst { case Modifier.config(`enumPrefixModifier`, prefix) => prefix }
      .getOrElse(if (flags.contains(DerivationFlag.AutoPrefixEnums)) typeNameToUpperSnakeCase(getTypeName(typeName, modifiers)) else "")

  private def getEnumSuffix(modifiers: Seq[Modifier], flags: Set[DerivationFlag], typeName: TypeName[?]): String =
    modifiers
      .collectFirst { case Modifier.config(`enumSuffixModifier`, suffix) => suffix }
      .getOrElse(if (flags.contains(DerivationFlag.AutoSuffixEnums)) typeNameToUpperSnakeCase(getTypeName(typeName, modifiers)) else "")

  private def getComment(modifiers: Seq[Modifier]): Option[String] =
    modifiers.collectFirst { case Modifier.config(`commentModifier`, value) => value }

  private def isEnum(cases: IndexedSeq[Term[?, ?, ?]], modifiers: Seq[Modifier]): Boolean =
    cases.forall(c =>
      innerSchema(c.value) match {
        case record: Reflect.Record[?, ?] => record.fields.length == 0
        case _                            => false
      }
    ) && !modifiers.exists { case Modifier.config(`oneOfModifier`, _) => true; case _ => false }

  private def innerSchema[F[_, _], A](schema: Reflect[F, A]): Reflect[F, A] =
    schema match {
      case Reflect.Deferred(value) => innerSchema(value())
      case _                       => schema
    }

  private def getDefaultValue[F[_, _], A](reflect: Reflect[F, A])(using F: HasBinding[F]): Option[A] =
    reflect.binding.defaultValue.map(_())

  private def getDefaultValue[A](using codec: ProtobufCodec[A]): A =
    codec match {
      case ProtobufCodec.Primitive(primitiveType)                          =>
        primitiveType match {
          case _: PrimitiveType.Boolean => false
          case _: PrimitiveType.Float   => 0.0f
          case _: PrimitiveType.Double  => 0.0
          case _: PrimitiveType.Int     => 0
          case _: PrimitiveType.Long    => 0L
          case _: PrimitiveType.String  => ""
          case _                        => throw new Exception(s"Unsupported primitive type: $primitiveType")
        }
      case ProtobufCodec.Message(_, fields, constructor, _, _, _, _, _, _) =>
        val registers = Registers(constructor.usedRegisters)
        fields.foreach {
          case field: SimpleField[?]   =>
            setToRegister(registers, RegisterOffset.Zero, field.register, field.defaultValue)
          case field: OneOfField[?]    =>
            setToRegister(registers, RegisterOffset.Zero, field.register, field.defaultValue)
          case field: ExcludedField[?] =>
            setToRegister(registers, RegisterOffset.Zero, field.register, field.defaultValue)
        }
        constructor.construct(registers, RegisterOffset.Zero)
      case ProtobufCodec.Optional(_)                                       =>
        None.asInstanceOf[A]
      case c: ProtobufCodec.Enum[A]                                        =>
        c.valuesByIndex(0)
      case ProtobufCodec.RepeatedMap(_, constructor, _, _)                 =>
        constructor.emptyObject.asInstanceOf[A]
      case ProtobufCodec.Repeated(_, constructor, _, _)                    =>
        constructor.emptyObject.asInstanceOf[A]
      case ProtobufCodec.Transform(from, _, codec)                         =>
        try from(getDefaultValue(using codec))
        catch { case _: Exception => null.asInstanceOf[A] }
      case c: ProtobufCodec.RecursiveMessage[A]                            =>
        getDefaultValue(using c.codec)
      case ProtobufCodec.Bytes                                             =>
        Array.empty[Byte]
    }

  private def constructEnumCase[F[_, _], A](c: Term[F, ?, A])(using hasBinding: HasBinding[F]): A =
    innerSchema(c.value) match {
      case r: Reflect.Record[F, _] if r.fields.isEmpty =>
        hasBinding.record(r.metadata).constructor.construct(Registers(RegisterOffset.Zero), RegisterOffset.Zero)
      case _                                           => throw new Exception(s"Unsupported enum case: $c")
    }

  private def isValidKeyType[A](codec: ProtobufCodec[A]): Boolean =
    codec match {
      case ProtobufCodec.Primitive(primitiveType) =>
        primitiveType match {
          case _: PrimitiveType.Int | _: PrimitiveType.Long | _: PrimitiveType.String | _: PrimitiveType.Boolean => true
          case _                                                                                                 => false
        }
      case ProtobufCodec.Transform(_, _, codec)   => isValidKeyType(codec)
      case _                                      => false
    }

  private def isValidValueTypeForMap[A](codec: ProtobufCodec[A]): Boolean =
    codec match {
      case _: ProtobufCodec.Optional[_]          => false // Optional values require repeated entry format
      case _: ProtobufCodec.Repeated[_, _]       => false // Repeated values require repeated entry format
      case _: ProtobufCodec.RepeatedMap[_, _, _] => false // Map values require repeated entry format
      case ProtobufCodec.Transform(_, _, codec)  => isValidValueTypeForMap(codec)
      case _                                     => true
    }

  private val unitOption = TypeName.option(TypeName.unit)
  private val unitSome   = TypeName.some(TypeName.unit)
}

object ProtobufDeriver extends ProtobufDeriver(Set.empty, Vector.empty, Vector.empty) {

  /**
    * Flags for the derivation process.
    */
  enum DerivationFlag {

    /**
      * Instead of using the optional keyword, the optional fields will be encoded as a oneOf field with two cases: one of type `Empty` and one for the actual value.
      */
    case OptionalAsOneOf

    /**
      * Automatically prefix the enum members with the type name.
      */
    case AutoPrefixEnums

    /**
      * Automatically suffix the enum members with the type name.
      */
    case AutoSuffixEnums

    /**
      * All types used in oneof fields will be encoded as nested types inside the parent message.
      */
    case NestedOneOf
  }

  final private case class TermInstance[F[_, _], A](term: Term[F, ?, A], instance: ProtobufCodec[A])
}
