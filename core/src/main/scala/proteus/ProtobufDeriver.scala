package proteus

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.util.Try
import scala.util.control.NoStackTrace

import zio.blocks.docs.Doc
import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.*
import zio.blocks.schema.derive.*
import zio.blocks.typeid.*

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
case class ProtobufDeriver private (
  flags: Set[DerivationFlag],
  instances: Vector[InstanceOverride],
  modifiers: Vector[ModifierOverride],
  messageCustomizer: ProtoIR.Message => ProtoIR.Message,
  enumCustomizer: ProtoIR.Enum => ProtoIR.Enum
) extends Deriver[ProtobufCodec] {

  // Shadows Deriver.instance[F, T, A] by name so extension methods are reachable.
  // InstanceShadow is sealed with no instances, making this uncallable.
  private[proteus] def instance(shadow: InstanceShadow): Nothing = throw new UnsupportedOperationException

  /**
    * Adds a custom codec instance for the given type.
    */
  def instance[A](instance: => ProtobufCodec[A])(using typeId: TypeId[A]): ProtobufDeriver =
    copy(instances = instances :+ InstanceOverrideByType(typeId, Lazy(instance)))

  /**
    * Adds a custom codec instance for a term (case class field or enum member) of the given type.
    * @param termName the name of the term to apply the instance override to (there will be a compile error if the term does not exist)
    * @param instance the instance to use for the term.
    */
  inline def instance[A: TypeId, B](termName: String, instance: => ProtobufCodec[B]): ProtobufDeriver = {
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.ProductOf[A] =>
        inline if (!constValue[proteus.Tuple.Contains[m.MirroredElemLabels, termName.type]])
          error("Field " + constValue[termName.type] + " does not exist in class " + constValue[m.MirroredLabel] + ".")
        else inline if (!constValue[proteus.Tuple.IsElemType[m.MirroredElemLabels, m.MirroredElemTypes, termName.type, B]])
          error("Instance type mismatch for field " + constValue[termName.type] + " in " + constValue[m.MirroredLabel] + ".")
      case m: Mirror.SumOf[A]     =>
        inline if (!constValue[proteus.Tuple.Contains[m.MirroredElemLabels, termName.type]])
          error("Case " + constValue[termName.type] + " does not exist in sealed trait or enum " + constValue[m.MirroredLabel] + ".")
        else inline if (!constValue[proteus.Tuple.IsElemType[m.MirroredElemLabels, m.MirroredElemTypes, termName.type, B]])
          error("Instance type mismatch for case " + constValue[termName.type] + " in " + constValue[m.MirroredLabel] + ".")
    }
    copy(instances = instances :+ InstanceOverrideByTypeAndTermName(summon[TypeId[A]], termName, Lazy(instance)))
  }

  /**
    * Adds a custom modifier for the given type.
    */
  def modifier[A](modifier: Modifier.Reflect)(using typeId: TypeId[A]): ProtobufDeriver =
    copy(modifiers = modifiers :+ ModifierReflectOverrideByType(typeId, modifier))

  /**
    * Adds a custom modifier for a term (case class field or enum member) of the given type.
    *
    * @param termName the name of the term to apply the modifier to (there will be a compile error if the term does not exist)
    * @param modifier the modifier to apply.
    */
  inline def modifier[A: TypeId](termName: String, modifier: Modifier.Term): ProtobufDeriver = {
    inline summonInline[Mirror.Of[A]] match {
      case m: Mirror.ProductOf[A] =>
        inline if (!constValue[proteus.Tuple.Contains[m.MirroredElemLabels, termName.type]]) {
          error("Field " + constValue[termName.type] + " does not exist in class " + constValue[m.MirroredLabel] + ".")
        }
      case m: Mirror.SumOf[A]     =>
        inline if (!constValue[proteus.Tuple.Contains[m.MirroredElemLabels, termName.type]]) {
          error("Case " + constValue[termName.type] + " does not exist in sealed trait or enum " + constValue[m.MirroredLabel] + ".")
        }
    }
    copy(modifiers = modifiers :+ ModifierTermOverrideByType(summon[TypeId[A]], termName, modifier))
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

  /**
    * Registers a transformation function that will be applied to every derived `ProtoIR.Message`.
    * Multiple calls compose in order via `andThen`.
    */
  def customizeMessages(f: ProtoIR.Message => ProtoIR.Message): ProtobufDeriver =
    copy(messageCustomizer = messageCustomizer.andThen(f))

  /**
    * Registers a transformation function that will be applied to every derived `ProtoIR.Enum`.
    * Multiple calls compose in order via `andThen`.
    */
  def customizeEnums(f: ProtoIR.Enum => ProtoIR.Enum): ProtobufDeriver =
    copy(enumCustomizer = enumCustomizer.andThen(f))

  override def instanceOverrides: IndexedSeq[InstanceOverride] = instances

  override def modifierOverrides: IndexedSeq[ModifierOverride] = modifiers

  private val instanceCache  = java.util.concurrent.ConcurrentHashMap[TypeId[?], ProtobufCodec.Message[Any]]()
  private val visitedCache   = new ThreadLocal[java.util.IdentityHashMap[TypeId[?], Unit]] {
    override def initialValue: java.util.IdentityHashMap[TypeId[?], Unit] = new java.util.IdentityHashMap[TypeId[?], Unit]
  }
  private val recursiveCache = new ThreadLocal[java.util.IdentityHashMap[TypeId[?], Unit]] {
    override def initialValue: java.util.IdentityHashMap[TypeId[?], Unit] = new java.util.IdentityHashMap[TypeId[?], Unit]
  }

  override def derivePrimitive[A](
    primitiveType: PrimitiveType[A],
    typeId: TypeId[A],
    binding: Binding[BindingType.Primitive, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  ): Lazy[ProtobufCodec[A]] =
    Lazy(ProtobufCodec.Primitive(primitiveType))

  override def deriveRecord[F[_, _], A](
    fields: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding[BindingType.Record, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] =
    Lazy {
      val visited   = visitedCache.get
      val recursive = recursiveCache.get
      if (visited.containsKey(typeId)) {
        recursive.put(typeId, ())
        Lazy(ProtobufCodec.RecursiveMessage(() => instanceCache.get(typeId).asInstanceOf[ProtobufCodec.Message[A]]))
      } else {
        visited.put(typeId, ())
        val recordBinding      = binding.asInstanceOf[Binding.Record[A]]
        val registers          = Reflect.Record.registers(fields.map(_.value).toArray)
        val offset             = Reflect.Record.usedRegisters(registers)
        val reservedIndexes    = getReservedIndexes(modifiers).toSet
        val allReservedIndexes = reservedIndexes ++ getReservedIndexes(fields.flatMap(_.modifiers)).toSet
        val nested             = modifiers.collectFirst { case Modifier.config(`nestedModifier`, value) => value.toBooleanOption }.flatten
        val builder            = Array.newBuilder[ProtobufCodec.MessageField[?]]
        var id                 = 0

        def addField[A](index: Int, field: Term[F, ?, A], instance: ProtobufCodec[A]): Unit = {
          val name     = getFieldName(field.name, field.modifiers)
          val register = registers(index)
          instance match {
            case t @ ProtobufCodec.Transform(from, to, ProtobufCodec.Message(_, Array(o: OneOfField[inner]), _, _, _, _, true, _, _, _)) =>
              val idIterator = getReservedIndexes(field.modifiers).iterator
              builder += OneOfField(
                name,
                o.cases.map {
                  case field: ExcludedField[?] => field
                  case field: SimpleField[?]   =>
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
            case ProtobufCodec.Message(_, Array(o: OneOfField[?]), _, _, _, _, true, _, _, _)                                            =>
              val idIterator = getReservedIndexes(field.modifiers).iterator
              builder += OneOfField(
                name,
                o.cases.map {
                  case field: ExcludedField[?] => field
                  case field: SimpleField[?]   =>
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
            case ProtobufCodec.Optional(codec, oneof)
                if oneof ||
                  flags.contains(DerivationFlag.OptionalAsOneOf) ||
                  field.modifiers.exists { case Modifier.config(`oneOfModifier`, _) => true; case _ => false } =>
              id += 1
              while (allReservedIndexes.contains(id)) id += 1
              val emptyId = id
              id += 1
              while (allReservedIndexes.contains(id)) id += 1
              val valueId = id
              val empty   = Empty()
              builder += OneOfField(
                name,
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
                getComment(field.modifiers)
              )
            case instance                                                                                                                =>
              val fieldId = getReservedIndex(field.modifiers) match {
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
                field.value.getDefaultValue.getOrElse(getDefaultValue(using instance)),
                getComment(field.modifiers),
                isDeprecated(field.modifiers)
              )
          }
        }

        fields.indices
          .foldLeft(Lazy(())) { (acc, idx) =>
            acc.flatMap { _ =>
              val field = fields(idx)
              if (isExcluded(field))
                Lazy {
                  val defaultValue =
                    Try(defaultFromReflect(field.value))
                      .orElse(Try(defaultFromRegister(registers(idx))))
                      .getOrElse(
                        throw new Exception(
                          s"Could not generate a default value for excluded field ${field.name} of type ${typeId.name}. Use `Schema#defaultValue` to assign an explicit default value to that type."
                        )
                      )
                  builder += ExcludedField(registers(idx), defaultValue)
                }
              else
                D.instance(field.value.metadata).map { instance =>
                  try addField(idx, field, instance)
                  catch {
                    case e: Exception =>
                      throw new Exception(s"Error deriving field ${field.name} of type ${typeId.name}", e)
                  }
                }
            }
          }
          .map { _ =>
            val codec = ProtobufCodec.Message(
              getTypeName(typeId, modifiers),
              builder.result(),
              recordBinding.constructor,
              recordBinding.deconstructor,
              offset,
              reservedIndexes,
              inline = false,
              nested = nested,
              comment = getComment(modifiers),
              customizeIR = messageCustomizer
            )

            visited.remove(typeId)
            if (recursive.containsKey(typeId)) instanceCache.put(typeId, codec.asInstanceOf[ProtobufCodec.Message[Any]]): Unit
            codec
          }
      }
    }.flatten

  override def deriveVariant[F[_, _], A](
    cases: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding[BindingType.Variant, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] = {
    if (typeId.isOption)
      D.instance(cases.find(c => c.name == "Some").get.value.asRecord.get.fields.head.value.metadata)
        .map { instance =>
          if (ProtobufCodec.isRepeated(using instance))
            throw new Exception(s"Unsupported usage of repeated inside optional type $typeId")
          val oneof = modifiers.collectFirst { case Modifier.config(`oneOfModifier`, _) => true }.getOrElse(false)
          ProtobufCodec.Optional(instance, oneof).asInstanceOf[ProtobufCodec[A]]
        }
    else if (isEnum(cases, modifiers)) {
      val filteredCases      = cases.filterNot(c =>
        c.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false } ||
          c.value.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false }
      )
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
        val prefix     = getEnumPrefix(modifiers, flags, typeId)
        val suffix     = getEnumSuffix(modifiers, flags, typeId)
        val enumName   = getEnumMemberName(c.name, c.modifiers, prefix, suffix)
        builder += ProtobufCodec.EnumValue(enumName, fieldIndex, a, getComment(c.modifiers), isDeprecated(c.modifiers))
      }
      Lazy(
        ProtobufCodec.Enum(
          getTypeName(typeId, modifiers),
          builder.result(),
          reservedIndexes.toList,
          nested = nested,
          getComment(modifiers),
          customizeIR = enumCustomizer
        )
      )
    } else
      Lazy {
        val visited   = visitedCache.get
        val recursive = recursiveCache.get
        if (visited.containsKey(typeId)) {
          recursive.put(typeId, ())
          Lazy(ProtobufCodec.RecursiveMessage(() => instanceCache.get(typeId).asInstanceOf[ProtobufCodec.Message[A]]))
        } else {
          visited.put(typeId, ())

          val nested             = modifiers.collectFirst { case Modifier.config(`nestedModifier`, value) => value.toBooleanOption }.flatten
          val inlineOneOf        = modifiers.collectFirst { case Modifier.config(`oneOfModifier`, value) => value.contains("inline") }.getOrElse(false)
          val nestedOneOf        = modifiers
            .collectFirst { case Modifier.config(`oneOfModifier`, value) => value.contains("nested") }
            .getOrElse(flags.contains(DerivationFlag.NestedOneOf))
          val variant            = binding.asInstanceOf[Binding.Variant[A]]
          val discriminator      = variant.discriminator
          val reservedIndexes    = getReservedIndexes(modifiers).toSet
          val allReservedIndexes = reservedIndexes ++ getReservedIndexes(cases.flatMap(_.modifiers)).toSet
          var id                 = 1
          val register           = Register.Object(0)
          val builder            = Array.newBuilder[SimpleField[?] | ExcludedField[?]]

          cases.indices
            .foldLeft(Lazy(())) { (acc, idx) =>
              acc.flatMap { _ =>
                val c = cases(idx)
                if (isExcluded(c)) {
                  Lazy(builder += ExcludedField(register.asInstanceOf[Register[Any]], null.asInstanceOf[A]))
                } else
                  D.instance(c.value.metadata).map { instance =>
                    val fieldId = getReservedIndex(c.modifiers) match {
                      case Some(reservedIndex) => reservedIndex
                      case None                =>
                        while (allReservedIndexes.contains(id)) id += 1
                        val current = id
                        id += 1
                        current
                    }
                    builder += SimpleField(
                      getFieldName(c.name, c.modifiers),
                      fieldId,
                      if (nestedOneOf) instance.makeNested else instance,
                      register.asInstanceOf[Register[Any]],
                      null.asInstanceOf[instance.Focus],
                      getComment(c.modifiers),
                      isDeprecated(c.modifiers)
                    )
                  }
              }
            }
            .map { _ =>
              val field = OneOfField(
                "value",
                builder.result(),
                register.asInstanceOf[Register[Any]],
                discriminator,
                defaultValue.getOrElse(null.asInstanceOf[A]),
                getComment(modifiers)
              )
              val codec = ProtobufCodec.Message(
                getTypeName(typeId, modifiers),
                Array(field),
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
                comment = getComment(modifiers),
                customizeIR = messageCustomizer
              )

              visited.remove(typeId)
              if (recursive.containsKey(typeId)) instanceCache.put(typeId, codec.asInstanceOf[ProtobufCodec.Message[Any]]): Unit
              codec
            }
        }
      }.flatten
  }

  override def deriveSequence[F[_, _], C[_], A](
    element: Reflect[F, A],
    typeId: TypeId[C[A]],
    binding: Binding[BindingType.Seq[C], C[A]],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[C[A]],
    examples: Seq[C[A]]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[C[A]]] = {
    val seqBinding = binding.asInstanceOf[Binding.Seq[C, A]]
    D.instance(element.metadata).map { instance =>
      val isByteArray = seqBinding.constructor.empty(element.typeId.classTag) match {
        case _: Array[Byte] => true
        case _              => false
      }
      if (isByteArray) ProtobufCodec.Bytes.asInstanceOf[ProtobufCodec[C[A]]]
      else {
        if (ProtobufCodec.isOptional(using instance))
          throw new Exception(s"Unsupported usage of optional inside repeated type $typeId")
        if (ProtobufCodec.isRepeated(using instance))
          throw new Exception(s"Unsupported usage of repeated inside repeated type $typeId")
        ProtobufCodec.Repeated[C, A](
          instance,
          seqBinding.constructor,
          seqBinding.deconstructor,
          isPacked(instance),
          element.typeId.classTag.asInstanceOf[ClassTag[A]]
        )
      }
    }
  }

  override def deriveMap[F[_, _], M[_, _], K, V](
    key: Reflect[F, K],
    value: Reflect[F, V],
    typeId: TypeId[M[K, V]],
    binding: Binding[BindingType.Map[M], M[K, V]],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[M[K, V]],
    examples: Seq[M[K, V]]
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
            if (mapInProto) "" else s"${getTypeName(key.typeId, Nil)}${getTypeName(value.typeId, Nil)}Entry",
            Array(
              SimpleField("key", 1, keyInstance, keyRegister, key.getDefaultValue.getOrElse(getDefaultValue(using keyInstance)), None),
              SimpleField("value", 2, valueInstance, valueRegister, value.getDefaultValue.getOrElse(getDefaultValue(using valueInstance)), None)
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

  override def deriveDynamic[F[_, _]](
    binding: Binding[BindingType.Dynamic, DynamicValue],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[DynamicValue],
    examples: Seq[DynamicValue]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[DynamicValue]] = Lazy.fail(new Exception("Dynamic is not supported"))

  override def deriveWrapper[F[_, _], A, B](
    wrapped: Reflect[F, B],
    typeId: TypeId[A],
    binding: Binding[BindingType.Wrapper[A, B], A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(implicit F: HasBinding[F], D: HasInstance[F]): Lazy[ProtobufCodec[A]] = {
    val wrapperBinding = binding.asInstanceOf[Binding.Wrapper[A, B]]
    D.instance(wrapped.metadata).map { wrappedCodec =>
      wrappedCodec.transform(wrapperBinding.wrap, wrapperBinding.unwrap)
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

  private def getTypeName(typeId: TypeId[?], modifiers: Seq[Modifier]): String =
    modifiers
      .collectFirst { case Modifier.config(`renameModifier`, newName) => newName }
      .getOrElse(s"${typeId.name}${typeId.typeArgs.collect { case TypeRepr.Ref(id) => getTypeName(id, Nil) }.mkString}")

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

  private def getEnumPrefix(modifiers: Seq[Modifier], flags: Set[DerivationFlag], typeId: TypeId[?]): String =
    modifiers
      .collectFirst { case Modifier.config(`enumPrefixModifier`, prefix) => prefix }
      .getOrElse(if (flags.contains(DerivationFlag.AutoPrefixEnums)) typeNameToUpperSnakeCase(getTypeName(typeId, modifiers)) else "")

  private def getEnumSuffix(modifiers: Seq[Modifier], flags: Set[DerivationFlag], typeId: TypeId[?]): String =
    modifiers
      .collectFirst { case Modifier.config(`enumSuffixModifier`, suffix) => suffix }
      .getOrElse(if (flags.contains(DerivationFlag.AutoSuffixEnums)) typeNameToUpperSnakeCase(getTypeName(typeId, modifiers)) else "")

  private def getComment(modifiers: Seq[Modifier]): Option[String] =
    modifiers.collectFirst { case Modifier.config(`commentModifier`, value) => value }

  private def isDeprecated(modifiers: Seq[Modifier]): Boolean =
    modifiers.exists { case Modifier.config(`deprecatedModifier`, _) => true; case _ => false }

  private def isEnum(cases: IndexedSeq[Term[?, ?, ?]], modifiers: Seq[Modifier]): Boolean =
    cases.forall(c =>
      innerSchema(c.value) match {
        case record: Reflect.Record[?, ?] => record.fields.length == 0
        case _                            => false
      }
    ) && !modifiers.exists { case Modifier.config(`oneOfModifier`, _) => true; case _ => false }

  def isExcluded[F[_, _], A](c: Term[F, A, ?]): Boolean =
    c.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false } ||
      c.value.modifiers.exists { case Modifier.config(`excludedModifier`, _) => true; case _ => false }

  private def innerSchema[F[_, _], A](schema: Reflect[F, A]): Reflect[F, A] =
    schema match {
      case d: Reflect.Deferred[F, A] => innerSchema(d.value)
      case _                         => schema
    }

  private def getDefaultValue[A](using codec: ProtobufCodec[A]): A =
    codec match {
      case ProtobufCodec.Primitive(primitiveType)                             =>
        primitiveType match {
          case _: PrimitiveType.Boolean => false
          case _: PrimitiveType.Float   => 0.0f
          case _: PrimitiveType.Double  => 0.0
          case _: PrimitiveType.Int     => 0
          case _: PrimitiveType.Long    => 0L
          case _: PrimitiveType.String  => ""
          case _                        => throw new Exception(s"Unsupported primitive type: $primitiveType")
        }
      case ProtobufCodec.Message(_, fields, constructor, _, _, _, _, _, _, _) =>
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
      case ProtobufCodec.Optional(_, _)                                       =>
        None.asInstanceOf[A]
      case c: ProtobufCodec.Enum[A]                                           =>
        c.valueOrThrow(0)
      case ProtobufCodec.RepeatedMap(_, constructor, _, _)                    =>
        constructor.emptyObject.asInstanceOf[A]
      case ProtobufCodec.Repeated(_, constructor, _, _, elementClassTag)      =>
        constructor.empty(elementClassTag)
      case ProtobufCodec.Transform(from, _, codec)                            =>
        try from(getDefaultValue(using codec))
        catch { case _: Exception => null.asInstanceOf[A] }
      case c: ProtobufCodec.RecursiveMessage[A]                               =>
        getDefaultValue(using c.codec)
      case ProtobufCodec.Bytes                                                =>
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

  private object NoDefaultValueException extends Exception with NoStackTrace

  private def defaultFromRegister[A](register: Register[?]): A =
    (register match {
      case _: Register.Object[_] => throw NoDefaultValueException
      case _: Register.Int       => 0
      case _: Register.Long      => 0L
      case _: Register.Boolean   => false
      case _: Register.Double    => 0.0
      case _: Register.Float     => 0.0f
      case _: Register.Byte      => 0.toByte
      case _: Register.Short     => 0.toShort
      case _: Register.Char      => 0.toChar
      case Register.Unit         => ()
    }).asInstanceOf[A]

  private def defaultFromReflect[F[_, _], A](schema: Reflect[F, A])(implicit F: HasBinding[F]): A =
    schema.getDefaultValue.getOrElse {
      schema match {
        case primitive: Reflect.Primitive[F, A]                                         =>
          primitive.primitiveType match {
            case _: PrimitiveType.Boolean => false
            case _: PrimitiveType.Float   => 0.0f
            case _: PrimitiveType.Double  => 0.0
            case _: PrimitiveType.Int     => 0
            case _: PrimitiveType.Long    => 0L
            case _: PrimitiveType.String  => ""
            case _: PrimitiveType.Byte    => 0.toByte
            case _: PrimitiveType.Short   => 0.toShort
            case _: PrimitiveType.Char    => 0.toChar
            case _                        => throw NoDefaultValueException
          }
        case seq: Reflect.Sequence[_, _, _]                                             =>
          val seqF       = seq.asInstanceOf[Reflect.Sequence[F, ?, ?]]
          val seqBinding = seqF.binding.asInstanceOf[Binding.Seq[?, Any]]
          val classTag   = seqF.element.typeId.classTag
          seqBinding.constructor.empty(classTag).asInstanceOf[A]
        case map: Reflect.Map[_, _, _, _]                                               =>
          val mapF       = map.asInstanceOf[Reflect.Map[F, ?, ?, ?]]
          val mapBinding = mapF.binding.asInstanceOf[Binding.Map[?, Any, Any]]
          mapBinding.constructor.emptyObject.asInstanceOf[A]
        case record: Reflect.Record[F, A]                                               =>
          val recordBinding = record.binding.asInstanceOf[Binding.Record[A]]
          val registers     = Reflect.Record.registers(record.fields.map(_.value).toArray)
          val offset        = Reflect.Record.usedRegisters(registers)
          val regValues     = Registers(offset)
          var idx           = 0
          while (idx < record.fields.length) {
            val term         = record.fields(idx)
            val defaultValue = term.value.getDefaultValue.getOrElse(defaultFromReflect(term.value))
            setToRegister(regValues, RegisterOffset.Zero, registers(idx), defaultValue)
            idx += 1
          }
          recordBinding.constructor.construct(regValues, RegisterOffset.Zero)
        case wrapper: Reflect.Wrapper[F, A, ?]                                          =>
          val innerValue  = defaultFromReflect(wrapper.wrapped)
          val wrapBinding = wrapper.binding.asInstanceOf[Binding.Wrapper[A, Any]]
          wrapBinding.wrap(innerValue)
        case variant: Reflect.Variant[F, ?] if isEnum(variant.cases, variant.modifiers) =>
          variant.cases.headOption.fold(throw NoDefaultValueException)(constructEnumCase)
        case variant: Reflect.Variant[F, ?] if variant.typeId.isOption                  => None.asInstanceOf[A]
        case variant: Reflect.Variant[F, ?]                                             => throw NoDefaultValueException
        case d: Reflect.Deferred[F, A]                                                  => defaultFromReflect(d.value)
        case _: Reflect.Dynamic[_]                                                      => throw NoDefaultValueException
      }
    }
}

sealed private[proteus] trait InstanceShadow

object ProtobufDeriver extends ProtobufDeriver(Set.empty, Vector.empty, Vector.empty, identity, identity) {

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
}
