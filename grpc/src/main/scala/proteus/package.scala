package proteus

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor

import proteus.internal.*

extension (protoType: ProtoIR.Type) {
  private[proteus] def toDescriptorType: FieldDescriptorProto.Type =
    protoType match {
      case ProtoIR.Type.String         => FieldDescriptorProto.Type.TYPE_STRING
      case ProtoIR.Type.Int32          => FieldDescriptorProto.Type.TYPE_INT32
      case ProtoIR.Type.Int64          => FieldDescriptorProto.Type.TYPE_INT64
      case ProtoIR.Type.Bool           => FieldDescriptorProto.Type.TYPE_BOOL
      case ProtoIR.Type.Bytes          => FieldDescriptorProto.Type.TYPE_BYTES
      case ProtoIR.Type.Double         => FieldDescriptorProto.Type.TYPE_DOUBLE
      case ProtoIR.Type.Float          => FieldDescriptorProto.Type.TYPE_FLOAT
      case ProtoIR.Type.Sint32         => FieldDescriptorProto.Type.TYPE_SINT32
      case ProtoIR.Type.Sint64         => FieldDescriptorProto.Type.TYPE_SINT64
      case ProtoIR.Type.Uint32         => FieldDescriptorProto.Type.TYPE_UINT32
      case ProtoIR.Type.Uint64         => FieldDescriptorProto.Type.TYPE_UINT64
      case ProtoIR.Type.Fixed32        => FieldDescriptorProto.Type.TYPE_FIXED32
      case ProtoIR.Type.Fixed64        => FieldDescriptorProto.Type.TYPE_FIXED64
      case ProtoIR.Type.Sfixed32       => FieldDescriptorProto.Type.TYPE_SFIXED32
      case ProtoIR.Type.Sfixed64       => FieldDescriptorProto.Type.TYPE_SFIXED64
      case _: ProtoIR.Type.EnumRefType => FieldDescriptorProto.Type.TYPE_ENUM
      case _: ProtoIR.Type.RefType     => FieldDescriptorProto.Type.TYPE_MESSAGE
      case _                           => throw new UnsupportedOperationException(s"Unsupported type: $protoType")
    }
}

extension (field: ProtoIR.Field) {
  private[proteus] def addToDescriptor(
    builder: DescriptorProto.Builder,
    oneOfIndex: Option[Int],
    parentFqn: String,
    topLevelFqns: Map[String, String]
  ): Unit = {
    val fieldBuilder = FieldDescriptorProto.newBuilder().setJsonName(toCamelCase(field.name)).setName(field.name).setNumber(field.number)
    oneOfIndex.foreach(fieldBuilder.setOneofIndex)

    def makeFqn(name: String, typeId: Option[String]): String =
      s".${typeId.flatMap(topLevelFqns.get).getOrElse(s"$parentFqn.$name")}"

    field.ty match {
      case listType: ProtoIR.Type.ListType    =>
        fieldBuilder.setLabel(FieldDescriptorProto.Label.LABEL_REPEATED)
        fieldBuilder.setType(listType.valueType.toDescriptorType)
        listType.valueType match {
          case enumType: ProtoIR.Type.EnumRefType => fieldBuilder.setTypeName(makeFqn(enumType.name, enumType.typeId))
          case messageType: ProtoIR.Type.RefType  => fieldBuilder.setTypeName(makeFqn(messageType.name, messageType.typeId))
          case _                                  =>
        }
      case mapType: ProtoIR.Type.MapType      =>
        fieldBuilder.setLabel(FieldDescriptorProto.Label.LABEL_REPEATED)
        fieldBuilder.setType(FieldDescriptorProto.Type.TYPE_MESSAGE)
        fieldBuilder.setTypeName(s".$parentFqn.${toUpperCamelCase(field.name)}Entry")
        val mapEntryBuilder      =
          DescriptorProto.newBuilder().setName(s"${toUpperCamelCase(field.name)}Entry").setOptions(MessageOptions.newBuilder().setMapEntry(true))
        val mapKeyFieldBuilder   = FieldDescriptorProto
          .newBuilder()
          .setName("key")
          .setNumber(1)
          .setType(mapType.keyType.toDescriptorType)
        mapType.keyType match {
          case enumType: ProtoIR.Type.EnumRefType => mapKeyFieldBuilder.setTypeName(makeFqn(enumType.name, enumType.typeId))
          case messageType: ProtoIR.Type.RefType  => mapKeyFieldBuilder.setTypeName(makeFqn(messageType.name, messageType.typeId))
          case _                                  =>
        }
        mapEntryBuilder.addField(mapKeyFieldBuilder.build())
        val mapValueFieldBuilder = FieldDescriptorProto
          .newBuilder()
          .setName("value")
          .setNumber(2)
          .setType(mapType.valueType.toDescriptorType)
        mapType.valueType match {
          case enumType: ProtoIR.Type.EnumRefType => mapValueFieldBuilder.setTypeName(makeFqn(enumType.name, enumType.typeId))
          case messageType: ProtoIR.Type.RefType  => mapValueFieldBuilder.setTypeName(makeFqn(messageType.name, messageType.typeId))
          case _                                  =>
        }
        mapEntryBuilder.addField(mapValueFieldBuilder.build())
        builder.addNestedType(mapEntryBuilder.build())
      case enumType: ProtoIR.Type.EnumRefType =>
        fieldBuilder.setType(FieldDescriptorProto.Type.TYPE_ENUM)
        fieldBuilder.setTypeName(makeFqn(enumType.name, enumType.typeId))
      case messageType: ProtoIR.Type.RefType  =>
        fieldBuilder.setType(FieldDescriptorProto.Type.TYPE_MESSAGE)
        fieldBuilder.setTypeName(makeFqn(messageType.name, messageType.typeId))
      case _                                  =>
        fieldBuilder.setType(field.ty.toDescriptorType)
    }

    builder.addField(fieldBuilder.build()): Unit
  }
}

extension (msg: ProtoIR.Message) {
  // `topLevelFqns` already contains every typeId in the file (computed once at the outer
  // Service/Dependency level), so we don't need to augment it as we descend into nested types.
  private[proteus] def toDescriptor(fqn: String, topLevelFqns: Map[String, String]): DescriptorProto = {
    val builder = DescriptorProto.newBuilder().setName(msg.name)
    msg.elements.foreach {
      case ProtoIR.MessageElement.FieldElement(field)                 =>
        if (field != ProtoIR.excludedField) field.addToDescriptor(builder, None, fqn, topLevelFqns)
      case ProtoIR.MessageElement.OneOfElement(oneOf)                 =>
        builder.addOneofDecl(OneofDescriptorProto.newBuilder().setName(oneOf.name).build())
        val oneOfIndex = builder.getOneofDeclCount - 1
        oneOf.fields.foreach(_.addToDescriptor(builder, Some(oneOfIndex), fqn, topLevelFqns))
      case ProtoIR.MessageElement.NestedMessageElement(nestedMessage) =>
        builder.addNestedType(nestedMessage.toDescriptor(s"$fqn.${nestedMessage.name}", topLevelFqns))
      case ProtoIR.MessageElement.NestedEnumElement(nestedEnum)       =>
        builder.addEnumType(nestedEnum.toDescriptor)
    }
    builder.build()
  }
}

extension (enumDef: ProtoIR.Enum) {
  private[proteus] def toDescriptor: EnumDescriptorProto = {
    val builder = EnumDescriptorProto.newBuilder().setName(enumDef.name)
    enumDef.values.foreach { enumValue =>
      builder.addValue(EnumValueDescriptorProto.newBuilder().setName(enumValue.name).setNumber(enumValue.intValue).build())
    }
    builder.build()
  }
}

extension (service: ProtoIR.Service) {
  private[proteus] def toDescriptor: ServiceDescriptorProto = {
    val serviceBuilder = ServiceDescriptorProto.newBuilder().setName(service.name)

    service.rpcs.foreach { rpc =>
      serviceBuilder.addMethod(
        MethodDescriptorProto
          .newBuilder()
          .setName(rpc.name)
          .setInputType(rpc.request.fqn.render)
          .setOutputType(rpc.response.fqn.render)
          .setClientStreaming(rpc.streamingRequest)
          .setServerStreaming(rpc.streamingResponse)
      )
    }
    serviceBuilder.build()
  }
}

extension (dependency: Dependency) {
  private[proteus] def fileDescriptor: Option[FileDescriptor] =
    if (dependency.types.nonEmpty) {
      ProtobufCodec.throwIfConflicts("dependency", dependency.dependencyName, dependency.findConflicts)
      val sharedFileBuilder         =
        FileDescriptorProto
          .newBuilder()
          .setName(s"${dependency.path.fold("")(_ + "/")}${dependency.dependencyName}.proto")
          .setPackage(dependency.packageName.getOrElse(""))
      val dependencyFileDescriptors = dependency.filteredDependencies.flatMap(_.fileDescriptor)
      dependencyFileDescriptors.foreach(fd => sharedFileBuilder.addDependency(fd.getName))
      val pkgPrefix                 = dependency.packageName.fold("")(_ + ".")
      val resolvedTypes             =
        ProtobufCodec.relocateNestedIn(dependency.filteredTypes.toList.distinctBy(ProtobufCodec.dedupKey))
      resolvedTypes.foreach {
        case ProtoIR.TopLevelDef.MessageDef(msg)  =>
          sharedFileBuilder.addMessageType(msg.toDescriptor(pkgPrefix + msg.name, dependency.topLevelFqns))
        case ProtoIR.TopLevelDef.EnumDef(enumDef) => sharedFileBuilder.addEnumType(enumDef.toDescriptor)
        case ProtoIR.TopLevelDef.ServiceDef(_)    =>
      }
      Some(FileDescriptor.buildFrom(sharedFileBuilder.build(), dependencyFileDescriptors.toArray))
    } else None
}
