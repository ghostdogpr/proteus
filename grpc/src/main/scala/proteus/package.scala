package proteus

import com.google.protobuf.DescriptorProtos.*

extension (protoType: ProtoIR.Type) {
  def toDescriptorType: FieldDescriptorProto.Type =
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
  def addToDescriptor(builder: DescriptorProto.Builder, oneofIndex: Option[Int]): Unit = {
    val fieldBuilder = FieldDescriptorProto.newBuilder().setName(field.name).setNumber(field.number)
    oneofIndex.foreach(fieldBuilder.setOneofIndex)

    field.ty match {
      case listType: ProtoIR.Type.ListType    =>
        fieldBuilder.setLabel(FieldDescriptorProto.Label.LABEL_REPEATED)
        fieldBuilder.setType(listType.valueType.toDescriptorType)
        listType.valueType match {
          case enumType: ProtoIR.Type.EnumRefType => fieldBuilder.setTypeName(enumType.fqn.render)
          case messageType: ProtoIR.Type.RefType  => fieldBuilder.setTypeName(messageType.fqn.render)
          case _                                  =>
        }
      case mapType: ProtoIR.Type.MapType      =>
        fieldBuilder.setLabel(FieldDescriptorProto.Label.LABEL_REPEATED)
        fieldBuilder.setType(FieldDescriptorProto.Type.TYPE_MESSAGE)
        fieldBuilder.setTypeName(s"${field.name.capitalize}Entry")
        val mapEntryBuilder =
          DescriptorProto.newBuilder().setName(s"${field.name.capitalize}Entry").setOptions(MessageOptions.newBuilder().setMapEntry(true))
        mapEntryBuilder.addField(
          FieldDescriptorProto.newBuilder().setName("key").setNumber(1).setType(mapType.keyType.toDescriptorType).build()
        )
        mapEntryBuilder.addField(
          FieldDescriptorProto.newBuilder().setName("value").setNumber(2).setType(mapType.valueType.toDescriptorType).build()
        )
        builder.addNestedType(mapEntryBuilder.build())
      case enumType: ProtoIR.Type.EnumRefType =>
        fieldBuilder.setType(FieldDescriptorProto.Type.TYPE_ENUM)
        fieldBuilder.setTypeName(enumType.fqn.render)
      case messageType: ProtoIR.Type.RefType  =>
        fieldBuilder.setType(FieldDescriptorProto.Type.TYPE_MESSAGE)
        fieldBuilder.setTypeName(messageType.fqn.render)
      case _                                  =>
        fieldBuilder.setType(field.ty.toDescriptorType)
    }

    builder.addField(fieldBuilder.build()): Unit
  }
}

extension (msg: ProtoIR.Message) {
  def toDescriptor: DescriptorProto = {
    val builder = DescriptorProto.newBuilder().setName(msg.name)

    msg.elements.foreach {
      case ProtoIR.MessageElement.FieldElement(field)                 =>
        field.addToDescriptor(builder, None)
      case ProtoIR.MessageElement.OneofElement(oneof)                 =>
        builder.addOneofDecl(OneofDescriptorProto.newBuilder().setName(oneof.name).build())
        val oneofIndex = builder.getOneofDeclCount - 1
        oneof.fields.foreach(_.addToDescriptor(builder, Some(oneofIndex)))
      case ProtoIR.MessageElement.NestedMessageElement(nestedMessage) =>
        builder.addNestedType(nestedMessage.toDescriptor)
      case ProtoIR.MessageElement.EnumDefElement(nestedEnum)          =>
        builder.addEnumType(nestedEnum.toDescriptor)
    }

    builder.build()
  }
}

extension (enumDef: ProtoIR.Enum) {
  def toDescriptor: EnumDescriptorProto = {
    val builder = EnumDescriptorProto.newBuilder().setName(enumDef.name)
    enumDef.values.foreach { enumValue =>
      builder.addValue(EnumValueDescriptorProto.newBuilder().setName(enumValue.name).setNumber(enumValue.intValue).build())
    }
    builder.build()
  }
}

extension (service: ProtoIR.Service) {
  def toDescriptor: ServiceDescriptorProto = {
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
