package proteus

import scala.jdk.CollectionConverters.*

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor
import io.grpc.*
import io.grpc.protobuf.ProtoFileDescriptorSupplier

case class Service[Rpcs] private (name: String, rpcs: List[Rpc[?, ?]], dependencies: Array[FileDescriptor] = Array.empty) {
  val toProtoIR: List[ProtoIR.TopLevelDef] =
    (ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR))) ::
      rpcs.flatMap(_.messagesToProtoIR)).distinct

  val fileDescriptor: FileDescriptor = {
    val fileBuilder = FileDescriptorProto.newBuilder().setName(s"${name.toLowerCase}.proto").setPackage("")

    dependencies.foreach(dep => fileBuilder.addDependency(dep.getName))

    val dependencyTypes =
      dependencies.flatMap(_.getMessageTypes.asScala).map(_.getName).toSet ++
        dependencies.flatMap(_.getEnumTypes.asScala).map(_.getName).toSet

    toProtoIR.foreach {
      case ProtoIR.TopLevelDef.MessageDef(msg)     => if (!dependencyTypes.contains(msg.name)) fileBuilder.addMessageType(msg.toDescriptor): Unit
      case ProtoIR.TopLevelDef.EnumDef(enumDef)    => if (!dependencyTypes.contains(enumDef.name)) fileBuilder.addEnumType(enumDef.toDescriptor): Unit
      case ProtoIR.TopLevelDef.ServiceDef(service) => fileBuilder.addService(service.toDescriptor)
    }
    FileDescriptor.buildFrom(fileBuilder.build(), dependencies)
  }

  val methodDescriptors: List[MethodDescriptor[?, ?]] =
    rpcs.sortBy(_.name).map(_.toMethodDescriptor(name, fileDescriptor))

  val serviceDescriptor: ServiceDescriptor =
    methodDescriptors
      .foldLeft(
        ServiceDescriptor
          .newBuilder(name)
          .setSchemaDescriptor(new ProtoFileDescriptorSupplier {
            def getFileDescriptor: FileDescriptor = fileDescriptor
          })
      )((builder, methodDescriptor) => builder.addMethod(methodDescriptor))
      .build()

  def rpc[Request, Response](rpc: Rpc[Request, Response]): Service[Rpcs & rpc.type] =
    Service(name, rpcs :+ rpc)

  def dependsOn(fileDescriptors: List[FileDescriptor]): Service[Rpcs] =
    copy(dependencies = fileDescriptors.toArray)
}

object Service {
  def apply(name: String): Service[Any] =
    Service(name, List.empty)
}
