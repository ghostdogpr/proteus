package proteus

import scala.jdk.CollectionConverters.*

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor
import io.grpc.*
import io.grpc.protobuf.ProtoFileDescriptorSupplier

case class Service[Rpcs] private (name: String, rpcs: List[Rpc[?, ?]], dependencies: List[Dependency] = Nil) {
  val toProtoIR: List[ProtoIR.TopLevelDef] =
    (ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR))) ::
      rpcs.flatMap(_.messagesToProtoIR)).distinct

  val fileDescriptor: FileDescriptor = {
    val fileBuilder = FileDescriptorProto.newBuilder().setName(s"${name.toLowerCase}.proto").setPackage("")

    val dependencyFileDescriptors = dependencies.flatMap(_.fileDescriptor)

    dependencyFileDescriptors.foreach(fileDescriptor => fileBuilder.addDependency(fileDescriptor.getName))

    val dependencyTypes =
      dependencyFileDescriptors.flatMap(_.getMessageTypes.asScala).map(_.getName).toSet ++
        dependencyFileDescriptors.flatMap(_.getEnumTypes.asScala).map(_.getName).toSet

    toProtoIR.foreach {
      case ProtoIR.TopLevelDef.MessageDef(msg)     => if (!dependencyTypes.contains(msg.name)) fileBuilder.addMessageType(msg.toDescriptor): Unit
      case ProtoIR.TopLevelDef.EnumDef(enumDef)    => if (!dependencyTypes.contains(enumDef.name)) fileBuilder.addEnumType(enumDef.toDescriptor): Unit
      case ProtoIR.TopLevelDef.ServiceDef(service) => fileBuilder.addService(service.toDescriptor)
    }
    FileDescriptor.buildFrom(fileBuilder.build(), dependencyFileDescriptors.toArray)
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

  def dependsOn(dependencies: List[Dependency]): Service[Rpcs] =
    copy(dependencies = dependencies)

  def render(packageName: Option[String], options: List[ProtoIR.TopLevelOption]): String =
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = dependencies.map(dependency => ProtoIR.Statement.ImportStatement(dependency.dependencyName)) ++
          toProtoIR.map(ProtoIR.Statement.TopLevelStatement(_))
      )
    )
}

object Service {
  def apply(name: String): Service[Any] =
    Service(name, List.empty)
}
