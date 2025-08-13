package proteus

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.Descriptors.FileDescriptor
import zio.blocks.schema.Schema

case class Dependency(dependencyName: String, types: Set[ProtoIR.TopLevelDef] = Set.empty, imports: List[ProtoIR.Statement.ImportStatement] = Nil) {
  def fileDescriptor: Option[FileDescriptor] =
    if (types.nonEmpty) {
      val sharedFileBuilder = FileDescriptorProto.newBuilder().setName(s"$dependencyName").setPackage("")
      types.foreach {
        case ProtoIR.TopLevelDef.MessageDef(msg)  => sharedFileBuilder.addMessageType(msg.toDescriptor)
        case ProtoIR.TopLevelDef.EnumDef(enumDef) => sharedFileBuilder.addEnumType(enumDef.toDescriptor)
        case ProtoIR.TopLevelDef.ServiceDef(_)    =>
      }
      Some(FileDescriptor.buildFrom(sharedFileBuilder.build(), Array.empty))
    } else None

  def add[A: Schema]: Dependency =
    copy(types = types ++ ProtobufCodec.toProtoIR(Schema[A].derive(ProtobufDeriver)))

  def exclude(dependency: Dependency): Dependency =
    copy(types = types -- dependency.types, imports = ProtoIR.Statement.ImportStatement(dependency.dependencyName) :: imports)

  def render(packageName: Option[String], options: List[ProtoIR.TopLevelOption]): String =
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = imports ++ types.map(ProtoIR.Statement.TopLevelStatement(_)).toList.sortBy(_.s.name)
      )
    )
}

object Dependency {
  def fromServices(dependencyName: String, services: Service[?]*): Dependency = {
    val allTypes = services.flatMap(_.toProtoIR).toSet

    val requestResponseTypeNames =
      services.flatMap(_.rpcs.flatMap(rpc => List(rpc.toProtoIR.request.fqn.name, rpc.toProtoIR.response.fqn.name))).toSet

    val commonTypes = allTypes.filterNot {
      case ProtoIR.TopLevelDef.MessageDef(msg)  => requestResponseTypeNames.contains(msg.name)
      case ProtoIR.TopLevelDef.EnumDef(enumDef) => requestResponseTypeNames.contains(enumDef.name)
      case ProtoIR.TopLevelDef.ServiceDef(_)    => true
    }

    Dependency(dependencyName, commonTypes)
  }
}
