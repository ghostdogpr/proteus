package proteus

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.Descriptors.FileDescriptor
import zio.blocks.schema.Schema

case class Dependency private (dependencyName: String, types: Set[ProtoIR.TopLevelDef], dependencies: List[Dependency]) {
  val typeReferences       = types.flatMap(_.collectTypeReferences).toSet
  val filteredDependencies = dependencies.filter(_.hasAnyOf(typeReferences))

  val fileDescriptor: Option[FileDescriptor] =
    if (types.nonEmpty) {
      val sharedFileBuilder = FileDescriptorProto.newBuilder().setName(s"$dependencyName").setPackage("")
      types.foreach {
        case ProtoIR.TopLevelDef.MessageDef(msg)  => sharedFileBuilder.addMessageType(msg.toDescriptor)
        case ProtoIR.TopLevelDef.EnumDef(enumDef) => sharedFileBuilder.addEnumType(enumDef.toDescriptor)
        case ProtoIR.TopLevelDef.ServiceDef(_)    =>
      }
      Some(FileDescriptor.buildFrom(sharedFileBuilder.build(), filteredDependencies.flatMap(_.fileDescriptor).toArray))
    } else None

  def add[A: Schema]: Dependency =
    add(Schema[A].derive(ProtobufDeriver))

  def add[A](codec: ProtobufCodec[A]): Dependency = {
    val t = ProtobufCodec.toProtoIR(codec)
    if (dependencies.exists(_.hasAnyOf(t.map(_.name).toSet))) this
    else copy(types = types ++ t)
  }

  def hasAnyOf(typeNames: Set[String]): Boolean =
    types.exists(typeDef => typeNames.contains(typeDef.name))

  def dependsOn(dependency: Dependency): Dependency =
    copy(types = types -- dependency.types, dependencies = dependencies :+ dependency)

  def render(packageName: Option[String], options: List[ProtoIR.TopLevelOption]): String = {
    val dependencyTypes = filteredDependencies.flatMap(_.types).map(_.name).toSet
    val filteredTypes   = types.filterNot(d => dependencyTypes.contains(d.name))

    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = filteredDependencies.toList.map(d => ProtoIR.Statement.ImportStatement(d.dependencyName)) ++
          filteredTypes.map(ProtoIR.Statement.TopLevelStatement(_)).toList.sortBy(_.s.name)
      )
    )
  }
}

object Dependency {
  def apply(dependencyName: String): Dependency =
    Dependency(dependencyName, Set.empty, Nil)

  private def apply(dependencyName: String, types: Set[ProtoIR.TopLevelDef]): Dependency =
    Dependency(dependencyName, types, Nil)

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
