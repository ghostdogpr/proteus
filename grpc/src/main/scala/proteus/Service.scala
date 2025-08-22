package proteus

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor

case class Service[Rpcs] private (name: String, rpcs: List[Rpc[?, ?]]) {
  val toProtoIR: List[ProtoIR.TopLevelDef] =
    (ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR))) ::
      rpcs.flatMap(_.messagesToProtoIR)).distinct

  private val typeReferences = toProtoIR.flatMap(_.collectTypeReferences).toSet

  def fileDescriptor(dependencies: List[Dependency]): FileDescriptor = {
    val fileBuilder = FileDescriptorProto.newBuilder().setName(s"${name.toLowerCase}.proto").setPackage("")

    val usedDependencies          = dependencies.filter(_.hasAnyOf(typeReferences))
    val dependencyFileDescriptors = usedDependencies.flatMap(_.fileDescriptor)

    dependencyFileDescriptors.foreach(fileDescriptor => fileBuilder.addDependency(fileDescriptor.getName))

    val dependencyTypes = usedDependencies.flatMap(_.types).map(_.name).toSet

    toProtoIR.filterNot(d => dependencyTypes.contains(d.name)).foreach {
      case ProtoIR.TopLevelDef.MessageDef(msg)     => fileBuilder.addMessageType(msg.toDescriptor): Unit
      case ProtoIR.TopLevelDef.EnumDef(enumDef)    => fileBuilder.addEnumType(enumDef.toDescriptor): Unit
      case ProtoIR.TopLevelDef.ServiceDef(service) => fileBuilder.addService(service.toDescriptor): Unit
    }
    FileDescriptor.buildFrom(fileBuilder.build(), dependencyFileDescriptors.toArray)
  }

  def rpc[Request, Response](rpc: Rpc[Request, Response]): Service[Rpcs & rpc.type] =
    Service(name, rpcs :+ rpc)

  def render(packageName: Option[String], options: List[ProtoIR.TopLevelOption], dependencies: Dependency*): String = {
    val filteredDependencies = dependencies.filter(_.hasAnyOf(typeReferences))
    val dependencyTypes      = filteredDependencies.flatMap(_.types).map(_.name).toSet
    val filteredDefinitions  = toProtoIR.filterNot(d => dependencyTypes.contains(d.name))
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = filteredDependencies.toList.map(d => ProtoIR.Statement.ImportStatement(d.dependencyName)) ++
          filteredDefinitions.map(ProtoIR.Statement.TopLevelStatement(_))
      )
    )
  }
}

object Service {
  def apply(name: String): Service[Any] =
    Service(name, List.empty)
}
