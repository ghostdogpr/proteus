package proteus

import java.nio.charset.StandardCharsets
import java.nio.file.*

import scala.collection.immutable.ListSet

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor

case class Service[Rpcs] private (
  packageName: Option[String],
  name: String,
  rpcs: List[Rpc[?, ?]],
  dependencies: List[Dependency],
  comment: Option[String]
) {
  val fullyQualifiedName: String = packageName.fold(name)(s => s"$s.$name")

  val toProtoIR: List[ProtoIR.TopLevelDef] =
    (ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR), comment)) ::
      rpcs.flatMap(_.messagesToProtoIR)).distinct

  val allDependencies: Set[Dependency] = dependencies.toSet ++ dependencies.flatMap(_.allDependencies)

  private val typeReferences = toProtoIR.flatMap(_.collectTypeReferences).toSet

  def fileDescriptor(dependencies: List[Dependency]): FileDescriptor = {
    val fileBuilder               = FileDescriptorProto.newBuilder().setName(s"${name.toLowerCase}.proto").setPackage(packageName.getOrElse(""))
    val allDependencies           = dependencies.toSet ++ dependencies.flatMap(_.allDependencies)
    val usedDependencies          = allDependencies.filter(_.hasAnyOf(typeReferences))
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
    Service(packageName, name, rpcs :+ rpc, dependencies, comment)

  def dependsOn(dependency: Dependency): Service[Rpcs] =
    copy(dependencies = dependencies :+ dependency)

  def render(options: List[ProtoIR.TopLevelOption]): String = {
    val filteredDependencies = allDependencies.filter(_.hasAnyOf(typeReferences))
    val dependencyTypes      = filteredDependencies.flatMap(_.types).map(_.name).toSet
    val filteredDefinitions  = toProtoIR.filterNot(d => dependencyTypes.contains(d.name))
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = filteredDependencies.toList.map(_.toImportStatement) ++
          filteredDefinitions.map(ProtoIR.Statement.TopLevelStatement(_))
      )
    )
  }

  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String, fileName: String): Unit = {
    val rendered = render(options)
    val path     = Path.of(folder, fileName)
    Files.createDirectories(path.getParent)
    Files.write(path, rendered.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING): Unit
  }

  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String): Unit =
    renderToFile(options, folder, internal.toSnakeCase(name))
}

object Service {
  def apply(name: String): Service[Any] =
    Service(None, name, List.empty, Nil, None)

  def apply(packageName: String, name: String): Service[Any] =
    Service(Some(packageName), name, List.empty, Nil, None)

  def apply(packageName: String, name: String, comment: String): Service[Any] =
    Service(Some(packageName), name, List.empty, Nil, Some(comment))
}

extension (dep: Dependency.type) {
  def fromServices(dependencyName: String, services: Service[?]*): Dependency =
    fromServices(dependencyName, None, None, services*)

  def fromServices(dependencyName: String, packageName: String, services: Service[?]*): Dependency =
    fromServices(dependencyName, Some(packageName), None, services*)

  def fromServices(dependencyName: String, packageName: String, path: String, services: Service[?]*): Dependency =
    fromServices(dependencyName, Some(packageName), Some(path), services*)

  private def fromServices(dependencyName: String, packageName: Option[String], path: Option[String], services: Service[?]*): Dependency = {
    val allTypes = ListSet.from(services.flatMap(_.toProtoIR))

    val requestResponseTypeNames =
      services.flatMap(_.rpcs.flatMap(rpc => List(rpc.toProtoIR.request.fqn.name, rpc.toProtoIR.response.fqn.name))).toSet

    val commonTypes = allTypes.filterNot {
      case ProtoIR.TopLevelDef.MessageDef(msg)  => requestResponseTypeNames.contains(msg.name)
      case ProtoIR.TopLevelDef.EnumDef(enumDef) => requestResponseTypeNames.contains(enumDef.name)
      case ProtoIR.TopLevelDef.ServiceDef(_)    => true
    }

    Dependency(dependencyName, packageName, path, commonTypes, Nil)
  }
}
