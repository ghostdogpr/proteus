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

  lazy val toProtoIR: List[ProtoIR.TopLevelDef] =
    (ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR), comment)) ::
      rpcs.flatMap(_.messagesToProtoIR)).distinct

  val allDependencies: Set[Dependency] = dependencies.toSet ++ dependencies.flatMap(_.allDependencies)

  private lazy val typeReferences = toProtoIR.flatMap(_.collectTypeReferences).toSet

  private lazy val filteredTypes          = {
    val dependencyTypes = allDependencies.filter(_.hasAnyOf(typeReferences)).flatMap(_.types).map(_.name).toSet
    toProtoIR.filterNot(d => dependencyTypes.contains(d.name))
  }
  private lazy val filteredTypeReferences = filteredTypes.flatMap(_.collectTypeReferences).toSet
  private lazy val usedDependencies       = allDependencies.filter(_.hasAnyOf(filteredTypeReferences))

  lazy val fileDescriptor: FileDescriptor = {
    val fileBuilder               = FileDescriptorProto.newBuilder().setName(s"${name.toLowerCase}.proto").setPackage(packageName.getOrElse(""))
    val dependencyFileDescriptors = usedDependencies.flatMap(_.fileDescriptor)

    dependencyFileDescriptors.foreach(fileDescriptor => fileBuilder.addDependency(fileDescriptor.getName))

    val topLevelFqns: Map[String, String] =
      (usedDependencies.flatMap(_.topLevelFqns) ++ filteredTypes.map(t => (t.name, packageName.fold("")(_ + ".") + t.name))).toMap

    filteredTypes.foreach {
      case ProtoIR.TopLevelDef.MessageDef(msg)     =>
        fileBuilder.addMessageType(msg.toDescriptor(packageName.fold("")(_ + ".") + msg.name, topLevelFqns)): Unit
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
    val conflicts = findConflicts
    if (conflicts.nonEmpty) {
      throw new Exception(
        s"Conflicts found in service $name:\n ${conflicts.map { case (name, defs) => s"- Type `$name` is defined in different ways: \n${defs.mkString("\n")}" }.mkString("\n")}\n"
      )
    }
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = usedDependencies.toList.map(_.toImportStatement) ++
          filteredTypes.map(ProtoIR.Statement.TopLevelStatement(_))
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

  def findConflicts: Map[String, List[String]] =
    filteredTypes
      .groupBy(_.name)
      .view
      .mapValues(_.map(Renderer.renderTopLevelDef).map(Text.renderText).toList.distinct)
      .toMap
      .filter((_, values) => values.length > 1)
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
    val allTypes      = ListSet.from(services.flatMap(_.toProtoIR))
    val dependencies  = services.toList.flatMap(_.allDependencies).distinct
    val filteredTypes = allTypes.filterNot(t => dependencies.exists(_.hasAnyOf(Set(t.name))))

    val requestResponseTypeNames =
      services.flatMap(_.rpcs.flatMap(rpc => List(rpc.toProtoIR.request.fqn.name, rpc.toProtoIR.response.fqn.name))).toSet

    val commonTypes = filteredTypes.filterNot {
      case ProtoIR.TopLevelDef.MessageDef(msg)  => requestResponseTypeNames.contains(msg.name)
      case ProtoIR.TopLevelDef.EnumDef(enumDef) => requestResponseTypeNames.contains(enumDef.name)
      case ProtoIR.TopLevelDef.ServiceDef(_)    => true
    }

    Dependency(dependencyName, packageName, path, commonTypes, dependencies)
  }
}
