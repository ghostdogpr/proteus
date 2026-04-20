package proteus

import java.nio.charset.StandardCharsets
import java.nio.file.*

import scala.collection.immutable.ListSet

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor

import proteus.internal.*

/**
  * A gRPC service definition.
  *
  * @param packageName an optional package name for the service.
  * @param name the name of the service.
  * @param rpcs the RPCs of the service.
  * @param dependencies the dependencies of the service.
  * @param comment an optional comment attached to the service.
  */
case class Service[Rpcs] private (
  packageName: Option[String],
  name: String,
  rpcs: List[Rpc[?, ?]],
  dependencies: List[Dependency],
  comment: Option[String]
) {

  /**
    * The fully qualified name of the service (including the package name).
    */
  val fullyQualifiedName: String = packageName.fold(name)(s => s"$s.$name")

  private case class ProtoIRResult(defs: List[ProtoIR.TopLevelDef], nestedInPaths: Map[String, String])

  // Defs are kept un-relocated so that, when aggregated across services via `fromServices`, contributions
  // for the same Scala type merge cleanly before relocation runs once at the dependency level.
  private lazy val protoIRResult: ProtoIRResult = {
    val raw         = ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR), comment)) ::
      rpcs.flatMap(_.messagesToProtoIR)
    val deduped     = raw.distinctBy(ProtobufCodec.dedupKey)
    val nestedPaths = ProtobufCodec.nestedInPaths(deduped)
    ProtoIRResult(deduped, nestedPaths)
  }

  /**
    * Converts the service to a ProtoIR representation.
    */
  lazy val toProtoIR: List[ProtoIR.TopLevelDef] = protoIRResult.defs

  private lazy val nestedInPaths: Map[String, String] = protoIRResult.nestedInPaths

  /**
    * All the dependencies of the service (including transitive dependencies).
    */
  val allDependencies: Set[Dependency] = dependencies.toSet ++ dependencies.flatMap(_.allDependencies)

  private lazy val typeReferences = toProtoIR.flatMap(_.collectTypeReferences).toSet

  private lazy val filteredTypes          = {
    val dependencyTypes = allDependencies.filter(_.hasAnyOf(typeReferences)).flatMap(_.types).map(_.name).toSet
    ProtobufCodec.relocateNestedIn(toProtoIR.filterNot(d => dependencyTypes.contains(d.name)))
  }
  private lazy val filteredTypeReferences = filteredTypes.flatMap(_.collectTypeReferences).toSet
  private lazy val usedDependencies       = allDependencies.filter(_.hasAnyOf(filteredTypeReferences))

  /**
    * Converts the service to a gRPC file descriptor for the reflection service.
    */
  lazy val fileDescriptor: FileDescriptor = {
    val fileName                  = packageName.fold(toSnakeCase(name))(p => s"${p.replace('.', '/')}/${toSnakeCase(name)}")
    val fileBuilder               = FileDescriptorProto.newBuilder().setName(s"$fileName.proto").setPackage(packageName.getOrElse(""))
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

  /**
    * Adds a new RPC to the service.
    */
  def rpc[Request, Response](rpc: Rpc[Request, Response]): Service[Rpcs & rpc.type] =
    Service(packageName, name, rpcs :+ rpc, dependencies, comment)

  /**
    * Adds a new dependency to the service.
    */
  def dependsOn(dependency: Dependency): Service[Rpcs] =
    copy(dependencies = dependencies :+ dependency)

  /**
    * Renders the service to a string representation of a .proto file.
    *
    * @param options options to write at the top of the .proto file.
    */
  def render(options: List[ProtoIR.TopLevelOption]): String = {
    val conflicts = findConflicts
    if (conflicts.nonEmpty) {
      throw new ProteusException(
        s"Conflicts found in service $name:\n ${conflicts.map { case (name, defs) => s"- Type `$name` is defined in different ways: \n${defs.mkString("\n")}" }.mkString("\n")}\n"
      )
    }
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = usedDependencies.toList.map(_.toImportStatement) ++
          ProtobufCodec.qualifyReferences(filteredTypes, nestedInPaths).map(ProtoIR.Statement.TopLevelStatement(_))
      )
    )
  }

  /**
    * Renders the service to a .proto file and writes it to the given folder.
    *
    * @param options options to write at the top of the .proto file.
    * @param folder the folder to write the .proto file to.
    * @param fileName the name of the .proto file.
    */
  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String, fileName: String): Unit = {
    val rendered       = render(options)
    val normalizedName = if (fileName.endsWith(".proto")) fileName else s"$fileName.proto"
    val path           = Path.of(folder, normalizedName)
    Files.createDirectories(path.getParent)
    Files.write(path, rendered.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING): Unit
  }

  /**
    * Renders the service to a .proto file and writes it to the given folder.
    *
    * @param options options to write at the top of the .proto file.
    * @param folder the folder to write the .proto file to.
    */
  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String): Unit =
    renderToFile(options, folder, internal.toSnakeCase(name))

  /**
    * A methods that detects if there are any conflicts in the service.
    * A conflict is a situation where the same type name is defined in different ways.
    */
  def findConflicts: Map[String, List[String]] =
    ProtobufCodec.conflictsOf(filteredTypes)
}

object Service {

  /**
    * Creates a new service.
    *
    * @param name the name of the service.
    */
  def apply(name: String): Service[Any] =
    Service(None, name, List.empty, Nil, None)

  /**
    * Creates a new service.
    *
    * @param packageName the package name of the service.
    * @param name the name of the service.
    */
  def apply(packageName: String, name: String): Service[Any] =
    Service(Some(packageName), name, List.empty, Nil, None)

  /**
    * Creates a new service.
    *
    * @param packageName the package name of the service.
    * @param name the name of the service.
    * @param comment a comment attached to the service.
    */
  def apply(packageName: String, name: String, comment: String): Service[Any] =
    Service(Some(packageName), name, List.empty, Nil, Some(comment))
}

extension (dep: Dependency.type) {

  /**
    * Creates a new dependency from a list of services.
    * The dependency will contain all the types used in the services except for the request and response types of the RPCs.
    *
    * @param dependencyName the name of the dependency.
    * @param services the services to create the dependency from.
    */
  def fromServices(dependencyName: String, services: Service[?]*): Dependency =
    fromServices(dependencyName, None, None, services*)

  /**
    * Creates a new dependency from a list of services.
    * The dependency will contain all the types used in the services except for the request and response types of the RPCs.
    *
    * @param dependencyName the name of the dependency.
    * @param packageName the package name of the dependency.
    * @param services the services to create the dependency from.
    */
  def fromServices(dependencyName: String, packageName: String, services: Service[?]*): Dependency =
    fromServices(dependencyName, Some(packageName), None, services*)

  /**
    * Creates a new dependency from a list of services.
    * The dependency will contain all the types used in the services except for the request and response types of the RPCs.
    *
    * @param dependencyName the name of the dependency.
    * @param packageName the package name of the dependency.
    * @param path the path to the dependency.
    * @param services the services to create the dependency from.
    */
  def fromServices(dependencyName: String, packageName: String, path: String, services: Service[?]*): Dependency =
    fromServices(dependencyName, Some(packageName), Some(path), services*)

  private def fromServices(dependencyName: String, packageName: Option[String], path: Option[String], services: Service[?]*): Dependency = {
    val allTypes      = ListSet.from(services.flatMap(_.toProtoIR))
    val dependencies  = services.toList.flatMap(_.allDependencies).distinct
    val filteredTypes = allTypes.filterNot(t => dependencies.exists(_.hasAnyOf(Set(t.name))))

    val requestResponseTypeNames =
      services.flatMap(_.rpcs.flatMap(rpc => List(rpc.toProtoIR.request.fqn.name, rpc.toProtoIR.response.fqn.name))).toSet

    // A type whose `nestedIn` chain roots at a request/response must travel with that parent (which
    // stays in the service file), so it doesn't belong in the shared dependency.
    val byTypeId                                                                               = filteredTypes.iterator.flatMap(d => d.typeId.map(_ -> d)).toMap
    def rootsAtRequestResponse(d: ProtoIR.TopLevelDef, seen: Set[String] = Set.empty): Boolean =
      d.nestedIn match {
        case Some(parentTid) if !seen.contains(parentTid) =>
          byTypeId.get(parentTid) match {
            case Some(parent) =>
              requestResponseTypeNames.contains(parent.name) || rootsAtRequestResponse(parent, seen + parentTid)
            case None         => false
          }
        case _                                            => false
      }

    val commonTypes = filteredTypes.filterNot {
      case _: ProtoIR.TopLevelDef.ServiceDef => true
      case d                                 => requestResponseTypeNames.contains(d.name) || rootsAtRequestResponse(d)
    }

    Dependency(dependencyName, packageName, path, commonTypes, dependencies)
  }
}
