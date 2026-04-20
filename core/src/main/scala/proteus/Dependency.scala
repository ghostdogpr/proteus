package proteus

import java.nio.charset.StandardCharsets
import java.nio.file.*

import scala.collection.immutable.ListSet

import proteus.internal.*

/**
  * A dependency is a collection of protobuf types.
  * It is typically used to bundle types together and write them to a single .proto file.
  */
final case class Dependency(
  dependencyName: String,
  packageName: Option[String],
  path: Option[String],
  types: ListSet[ProtoIR.TopLevelDef],
  dependencies: List[Dependency]
) {
  private lazy val typeReferences                       = types.flatMap(_.collectTypeReferences)
  private[proteus] val allDependencies: Set[Dependency] = dependencies.toSet ++ dependencies.flatMap(_.allDependencies)
  private[proteus] lazy val filteredDependencies        = allDependencies.filter(_.hasAnyOf(typeReferences))
  private lazy val dependencyTypes                      = filteredDependencies.flatMap(_.types)
  private lazy val filteredTypes                        = types -- dependencyTypes

  private[proteus] lazy val topLevelFqns: Map[String, String] =
    (allDependencies.flatMap(_.topLevelFqns) ++ types.map(t => (t.name, packageName.fold("")(_ + ".") + t.name))).toMap

  private[proteus] lazy val toImportStatement: ProtoIR.Statement.ImportStatement =
    ProtoIR.Statement.ImportStatement(s"${path.fold("")(_ + "/")}$dependencyName.proto")

  private lazy val subDepPaths: Map[String, String] =
    filteredDependencies.toList.flatMap(dep => ProtobufCodec.nestedInPaths(dep.types.toList)).toMap

  private[proteus] def hasAnyOf(typeNames: Set[String]): Boolean =
    types.exists(typeDef => typeNames.contains(typeDef.name))

  /**
    * Adds a new type to the current dependency.
    *
    * @param codec the protobuf codec for the type to add.
    */
  def add[A](using codec: ProtobufCodec[A]): Dependency = {
    val t = ProtobufCodec.toProtoIR(codec).filter(t => !allDependencies.exists(_.hasAnyOf(Set(t.name))))
    copy(types = types ++ t)
  }

  /**
    * Adds a dependency to the current dependency.
    * The types that are already included in the added dependency will be removed from the current dependency.
    *
    * @param dependency the dependency to add.
    */
  def dependsOn(dependency: Dependency): Dependency =
    copy(types = types -- dependency.types -- dependency.dependencyTypes, dependencies = dependencies :+ dependency)

  /**
    * Generates a .proto file for the dependency as a string.
    *
    * @param options options to write at the top of the .proto file.
    */
  def render(options: List[ProtoIR.TopLevelOption]): String = {
    val rawTypes      = filteredTypes.toList
    val ownPaths      = ProtobufCodec.nestedInPaths(rawTypes)
    val resolvedTypes = ProtobufCodec.relocateNestedIn(rawTypes)
    val conflicts     = ProtobufCodec.conflictsOf(resolvedTypes)
    if (conflicts.nonEmpty) {
      throw new ProteusException(
        s"Conflicts found in dependency $dependencyName:\n ${conflicts.map { case (name, defs) => s"- Type `$name` is defined in different ways: \n${defs.mkString("\n")}" }.mkString("\n")}\n"
      )
    }
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = filteredDependencies.toList.map(_.toImportStatement) ++
          ProtobufCodec.qualifyReferences(resolvedTypes, subDepPaths ++ ownPaths).map(ProtoIR.Statement.TopLevelStatement(_))
      )
    )
  }

  /**
    * Generates a .proto file for the dependency and writes it to the given folder.
    *
    * @param options options to write at the top of the .proto file.
    * @param folder the folder to write the .proto file to.
    */
  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String): Unit = {
    val rendered = render(options)
    val fileName = s"${internal.toSnakeCase(dependencyName)}.proto"
    val fullPath = path.fold(Path.of(folder, fileName))(Path.of(folder, _, fileName))
    Files.createDirectories(fullPath.getParent)
    Files.write(fullPath, rendered.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING): Unit
  }

  /**
    * Returns a map of types that are defined in conflicting ways.
    * The key is the type name, and the value is a list of definitions that conflict.
    */
  def findConflicts: Map[String, List[String]] =
    ProtobufCodec.conflictsOf(ProtobufCodec.relocateNestedIn(types.toList))
}

object Dependency {

  /**
    * Creates a new dependency with the given name.
    *
    * @param dependencyName the name of the dependency.
    */
  def apply(dependencyName: String): Dependency =
    Dependency(dependencyName, None, None, ListSet.empty, Nil)

  /**
    * Creates a new dependency with the given name and package name.
    *
    * @param dependencyName the name of the dependency.
    * @param packageName the package name of the dependency.
    */
  def apply(dependencyName: String, packageName: String): Dependency =
    Dependency(dependencyName, Some(packageName), None, ListSet.empty, Nil)

  /**
    * Creates a new dependency with the given name, package name and path.
    *
    * @param dependencyName the name of the dependency.
    * @param packageName the package name of the dependency.
    * @param path the path to the dependency, used when generating import statements and writing to a file.
    */
  def apply(dependencyName: String, packageName: String, path: String): Dependency =
    Dependency(dependencyName, Some(packageName), Some(path), ListSet.empty, Nil)
}
