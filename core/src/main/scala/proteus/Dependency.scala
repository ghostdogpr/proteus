package proteus

import java.nio.charset.StandardCharsets
import java.nio.file.*

import zio.blocks.schema.Schema

case class Dependency(packageName: Option[String], dependencyName: String, types: Set[ProtoIR.TopLevelDef], dependencies: List[Dependency]) {
  val typeReferences       = types.flatMap(_.collectTypeReferences).toSet
  val filteredDependencies = dependencies.filter(_.hasAnyOf(typeReferences))

  def add[A: Schema](using deriver: ProtobufDeriver): Dependency =
    add(Schema[A].derive(deriver))

  def add[A](codec: ProtobufCodec[A]): Dependency = {
    val t = ProtobufCodec.toProtoIR(codec)
    if (dependencies.exists(_.hasAnyOf(t.map(_.name).toSet))) this
    else copy(types = types ++ t)
  }

  def hasAnyOf(typeNames: Set[String]): Boolean =
    types.exists(typeDef => typeNames.contains(typeDef.name))

  def dependsOn(dependency: Dependency): Dependency =
    copy(types = types -- dependency.types, dependencies = dependencies :+ dependency)

  def render(options: List[ProtoIR.TopLevelOption]): String = {
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

  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String): Unit = {
    val rendered = render(options)
    val fileName = internal.toSnakeCase(dependencyName)
    val path     = Path.of(folder, s"$fileName.proto")
    Files.createDirectories(path.getParent)
    Files.write(path, rendered.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING): Unit
  }
}

object Dependency {
  def apply(dependencyName: String): Dependency =
    Dependency(None, dependencyName, Set.empty, Nil)

  def apply(packageName: String, dependencyName: String): Dependency =
    Dependency(Some(packageName), dependencyName, Set.empty, Nil)
}
