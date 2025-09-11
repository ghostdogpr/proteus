package proteus

import zio.blocks.schema.Modifier

import proteus.internal.*

object Modifiers {
  val excluded: Modifier.Term                      = Modifier.config(excludedModifier, "true")
  val nested: Modifier.Reflect                     = Modifier.config(nestedModifier, "true")
  val oneOf: Modifier.Reflect                      = Modifier.config(oneOfModifier, "")
  def rename(name: String): Modifier.Reflect       = Modifier.config(renameModifier, name)
  def enumPrefix(prefix: String): Modifier.Reflect = Modifier.config(enumPrefixModifier, prefix)
  def comment(comment: String): Modifier.config    = Modifier.config(commentModifier, comment)
  def reserved(indexes: Int*): Modifier.config     = Modifier.config(reservedModifier, indexes.mkString(","))
  def oneOf(flags: OneOfFlag*): Modifier.Reflect   = Modifier.config(oneOfModifier, flags.mkString(",").toLowerCase)

  enum OneOfFlag {
    case Inline, Nested
  }
}
