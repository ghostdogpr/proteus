package proteus

import zio.blocks.schema.Modifier

/**
  * An entry binding a term name to a `Modifier.Term`, used by [[ProtobufDeriver.modifier]] to apply
  * modifiers to multiple terms of the same type in a single call.
  *
  * Instances should be created with [[Modifiers.field]] so the singleton type of the name is preserved
  * and compile-time validation against the target type's fields or cases can run.
  */
final class FieldMod[+N <: String & Singleton](val name: N, val modifier: Modifier.Term)
