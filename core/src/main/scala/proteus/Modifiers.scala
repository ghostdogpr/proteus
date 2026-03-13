package proteus

import zio.blocks.schema.Modifier

import proteus.internal.*

/**
  * Modifiers are used to customize the derivation process.
  */
object Modifiers {

  /**
    * A modifier to exclude a field or an enum member from the protobuf type.
    */
  val excluded: Modifier.Term = Modifier.config(excludedModifier, "true")

  /**
    * A modifier to mark a field as deprecated.
    */
  val deprecated: Modifier.Term = Modifier.config(deprecatedModifier, "true")

  /**
    * A modifier to nest a type inside its parent message.
    */
  val nested: Modifier.Reflect = Modifier.config(nestedModifier, "true")

  /**
    * A modifier to force a type to be defined at the root level.
    */
  val unnested: Modifier.Reflect = Modifier.config(nestedModifier, "false")

  /**
    * A modifier to force a type to be encoded as a oneOf.
    * Sum types with no fields are encoded as enums by default and this modifier can be used to force them to be encoded as a oneOf instead.
    * You can also use this modifier on a optional field to force it to be encoded as a oneOf instead of an optional field.
    */
  val oneOf: Modifier.config = Modifier.config(oneOfModifier, "")

  /**
    * A modifier to control how oneof types are encoded, see [[OneOfFlag]] for more details.
    */
  def oneOf(flags: OneOfFlag*): Modifier.Reflect = Modifier.config(oneOfModifier, flags.mkString(",").toLowerCase)

  /**
    * A modifier to prefix members of an enum type with a string.
    */
  def enumPrefix(prefix: String): Modifier.Reflect = Modifier.config(enumPrefixModifier, prefix)

  /**
    * A modifier to suffix members of an enum type with a string.
    */
  def enumSuffix(suffix: String): Modifier.Reflect = Modifier.config(enumSuffixModifier, suffix)

  /**
    * A modifier to add a comment to a type, field or enum member.
    */
  def comment(comment: String): Modifier.config = Modifier.config(commentModifier, comment)

  /**
    * A modifier to rename a type, field or enum member.
    */
  def rename(name: String): Modifier.config = Modifier.config(renameModifier, name)

  /**
    * A modifier to add some reserved indexes to a type.
    * Those reserved indexes will be skipped when deriving the protobuf type.
    * If this modifier is applied to a field, instead the field will use the given index(es).
    */
  def reserved(indexes: Int*): Modifier.config = Modifier.config(reservedModifier, indexes.mkString(","))

  /**
    * Flags for the oneOf modifier.
    */
  enum OneOfFlag {

    /**
      * This flag will make the oneof inlined as a single field inside the parent message.
      *
      * ```protobuf
      * message Example {
      *   int32 field = 1;
      *   oneof address { // inline oneof field
      *     Email email = 2;
      *     Phone phone = 3;
      *   }
      * }
      * ```
      */
    case Inline

    /**
      * This flag will make the oneof as a separate type nested inside the parent message.
      *
      * ```protobuf
      * message Example {
      *   message Address { // nested oneof type
      *     oneof value {
      *       Email email = 1;
      *       Phone phone = 2;
      *     }
      *   }
      *
      *   int32 field = 1;
      *   Address address = 2;
      * }
      * ```
      */
    case Nested
  }
}
