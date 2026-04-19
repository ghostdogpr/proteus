package proteus

import zio.blocks.schema.Modifier
import zio.blocks.typeid.TypeId

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
    * A modifier to nest a type inside a specific ancestor message `A`, rather than its direct parent.
    * The target name is resolved at rendering time against the actual rendered name of `A`, so `rename` on `A` is honored.
    * If the target is not among the types visible to the service or dependency being rendered, an error is raised at render time.
    *
    * ```scala
    * case class C(i: Int)
    * case class B(c: C)
    * case class A(b: B)
    *
    * val deriver = ProtobufDeriver.default.modifier[C](Modifiers.nestedIn[A])
    * ```
    */
  inline def nestedIn[A](using typeId: TypeId[A]): Modifier.Reflect =
    Modifier.config(nestedModifier, encodeNestedIn(typeId.fullName))

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
    * A modifier to force a field (or enum/oneof case) to have the given index, and have all subsequent fields
    * continue numbering from that index.
    *
    * For example, given `case class M(a: Int, b: Int, c: Int)` with `reservedFrom(5)` applied to `b`, the resulting
    * indexes will be `a = 1`, `b = 5`, `c = 6` (versus `reserved(5)` which would produce `a = 1`, `c = 2`, `b = 5`).
    */
  def reservedFrom(index: Int): Modifier.config = Modifier.config(reservedFromModifier, index.toString)

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
