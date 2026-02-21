package proteus

import scala.quoted.*

import zio.blocks.schema.Modifier
import zio.blocks.schema.Schema
import zio.blocks.typeid.TypeId

/**
  * Macro implementations for [[ProtobufDeriver]] extension methods.
  *
  * Using macros instead of `inline def` with `summonFrom` produces single-line bytecode
  * at call sites (no intermediate local variables), which matters for chained calls like
  * `deriver.instance[A](codec).modifier[B](mod)`.
  */
object ProtobufDeriverMacros {

  /**
    * Macro for `instance[A](codec)`. Resolves `TypeId[A]` at compile time.
    */
  def instanceImpl[A: Type](instance: Expr[ProtobufCodec[A]], self: Expr[ProtobufDeriver])(using Quotes): Expr[ProtobufDeriver] = {
    val typeId = summonTypeId[A]
    '{ $self.addInstance($typeId, $instance) }
  }

  /**
    * Macro for `instance[A, B](termName, codec)`. Resolves `TypeId[A]` at compile time
    * and validates that:
    *  - the term exists in `A` (as a case class field or enum/sealed trait member)
    *  - the instance type `B` matches the term's declared type
    */
  def termInstanceImpl[A: Type, B: Type](termName: Expr[String], instance: Expr[ProtobufCodec[B]], self: Expr[ProtobufDeriver])(
    using Quotes
  ): Expr[ProtobufDeriver] = {
    import quotes.reflect.*

    val termNameStr = termName.valueOrAbort
    val sym         = TypeRepr.of[A].typeSymbol
    val fields      = sym.caseFields

    // For case classes, validate field name and type
    if (fields.nonEmpty) {
      fields.find(_.name == termNameStr) match {
        case None        => report.errorAndAbort(s"Field $termNameStr does not exist in class ${sym.name}.")
        case Some(field) =>
          val fieldType = TypeRepr.of[A].memberType(field)
          if (!(TypeRepr.of[B] =:= fieldType))
            report.errorAndAbort(
              s"Instance type mismatch for field $termNameStr in ${sym.name}: expected ${fieldType.show}, got ${TypeRepr.of[B].show}."
            )
      }
    } else {
      // For sealed traits/enums, validate case name and type
      val children = sym.children
      if (children.nonEmpty) {
        children.find(_.name == termNameStr) match {
          case None        => report.errorAndAbort(s"Case $termNameStr does not exist in sealed trait or enum ${sym.name}.")
          case Some(child) =>
            if (!(TypeRepr.of[B] =:= child.typeRef))
              report.errorAndAbort(
                s"Instance type mismatch for case $termNameStr in ${sym.name}: expected ${child.typeRef.show}, got ${TypeRepr.of[B].show}."
              )
        }
      } else {
        report.errorAndAbort(s"Type ${sym.name} is not a case class, sealed trait, or enum.")
      }
    }

    val typeId = summonTypeId[A]
    '{ $self.addInstance($typeId, $termName, $instance) }
  }

  /**
    * Macro for `modifier[A](modifier)`. Resolves `TypeId[A]` at compile time.
    */
  def modifierImpl[A: Type](modifier: Expr[Modifier.Reflect], self: Expr[ProtobufDeriver])(using Quotes): Expr[ProtobufDeriver] = {
    val typeId = summonTypeId[A]
    '{ $self.addModifier($typeId, $modifier) }
  }

  /**
    * Macro for `modifier[A](termName, modifier)`. Resolves `TypeId[A]` at compile time
    * and validates that the term exists in `A` (as a case class field or enum/sealed trait member).
    */
  def termModifierImpl[A: Type](termName: Expr[String], modifier: Expr[Modifier.Term], self: Expr[ProtobufDeriver])(
    using Quotes
  ): Expr[ProtobufDeriver] = {
    import quotes.reflect.*

    val termNameStr = termName.valueOrAbort
    val sym         = TypeRepr.of[A].typeSymbol
    val fields      = sym.caseFields.map(_.name)

    // For case classes, validate field name
    if (fields.nonEmpty) {
      if (!fields.contains(termNameStr))
        report.errorAndAbort(s"Field $termNameStr does not exist in class ${sym.name}.")
    } else {
      // For sealed traits/enums, validate case name
      val children = sym.children.map(_.name)
      if (children.nonEmpty) {
        if (!children.contains(termNameStr))
          report.errorAndAbort(s"Case $termNameStr does not exist in sealed trait or enum ${sym.name}.")
      } else {
        report.errorAndAbort(s"Type ${sym.name} is not a case class, sealed trait, or enum.")
      }
    }

    val typeId = summonTypeId[A]
    '{ $self.addModifier($typeId, $termName, $modifier) }
  }

  /**
    * Resolves `TypeId[A]` at compile time using the best available source:
    *  1. From `Schema[A].reflect.typeId` if a schema is in scope
    *  2. From a given `TypeId[A]` instance
    *  3. Via `TypeId.derived[A]` as a last resort
    */
  private def summonTypeId[A: Type](using Quotes): Expr[TypeId[A]] =
    Expr.summon[Schema[A]] match {
      case Some(schema) => '{ $schema.reflect.typeId }
      case None         =>
        Expr.summon[TypeId[A]] match {
          case Some(typeId) => typeId
          case None         => '{ TypeId.derived[A] }
        }
    }
}
