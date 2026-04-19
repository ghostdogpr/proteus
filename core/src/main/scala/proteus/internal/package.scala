package proteus
package internal

import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

private[proteus] def getFromRegister[A](registers: Registers, offset: RegisterOffset, register: Register[A]): A =
  register match {
    case reg: Register.Object[_] => reg.get(registers, offset)
    case reg: Register.Int       => reg.get(registers, offset)
    case reg: Register.Long      => reg.get(registers, offset)
    case reg: Register.Boolean   => reg.get(registers, offset)
    case reg: Register.Double    => reg.get(registers, offset)
    case reg: Register.Float     => reg.get(registers, offset)
    case reg: Register.Byte      => reg.get(registers, offset)
    case reg: Register.Short     => reg.get(registers, offset)
    case reg: Register.Char      => reg.get(registers, offset)
    case Register.Unit           => Register.Unit.get(registers, offset)
  }

private[proteus] def setToRegister[A](registers: Registers, offset: RegisterOffset, register: Register[A], value: A): Unit =
  register match {
    case reg: Register.Object[_] => reg.set(registers, offset, value)
    case reg: Register.Int       => reg.set(registers, offset, value)
    case reg: Register.Long      => reg.set(registers, offset, value)
    case reg: Register.Boolean   => reg.set(registers, offset, value)
    case reg: Register.Double    => reg.set(registers, offset, value)
    case reg: Register.Float     => reg.set(registers, offset, value)
    case reg: Register.Byte      => reg.set(registers, offset, value)
    case reg: Register.Short     => reg.set(registers, offset, value)
    case reg: Register.Char      => reg.set(registers, offset, value)
    case Register.Unit           => Register.Unit.set(registers, offset, value)
  }

private[proteus] def toSnakeCase(s: String): String =
  s.split("(?=[A-Z])").map(_.toLowerCase).mkString("_")

private[proteus] def toUpperSnakeCase(s: String): String =
  s.split("(?=[A-Z])").map(_.toUpperCase).mkString("_")

private[proteus] def toCamelCase(s: String): String =
  s.split("_").toList match {
    case Nil          => ""
    case head :: tail => head.toLowerCase + tail.map(_.capitalize).mkString
  }

private[proteus] def toUpperCamelCase(s: String): String =
  s.split("_").toList match {
    case Nil   => ""
    case parts => parts.map(_.capitalize).mkString
  }

private[proteus] def typeNameToUpperSnakeCase(s: String): String =
  s.replaceAll("([a-z0-9])([A-Z])", "$1_$2").replaceAll("([A-Z])([A-Z][a-z])", "$1_$2").toUpperCase

private[proteus] def encodeNestedIn(fullName: String): String      =
  s"$nestedInModifierPrefix$fullName"
private[proteus] def decodeNestedIn(value: String): Option[String] =
  if (value.startsWith(nestedInModifierPrefix)) Some(value.drop(nestedInModifierPrefix.length)) else None

private[proteus] val oneOfModifier          = "proteus.oneof"
private[proteus] val nestedModifier         = "proteus.nested"
private[proteus] val nestedInModifierPrefix = "in:"
private[proteus] val excludedModifier       = "proteus.excluded"
private[proteus] val reservedModifier       = "proteus.reserved"
private[proteus] val reservedFromModifier   = "proteus.reserved.from"
private[proteus] val renameModifier         = "proteus.rename"
private[proteus] val enumPrefixModifier     = "proteus.enum.prefix"
private[proteus] val enumSuffixModifier     = "proteus.enum.suffix"
private[proteus] val commentModifier        = "proteus.comment"
private[proteus] val deprecatedModifier     = "proteus.deprecated"
