package proteus

import java.time.*

import scala.collection.immutable.HashMap

import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

private def isOptional[A](using schema: Reflect.Bound[A]): Boolean =
  schema match {
    case e: Reflect.Variant.Bound[?] => e.typeName == TypeName.option
    case Reflect.Deferred(schema0)   => isOptional(using schema0())
    case _                           => false
  }

private def isEnum(e: Reflect.Variant.Bound[?]): Boolean =
  e.cases.forall(c =>
    innerSchema(c.value) match {
      case record: Reflect.Record.Bound[?] => record.fields.length == 0
      case _                               => false
    }
  ) // && !e.annotations.exists { case oneof() => true; case _ => false }

private def innerSchema(schema: Reflect.Bound[?]): Reflect.Bound[?] =
  schema match {
    case Reflect.Deferred(value) => innerSchema(value())
    case _                       => schema
  }

def getFromRegister[A](registers: Registers, offset: RegisterOffset, register: Register[A]): A =
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

def setToRegister[A](registers: Registers, offset: RegisterOffset, register: Register[A], value: A): Unit =
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

type DateTime = OffsetDateTime

object DateTime {
  def unsafeSystemNow(): DateTime =
    OffsetDateTime.now()

  val timeZoneId: ZoneId = ZoneId.of("Asia/Seoul")

  val min: DateTime = OffsetDateTime.ofInstant(Instant.EPOCH, timeZoneId)

  def ofEpochMilli(millis: Long, zoneId: ZoneId = timeZoneId): DateTime = {
    val instant = Instant.ofEpochMilli(millis)

    OffsetDateTime.ofInstant(instant, zoneId)
  }
}

extension (dateTime: DateTime) {
  def toEpochMilli: Long =
    dateTime.toInstant.toEpochMilli
}
