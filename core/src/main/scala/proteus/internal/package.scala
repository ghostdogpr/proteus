package proteus.internal

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
