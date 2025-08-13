package proteus

import com.google.protobuf.CodedOutputStream
import zio.blocks.schema.binding.RegisterOffset.*
import zio.blocks.schema.binding.Registers

sealed trait ProtoWriter

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object ProtoWriter {
  final case class BoolPrimitive(a: Boolean, id: Int)  extends ProtoWriter {
    val innerSize: Int                               = if (id == -1) CodedOutputStream.computeBoolSizeNoTag(a) else CodedOutputStream.computeBoolSize(id, a)
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeBoolNoTag(a) else output.writeBool(id, a)
  }
  final case class FloatPrimitive(a: Float, id: Int)   extends ProtoWriter {
    val innerSize: Int                               = if (id == -1) CodedOutputStream.computeFloatSizeNoTag(a) else CodedOutputStream.computeFloatSize(id, a)
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeFloatNoTag(a) else output.writeFloat(id, a)
  }
  final case class DoublePrimitive(a: Double, id: Int) extends ProtoWriter {
    val innerSize: Int                               = if (id == -1) CodedOutputStream.computeDoubleSizeNoTag(a) else CodedOutputStream.computeDoubleSize(id, a)
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeDoubleNoTag(a) else output.writeDouble(id, a)
  }
  final case class IntPrimitive(a: Int, id: Int)       extends ProtoWriter {
    val innerSize: Int                               = if (id == -1) CodedOutputStream.computeInt32SizeNoTag(a) else CodedOutputStream.computeInt32Size(id, a)
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeInt32NoTag(a) else output.writeInt32(id, a)
  }
  final case class LongPrimitive(a: Long, id: Int)     extends ProtoWriter {
    val innerSize: Int                               = if (id == -1) CodedOutputStream.computeInt64SizeNoTag(a) else CodedOutputStream.computeInt64Size(id, a)
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeInt64NoTag(a) else output.writeInt64(id, a)
  }
  final case class StringPrimitive(a: String, id: Int) extends ProtoWriter {
    val innerSize: Int                               = if (id == -1) CodedOutputStream.computeStringSizeNoTag(a) else CodedOutputStream.computeStringSize(id, a)
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeStringNoTag(a) else output.writeString(id, a)
  }

  final case class Message(id: Int, fields: List[ProtoWriter], oneOfOrRepeated: Boolean) extends ProtoWriter {
    val innerSize: Int                               = {
      var size      = 0
      var remaining = fields
      while (remaining ne Nil) {
        size += ProtoWriter.fullSize(remaining.head)
        remaining = remaining.tail
      }
      size
    }
    val nonEmpty: Boolean                            = oneOfOrRepeated || innerSize != 0
    val fullSize: Int                                =
      if (nonEmpty) 1 + CodedOutputStream.computeUInt32SizeNoTag(innerSize) + innerSize else 0
    def write(using output: CodedOutputStream): Unit = {
      if (id != -1 && nonEmpty) {
        output.writeTag(id, 2)
        output.writeUInt32NoTag(innerSize)
      }
      var remaining = fields
      while (remaining ne Nil) {
        ProtoWriter.write(remaining.head)
        remaining = remaining.tail
      }
    }
  }

  final case class Repeated(elements: List[ProtoWriter], id: Int, packed: Boolean) extends ProtoWriter {
    val innerSize: Int                               = {
      var size      = 0
      var remaining = elements
      while (remaining ne Nil) {
        size += ProtoWriter.fullSize(remaining.head)
        remaining = remaining.tail
      }
      size
    }
    val fullSize: Int                                =
      if (packed && (elements ne Nil)) 1 + CodedOutputStream.computeUInt32SizeNoTag(innerSize) + innerSize
      else innerSize
    def write(using output: CodedOutputStream): Unit = {
      if (packed && (elements ne Nil)) {
        output.writeTag(id, 2)
        output.writeUInt32NoTag(innerSize)
      }
      var remaining = elements
      while (remaining ne Nil) {
        ProtoWriter.write(remaining.head)
        remaining = remaining.tail
      }
    }
  }

  def write(writer: ProtoWriter)(using output: CodedOutputStream): Unit =
    writer match {
      case f: ProtoWriter.Message         => f.write
      case f: ProtoWriter.IntPrimitive    => f.write
      case f: ProtoWriter.LongPrimitive   => f.write
      case f: ProtoWriter.StringPrimitive => f.write
      case f: ProtoWriter.BoolPrimitive   => f.write
      case f: ProtoWriter.DoublePrimitive => f.write
      case f: ProtoWriter.Repeated        => f.write
      case f: ProtoWriter.FloatPrimitive  => f.write
    }

  def innerSize(writer: ProtoWriter): Int =
    writer match {
      case f: ProtoWriter.Message         => f.innerSize
      case f: ProtoWriter.IntPrimitive    => f.innerSize
      case f: ProtoWriter.LongPrimitive   => f.innerSize
      case f: ProtoWriter.StringPrimitive => f.innerSize
      case f: ProtoWriter.BoolPrimitive   => f.innerSize
      case f: ProtoWriter.DoublePrimitive => f.innerSize
      case f: ProtoWriter.Repeated        => f.innerSize
      case f: ProtoWriter.FloatPrimitive  => f.innerSize
    }

  def fullSize(writer: ProtoWriter): Int =
    writer match {
      case f: ProtoWriter.Message         => f.fullSize
      case f: ProtoWriter.IntPrimitive    => f.innerSize
      case f: ProtoWriter.LongPrimitive   => f.innerSize
      case f: ProtoWriter.StringPrimitive => f.innerSize
      case f: ProtoWriter.BoolPrimitive   => f.innerSize
      case f: ProtoWriter.DoublePrimitive => f.innerSize
      case f: ProtoWriter.Repeated        => f.fullSize
      case f: ProtoWriter.FloatPrimitive  => f.innerSize
    }
}
