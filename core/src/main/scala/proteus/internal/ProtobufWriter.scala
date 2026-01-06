package proteus
package internal

import com.google.protobuf.CodedOutputStream

/**
  * Data structure for writing protobuf messages.
  * Required to avoid calculating sizes multiple times.
  */
sealed abstract private[proteus] class ProtobufWriter(val size: Int) {
  def write(using output: CodedOutputStream): Unit
}

private[proteus] object ProtobufWriter {
  final case class BoolPrimitive(a: Boolean, id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeBoolSizeNoTag(a) else CodedOutputStream.computeBoolSize(id, a))     {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeBoolNoTag(a) else output.writeBool(id, a)
  }
  final case class FloatPrimitive(a: Float, id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeFloatSizeNoTag(a) else CodedOutputStream.computeFloatSize(id, a))   {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeFloatNoTag(a) else output.writeFloat(id, a)
  }
  final case class DoublePrimitive(a: Double, id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeDoubleSizeNoTag(a) else CodedOutputStream.computeDoubleSize(id, a)) {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeDoubleNoTag(a) else output.writeDouble(id, a)
  }
  final case class IntPrimitive(a: Int, id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeInt32SizeNoTag(a) else CodedOutputStream.computeInt32Size(id, a))   {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeInt32NoTag(a) else output.writeInt32(id, a)
  }
  final case class LongPrimitive(a: Long, id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeInt64SizeNoTag(a) else CodedOutputStream.computeInt64Size(id, a))   {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeInt64NoTag(a) else output.writeInt64(id, a)
  }
  final case class StringPrimitive(a: String, id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeStringSizeNoTag(a) else CodedOutputStream.computeStringSize(id, a)) {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeStringNoTag(a) else output.writeString(id, a)
  }

  final case class Message(id: Int, fields: List[ProtobufWriter], innerSize: Int)
    extends ProtobufWriter(CodedOutputStream.computeUInt32Size(id, innerSize) + innerSize) {
    def write(using output: CodedOutputStream): Unit = {
      if (id != -1) {
        output.writeTag(id, 2)
        output.writeUInt32NoTag(innerSize)
      }
      var remaining = fields
      while (remaining ne Nil) {
        remaining.head.write
        remaining = remaining.tail
      }
    }
  }

  final case class Repeated(elements: List[ProtobufWriter], id: Int, packed: Boolean, innerSize: Int)
    extends ProtobufWriter(
      if (packed && (elements ne Nil)) CodedOutputStream.computeUInt32Size(id, innerSize) + innerSize
      else innerSize
    ) {
    def write(using output: CodedOutputStream): Unit = {
      if (packed && (elements ne Nil)) {
        output.writeTag(id, 2)
        output.writeUInt32NoTag(innerSize)
      }
      var remaining = elements
      while (remaining ne Nil) {
        remaining.head.write
        remaining = remaining.tail
      }
    }
  }

  final case class Bytes(a: Array[Byte], id: Int)
    extends ProtobufWriter(if (id == -1) CodedOutputStream.computeByteArraySizeNoTag(a) else CodedOutputStream.computeByteArraySize(id, a)) {
    def write(using output: CodedOutputStream): Unit = if (id == -1) output.writeByteArrayNoTag(a) else output.writeByteArray(id, a)
  }
}
