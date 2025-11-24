package proteus
package internal

import java.io.*

import io.grpc.MethodDescriptor.Marshaller
import io.grpc.Status

private[proteus] class ProtobufMarshaller[T](using codec: ProtobufCodec[T]) extends Marshaller[T] {
  def stream(value: T): InputStream =
    try
      new ByteArrayInputStream(codec.encode(value))
    catch {
      case e: Exception => throw Status.INTERNAL.withDescription("Failed to encode protobuf output").withCause(e).asRuntimeException()
    }

  def parse(stream: InputStream): T =
    try
      codec.decode(stream)
    catch {
      case e: Exception => throw Status.INVALID_ARGUMENT.withDescription("Failed to parse protobuf input").withCause(e).asRuntimeException()
    }
}
