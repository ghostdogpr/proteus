package proteus

import java.io.*

import io.grpc.MethodDescriptor.Marshaller

class ProtobufMarshaller[T](using codec: ProtobufCodec[T]) extends Marshaller[T] {
  def stream(value: T): InputStream = new ByteArrayInputStream(codec.encode(value))

  def parse(stream: InputStream): T = codec.decode(stream)
}
