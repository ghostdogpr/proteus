package proteus

import com.google.protobuf.Descriptors.{FileDescriptor, MethodDescriptor => PbMethodDescriptor, ServiceDescriptor => PbServiceDescriptor}
import io.grpc.MethodDescriptor
import io.grpc.MethodDescriptor.MethodType
import io.grpc.protobuf.ProtoMethodDescriptorSupplier
import zio.blocks.schema.Schema

sealed trait Rpc[Req, Resp](using requestCodec: ProtobufCodec[Req], responseCodec: ProtobufCodec[Resp]) { self =>
  type Request  = Req
  type Response = Resp

  val name: String

  val toProtoIR: ProtoIR.Rpc =
    ProtoIR.Rpc(
      name = name,
      request = ProtoIR.RpcMessage(ProtoIR.Fqn(None, requestCodec.asInstanceOf[ProtobufCodec.Message[?]].name)),
      response = ProtoIR.RpcMessage(ProtoIR.Fqn(None, responseCodec.asInstanceOf[ProtobufCodec.Message[?]].name)),
      streamingRequest = self match {
        case _: Rpc.ClientStreaming[?, ?] | _: Rpc.BidiStreaming[?, ?] => true
        case _                                                         => false
      },
      streamingResponse = self match {
        case _: Rpc.ServerStreaming[?, ?] | _: Rpc.BidiStreaming[?, ?] => true
        case _                                                         => false
      }
    )

  val messagesToProtoIR: List[ProtoIR.TopLevelDef] =
    ProtobufCodec.toProtoIR(requestCodec) ++ ProtobufCodec.toProtoIR(responseCodec)

  def toMethodDescriptor(serviceName: String, fileDescriptor: FileDescriptor): MethodDescriptor[Request, Response] = {
    val serviceDescriptor = fileDescriptor.findServiceByName(serviceName)
    val methodDescriptor  = serviceDescriptor.findMethodByName(name)

    MethodDescriptor
      .newBuilder()
      .setType(self match {
        case _: Rpc.Unary[?, ?]           => MethodType.UNARY
        case _: Rpc.ClientStreaming[?, ?] => MethodType.CLIENT_STREAMING
        case _: Rpc.ServerStreaming[?, ?] => MethodType.SERVER_STREAMING
        case _: Rpc.BidiStreaming[?, ?]   => MethodType.BIDI_STREAMING
      })
      .setFullMethodName(MethodDescriptor.generateFullMethodName(serviceName, name))
      .setSampledToLocalTracing(true)
      .setRequestMarshaller(ProtobufMarshaller[Request])
      .setResponseMarshaller(ProtobufMarshaller[Response])
      .setSchemaDescriptor(new ProtoMethodDescriptorSupplier {
        def getMethodDescriptor: PbMethodDescriptor   = methodDescriptor
        def getServiceDescriptor: PbServiceDescriptor = serviceDescriptor
        def getFileDescriptor: FileDescriptor         = fileDescriptor
      })
      .build()
  }
}

object Rpc {
  case class Unary[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]
  case class ClientStreaming[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]
  case class ServerStreaming[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]
  case class BidiStreaming[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]

  def unary[Request: Schema, Response: Schema](name: String): Unary[Request, Response]                     =
    Unary(name)(using Schema[Request].derive(ProtobufDeriver), Schema[Response].derive(ProtobufDeriver))
  def clientStreaming[Request: Schema, Response: Schema](name: String): ClientStreaming[Request, Response] =
    ClientStreaming(name)(using Schema[Request].derive(ProtobufDeriver), Schema[Response].derive(ProtobufDeriver))
  def serverStreaming[Request: Schema, Response: Schema](name: String): ServerStreaming[Request, Response] =
    ServerStreaming(name)(using Schema[Request].derive(ProtobufDeriver), Schema[Response].derive(ProtobufDeriver))
  def bidiStreaming[Request: Schema, Response: Schema](name: String): BidiStreaming[Request, Response]     =
    BidiStreaming(name)(using Schema[Request].derive(ProtobufDeriver), Schema[Response].derive(ProtobufDeriver))
}
