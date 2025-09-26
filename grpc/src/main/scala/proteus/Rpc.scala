package proteus

import com.google.protobuf.Descriptors.{FileDescriptor, MethodDescriptor => PbMethodDescriptor, ServiceDescriptor => PbServiceDescriptor}
import io.grpc.MethodDescriptor
import io.grpc.MethodDescriptor.MethodType
import io.grpc.protobuf.ProtoMethodDescriptorSupplier

sealed trait Rpc[Req, Resp](using requestCodec: ProtobufCodec[Req], responseCodec: ProtobufCodec[Resp]) { self =>
  type Request  = Req
  type Response = Resp

  val name: String
  val comment: Option[String]

  val toProtoIR: ProtoIR.Rpc =
    ProtoIR.Rpc(
      name = name,
      request = ProtoIR.RpcMessage(ProtoIR.Fqn(None, requestCodec.getName.getOrElse(""))),
      response = ProtoIR.RpcMessage(ProtoIR.Fqn(None, responseCodec.getName.getOrElse(""))),
      streamingRequest = self match {
        case _: Rpc.ClientStreaming[?, ?] | _: Rpc.BidiStreaming[?, ?] => true
        case _                                                         => false
      },
      streamingResponse = self match {
        case _: Rpc.ServerStreaming[?, ?] | _: Rpc.BidiStreaming[?, ?] => true
        case _                                                         => false
      },
      comment = comment
    )

  val messagesToProtoIR: List[ProtoIR.TopLevelDef] =
    ProtobufCodec.toProtoIR(requestCodec) ++ ProtobufCodec.toProtoIR(responseCodec)

  def toMethodDescriptor(serviceName: String, packageName: Option[String], fileDescriptor: FileDescriptor): MethodDescriptor[Request, Response] = {
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
      .setFullMethodName(MethodDescriptor.generateFullMethodName(packageName.fold(serviceName)(s => s"$s.$serviceName"), name))
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
  case class Unary[Request, Response](name: String, comment: Option[String])(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ) extends Rpc[Request, Response]
  case class ClientStreaming[Request, Response](name: String, comment: Option[String])(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ) extends Rpc[Request, Response]
  case class ServerStreaming[Request, Response](name: String, comment: Option[String])(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ) extends Rpc[Request, Response]
  case class BidiStreaming[Request, Response](name: String, comment: Option[String])(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ) extends Rpc[Request, Response]

  def unary[Request, Response](
    name: String
  )(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response]): Unary[Request, Response] =
    Unary(name, None)(using requestCodec, responseCodec)

  def unary[Request, Response](name: String, comment: String)(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): Unary[Request, Response] =
    Unary(name, Some(comment))(using requestCodec, responseCodec)

  def clientStreaming[Request, Response](
    name: String
  )(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response]): ClientStreaming[Request, Response] =
    ClientStreaming(name, None)(using requestCodec, responseCodec)

  def clientStreaming[Request, Response](name: String, comment: String)(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): ClientStreaming[Request, Response] =
    ClientStreaming(name, Some(comment))(using requestCodec, responseCodec)

  def serverStreaming[Request, Response](
    name: String
  )(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response]): ServerStreaming[Request, Response] =
    ServerStreaming(name, None)(using requestCodec, responseCodec)

  def serverStreaming[Request, Response](name: String, comment: String)(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): ServerStreaming[Request, Response] =
    ServerStreaming(name, Some(comment))(using requestCodec, responseCodec)

  def bidiStreaming[Request, Response](
    name: String
  )(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response]): BidiStreaming[Request, Response] =
    BidiStreaming(name, None)(using requestCodec, responseCodec)

  def bidiStreaming[Request, Response](name: String, comment: String)(
    using requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): BidiStreaming[Request, Response] =
    BidiStreaming(name, Some(comment))(using requestCodec, responseCodec)
}
