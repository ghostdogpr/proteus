package proteus

import com.google.protobuf.Descriptors.{FileDescriptor, MethodDescriptor => PbMethodDescriptor, ServiceDescriptor => PbServiceDescriptor}
import io.grpc.MethodDescriptor
import io.grpc.MethodDescriptor.MethodType
import io.grpc.protobuf.ProtoMethodDescriptorSupplier

import proteus.internal.ProtobufMarshaller

/**
  * A RPC definition.
  *
  * @param Req the request type.
  * @param Resp the response type.
  */
sealed trait Rpc[Req, Resp] { self =>
  private[proteus] type Request  = Req
  private[proteus] type Response = Resp

  /**
    * The name of the RPC.
    */
  val name: String

  /**
    * An optional comment attached to the RPC.
    */
  val comment: Option[String]

  /**
    * The codec for the request.
    */
  given requestCodec: ProtobufCodec[Request]

  /**
    * The codec for the response.
    */
  given responseCodec: ProtobufCodec[Response]

  /**
    * Converts the RPC to a ProtoIR representation.
    */
  lazy val toProtoIR: ProtoIR.Rpc =
    ProtoIR.Rpc(
      name = name,
      request = ProtoIR.RpcMessage(ProtoIR.Fqn(None, requestCodec.getName)),
      response = ProtoIR.RpcMessage(ProtoIR.Fqn(None, responseCodec.getName)),
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

  /**
    * Converts the messages used in the RPC to ProtoIR top-level definitions.
    */
  private[proteus] def messagesToProtoIR: List[ProtoIR.TopLevelDef] =
    ProtobufCodec.findTopLevelDefs(requestCodec) ++ ProtobufCodec.findTopLevelDefs(responseCodec)

  /**
    * Converts the RPC to a gRPC method descriptor for the reflection service.
    */
  def toMethodDescriptor(service: Service[?]): MethodDescriptor[Request, Response] = {
    val fileDescriptor    = service.fileDescriptor
    val serviceDescriptor = fileDescriptor.findServiceByName(service.name)
    val methodDescriptor  = serviceDescriptor.findMethodByName(name)

    MethodDescriptor
      .newBuilder()
      .setType(self match {
        case _: Rpc.Unary[?, ?]           => MethodType.UNARY
        case _: Rpc.ClientStreaming[?, ?] => MethodType.CLIENT_STREAMING
        case _: Rpc.ServerStreaming[?, ?] => MethodType.SERVER_STREAMING
        case _: Rpc.BidiStreaming[?, ?]   => MethodType.BIDI_STREAMING
      })
      .setFullMethodName(MethodDescriptor.generateFullMethodName(service.packageName.fold(service.name)(s => s"$s.${service.name}"), name))
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
    using _requestCodec: => ProtobufCodec[Request],
    _responseCodec: => ProtobufCodec[Response]
  ) extends Rpc[Request, Response] {
    given requestCodec: ProtobufCodec[Request]   = _requestCodec
    given responseCodec: ProtobufCodec[Response] = _responseCodec
  }
  case class ClientStreaming[Request, Response](name: String, comment: Option[String])(
    using _requestCodec: => ProtobufCodec[Request],
    _responseCodec: => ProtobufCodec[Response]
  ) extends Rpc[Request, Response] {
    given requestCodec: ProtobufCodec[Request]   = _requestCodec
    given responseCodec: ProtobufCodec[Response] = _responseCodec
  }
  case class ServerStreaming[Request, Response](name: String, comment: Option[String])(
    using _requestCodec: => ProtobufCodec[Request],
    _responseCodec: => ProtobufCodec[Response]
  ) extends Rpc[Request, Response] {
    given requestCodec: ProtobufCodec[Request]   = _requestCodec
    given responseCodec: ProtobufCodec[Response] = _responseCodec
  }
  case class BidiStreaming[Request, Response](name: String, comment: Option[String])(
    using _requestCodec: => ProtobufCodec[Request],
    _responseCodec: => ProtobufCodec[Response]
  ) extends Rpc[Request, Response] {
    given requestCodec: ProtobufCodec[Request]   = _requestCodec
    given responseCodec: ProtobufCodec[Response] = _responseCodec
  }

  /**
    * Creates a unary RPC.
    *
    * @param name the name of the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def unary[Request, Response](
    name: String
  )(using requestCodec: => ProtobufCodec[Request], responseCodec: => ProtobufCodec[Response]): Unary[Request, Response] =
    Unary(name, None)(using requestCodec, responseCodec)

  /**
    * Creates a unary RPC with a comment.
    *
    * @param name the name of the RPC.
    * @param comment the comment for the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def unary[Request, Response](name: String, comment: String)(
    using requestCodec: => ProtobufCodec[Request],
    responseCodec: => ProtobufCodec[Response]
  ): Unary[Request, Response] =
    Unary(name, Some(comment))(using requestCodec, responseCodec)

  /**
    * Creates a client streaming RPC.
    *
    * @param name the name of the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def clientStreaming[Request, Response](
    name: String
  )(using requestCodec: => ProtobufCodec[Request], responseCodec: => ProtobufCodec[Response]): ClientStreaming[Request, Response] =
    ClientStreaming(name, None)(using requestCodec, responseCodec)

  /**
    * Creates a client streaming RPC with a comment.
    *
    * @param name the name of the RPC.
    * @param comment the comment for the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def clientStreaming[Request, Response](name: String, comment: String)(
    using requestCodec: => ProtobufCodec[Request],
    responseCodec: => ProtobufCodec[Response]
  ): ClientStreaming[Request, Response] =
    ClientStreaming(name, Some(comment))(using requestCodec, responseCodec)

  /**
    * Creates a server streaming RPC.
    *
    * @param name the name of the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def serverStreaming[Request, Response](
    name: String
  )(using requestCodec: => ProtobufCodec[Request], responseCodec: => ProtobufCodec[Response]): ServerStreaming[Request, Response] =
    ServerStreaming(name, None)(using requestCodec, responseCodec)

  /**
    * Creates a server streaming RPC with a comment.
    *
    * @param name the name of the RPC.
    * @param comment the comment for the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def serverStreaming[Request, Response](name: String, comment: String)(
    using requestCodec: => ProtobufCodec[Request],
    responseCodec: => ProtobufCodec[Response]
  ): ServerStreaming[Request, Response] =
    ServerStreaming(name, Some(comment))(using requestCodec, responseCodec)

  /**
    * Creates a bidirectional streaming RPC.
    *
    * @param name the name of the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def bidiStreaming[Request, Response](
    name: String
  )(using requestCodec: => ProtobufCodec[Request], responseCodec: => ProtobufCodec[Response]): BidiStreaming[Request, Response] =
    BidiStreaming(name, None)(using requestCodec, responseCodec)

  /**
    * Creates a bidirectional streaming RPC with a comment.
    *
    * @param name the name of the RPC.
    * @param comment the comment for the RPC.
    * @param requestCodec the codec for the request.
    * @param responseCodec the codec for the response.
    */
  def bidiStreaming[Request, Response](name: String, comment: String)(
    using requestCodec: => ProtobufCodec[Request],
    responseCodec: => ProtobufCodec[Response]
  ): BidiStreaming[Request, Response] =
    BidiStreaming(name, Some(comment))(using requestCodec, responseCodec)
}
