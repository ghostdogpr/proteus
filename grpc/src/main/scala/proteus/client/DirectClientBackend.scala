package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import io.grpc.*
import io.grpc.stub.*

class DirectClientBackend(channel: Channel) extends ClientBackendUnary[[A] =>> A] {
  def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response], options: CallOptions): Request => Response =
    request => {
      val methodDescriptor = rpc.toMethodDescriptor(service)
      val call             = channel.newCall(methodDescriptor, options)
      try
        ClientCalls.blockingUnaryCall(call, request)
      catch {
        case ex: StatusRuntimeException => throw ex
        case ex: Exception              => throw Status.INTERNAL.withDescription(ex.getMessage).withCause(ex).asRuntimeException()
      }
    }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response],
    options: CallOptions
  ): (Request, Metadata) => (Response, Metadata) = { (request, requestMetadata) =>
    val methodDescriptor         = rpc.toMethodDescriptor(service)
    val responseHeaders          = new AtomicReference[Metadata]()
    val responseTrailers         = new AtomicReference[Metadata]()
    val interceptor              = MetadataUtils.newCaptureMetadataInterceptor(responseHeaders, responseTrailers)
    val interceptedChannel       = ClientInterceptors.intercept(channel, interceptor)
    val metadataAttachingChannel = ClientInterceptors.intercept(interceptedChannel, MetadataUtils.newAttachHeadersInterceptor(requestMetadata))

    try {
      val call             = metadataAttachingChannel.newCall(methodDescriptor, options)
      val response         = ClientCalls.blockingUnaryCall(call, request)
      val combinedMetadata = new Metadata()
      Option(responseHeaders.get()).foreach(combinedMetadata.merge)
      Option(responseTrailers.get()).foreach(combinedMetadata.merge)
      (response, combinedMetadata)
    } catch {
      case ex: StatusRuntimeException => throw ex
      case ex: Exception              => throw Status.INTERNAL.withDescription(ex.getMessage).withCause(ex).asRuntimeException()
    }
  }
}
