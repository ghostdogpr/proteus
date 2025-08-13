package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import io.grpc.*
import io.grpc.StatusRuntimeException
import io.grpc.stub.*

trait ClientBackendUnary[Unary[_]] {
  def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response]): Unary[Request => Unary[Response]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response]
  ): Unary[(Request, Metadata) => Unary[(Response, Metadata)]]
}

trait ClientBackend[Unary[_], Streaming[_]] extends ClientBackendUnary[Unary] {
  def client[Request, Response](service: Service[?], rpc: Rpc.ClientStreaming[Request, Response]): Unary[Streaming[Request] => Unary[Response]]
  def client[Request, Response](service: Service[?], rpc: Rpc.ServerStreaming[Request, Response]): Unary[Request => Streaming[Response]]
  def client[Request, Response](service: Service[?], rpc: Rpc.BidiStreaming[Request, Response]): Unary[Streaming[Request] => Streaming[Response]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ClientStreaming[Request, Response]
  ): Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.ServerStreaming[Request, Response]
  ): Unary[(Request, Metadata) => Streaming[Response]]
  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.BidiStreaming[Request, Response]
  ): Unary[(Streaming[Request], Metadata) => Streaming[Response]]
}

object ClientBackend {
  def direct(channel: Channel): ClientBackendUnary[[A] =>> A] = new ClientBackendUnary[[A] =>> A] {
    def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response]): Request => Response =
      request => {
        val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
        val call             = channel.newCall(methodDescriptor, CallOptions.DEFAULT)
        try
          ClientCalls.blockingUnaryCall(call, request)
        catch {
          case ex: StatusRuntimeException => throw ex
          case ex: Exception              => throw Status.INTERNAL.withDescription(ex.getMessage).withCause(ex).asRuntimeException()
        }
      }

    def clientWithMetadata[Request, Response](
      service: Service[?],
      rpc: Rpc.Unary[Request, Response]
    ): (Request, Metadata) => (Response, Metadata) = { (request, requestMetadata) =>
      val methodDescriptor         = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
      val responseHeaders          = new AtomicReference[Metadata]()
      val responseTrailers         = new AtomicReference[Metadata]()
      val interceptor              = MetadataUtils.newCaptureMetadataInterceptor(responseHeaders, responseTrailers)
      val interceptedChannel       = ClientInterceptors.intercept(channel, interceptor)
      val metadataAttachingChannel = ClientInterceptors.intercept(interceptedChannel, MetadataUtils.newAttachHeadersInterceptor(requestMetadata))

      try {
        val call             = metadataAttachingChannel.newCall(methodDescriptor, CallOptions.DEFAULT)
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
}
