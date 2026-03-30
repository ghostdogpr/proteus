package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import io.grpc.*
import io.grpc.stub.*

/**
  * A client backend that uses direct style to return results (no wrapper monad is used).
  * Streaming is not supported.
  */
class DirectClientBackend(channel: Channel) extends ClientBackendUnary[[A] =>> A] {
  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => Response = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    request => {
      val call = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))
      try
        ClientCalls.blockingUnaryCall(call, request)
      catch {
        case ex: StatusRuntimeException => throw ex
        case ex: Exception              => throw Status.INTERNAL.withDescription(ex.getMessage).withCause(ex).asRuntimeException()
      }
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => (Response, Metadata) = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    (request, requestMetadata) => {
      val responseHeaders          = new AtomicReference[Metadata]()
      val responseTrailers         = new AtomicReference[Metadata]()
      val interceptor              = MetadataUtils.newCaptureMetadataInterceptor(responseHeaders, responseTrailers)
      val interceptedChannel       = ClientInterceptors.intercept(channel, interceptor)
      val metadataAttachingChannel = ClientInterceptors.intercept(interceptedChannel, MetadataUtils.newAttachHeadersInterceptor(requestMetadata))

      try {
        val call             = metadataAttachingChannel.newCall(methodDescriptor, options(CallOptions.DEFAULT))
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
