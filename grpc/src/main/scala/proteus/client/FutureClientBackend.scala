package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{Future, Promise}

import io.grpc.*
import io.grpc.stub.*

class FutureClientBackend(channel: Channel) extends ClientBackendUnary[Future] {
  def client[Request, Response](service: Service[?], rpc: Rpc.Unary[Request, Response]): Future[Request => Future[Response]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
    Future.successful { request =>
      val promise = Promise[Response]()
      val call    = channel.newCall(methodDescriptor, CallOptions.DEFAULT)

      try {
        val listener = new StreamObserver[Response] {
          def onNext(value: Response): Unit = promise.success(value)
          def onError(t: Throwable): Unit   = promise.failure(t)
          def onCompleted(): Unit           = ()
        }

        io.grpc.stub.ClientCalls.asyncUnaryCall(call, request, listener)
        promise.future
      } catch {
        case ex: Exception => Future.failed(ex)
      }
    }
  }

  def clientWithMetadata[Request, Response](
    service: Service[?],
    rpc: Rpc.Unary[Request, Response]
  ): Future[(Request, Metadata) => Future[(Response, Metadata)]] = {
    val methodDescriptor = rpc.toMethodDescriptor(service.name, service.fileDescriptor)
    Future.successful { (request, requestMetadata) =>
      val promise                  = Promise[(Response, Metadata)]()
      val responseHeaders          = new AtomicReference[Metadata]()
      val responseTrailers         = new AtomicReference[Metadata]()
      val interceptor              = MetadataUtils.newCaptureMetadataInterceptor(responseHeaders, responseTrailers)
      val interceptedChannel       = ClientInterceptors.intercept(channel, interceptor)
      val metadataAttachingChannel = ClientInterceptors.intercept(interceptedChannel, MetadataUtils.newAttachHeadersInterceptor(requestMetadata))
      val call                     = metadataAttachingChannel.newCall(methodDescriptor, CallOptions.DEFAULT)

      val listener = new StreamObserver[Response] {
        def onNext(value: Response): Unit = {
          val combinedMetadata = new Metadata()
          Option(responseHeaders.get()).foreach(combinedMetadata.merge)
          Option(responseTrailers.get()).foreach(combinedMetadata.merge)
          promise.success((value, combinedMetadata))
        }
        def onError(t: Throwable): Unit   = promise.failure(t)
        def onCompleted(): Unit           = ()
      }

      io.grpc.stub.ClientCalls.asyncUnaryCall(call, request, listener)
      promise.future
    }
  }
}
