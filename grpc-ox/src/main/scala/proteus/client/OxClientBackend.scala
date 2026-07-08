package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

import io.grpc.*
import io.grpc.stub.{ClientCalls, MetadataUtils}
import ox.{discard, forkDiscard, supervised}
import ox.channels.Channel as OxChannel
import ox.flow.Flow

/**
  * A client backend that uses direct style with Ox for streaming.
  * Unary RPCs return plain values, streaming RPCs use Ox Flow.
  *
  * @param channel the gRPC channel used to issue calls.
  * @param prefetchN initial in-flight response window for server-streaming / bidi RPCs; also sizes the response channel buffer.
  */
class OxClientBackend(channel: Channel, prefetchN: Int) extends ClientBackend[[A] =>> A, Flow] {
  type Tag[A] = NoTag[A]

  private val prefetch: Int = math.max(prefetchN, 1)

  private def responseListener[T](
    responseChannel: OxChannel[T],
    readySignal: OxChannel[Unit],
    trailersRef: AtomicReference[Metadata] = null,
    headersRef: AtomicReference[Metadata] = null
  ): ClientCall.Listener[T] =
    new ClientCall.Listener[T] {
      override def onMessage(message: T): Unit =
        responseChannel.sendOrClosed(message).discard

      override def onClose(status: Status, trailers: Metadata): Unit = {
        if (trailersRef != null) trailersRef.set(trailers)
        if (status.isOk) responseChannel.doneOrClosed().discard
        else responseChannel.errorOrClosed(status.asException(trailers)).discard
        readySignal.doneOrClosed().discard
      }

      override def onHeaders(headers: Metadata): Unit =
        if (headersRef != null) headersRef.set(headers)

      override def onReady(): Unit =
        readySignal.sendOrClosed(()).discard
    }

  final private class CallClosedWhileWaitingException
    extends StatusRuntimeException(Status.CANCELLED.withDescription("Call closed while waiting for readiness"))

  private def awaitReady(call: ClientCall[?, ?], readySignal: OxChannel[Unit]): Unit =
    while (!call.isReady)
      readySignal.receiveOrClosed() match {
        case _: ox.channels.ChannelClosed =>
          throw new CallClosedWhileWaitingException
        case _                            => ()
      }

  private def streamingResponseFlow[T](responseChannel: OxChannel[T], call: ClientCall[?, ?]): Flow[T] =
    Flow.fromSource(responseChannel).tap(_ => call.request(1))

  def client[Rpcs, Request, Response](rpc: Rpc.Unary[Request, Response], service: Service[Rpcs], options: CallOptions => CallOptions)(
    using HasRpc[Rpcs, rpc.type]
  ): Request => Response = {
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

  def client[Rpcs, Request, Response](rpc: Rpc.ClientStreaming[Request, Response], service: Service[Rpcs], options: CallOptions => CallOptions)(
    using HasRpc[Rpcs, rpc.type],
    NoTag[Request]
  ): Flow[Request] => Response = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    requestFlow => {
      val responseChannel = OxChannel.buffered[Response](1)
      val readySignal     = OxChannel.buffered[Unit](1)
      val call            = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

      call.start(responseListener(responseChannel, readySignal), new Metadata())
      call.request(1)

      try {
        requestFlow.runForeach { req =>
          awaitReady(call, readySignal)
          call.sendMessage(req)
        }
        call.halfClose()
      } catch {
        case NonFatal(ex) =>
          call.cancel("Error sending requests", ex)
          throw ex
      }

      Flow.fromSource(responseChannel).runLast()
    }
  }

  def client[Rpcs, Request, Response](rpc: Rpc.ServerStreaming[Request, Response], service: Service[Rpcs], options: CallOptions => CallOptions)(
    using HasRpc[Rpcs, rpc.type],
    NoTag[Response]
  ): Request => Flow[Response] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    request =>
      Flow.usingEmit { emit =>
        val responseChannel = OxChannel.buffered[Response](prefetch)
        val readySignal     = OxChannel.buffered[Unit](1)
        val call            = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

        call.start(responseListener(responseChannel, readySignal), new Metadata())
        call.sendMessage(request)
        call.halfClose()
        call.request(prefetch)

        try streamingResponseFlow(responseChannel, call).runForeach(emit.apply)
        finally call.cancel("Response stream terminated", null)
      }
  }

  def client[Rpcs, Request, Response](rpc: Rpc.BidiStreaming[Request, Response], service: Service[Rpcs], options: CallOptions => CallOptions)(
    using HasRpc[Rpcs, rpc.type],
    NoTag[Request],
    NoTag[Response]
  ): Flow[Request] => Flow[Response] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    requestFlow =>
      Flow.usingEmit { emit =>
        supervised {
          val responseChannel = OxChannel.buffered[Response](prefetch)
          val readySignal     = OxChannel.buffered[Unit](1)
          val senderError     = new AtomicReference[Throwable]()
          val call            = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

          call.start(responseListener(responseChannel, readySignal), new Metadata())
          call.request(prefetch)

          forkDiscard {
            try {
              requestFlow.runForeach { req =>
                awaitReady(call, readySignal)
                call.sendMessage(req)
              }
              call.halfClose()
            } catch {
              case NonFatal(ex) =>
                senderError.set(ex)
                call.cancel("Error sending requests", ex)
            }
          }

          try streamingResponseFlow(responseChannel, call).runForeach(emit.apply)
          catch {
            case NonFatal(ex) =>
              val original = senderError.get()
              throw if (original != null) original else ex
          }
          senderError.get() match {
            case null | _: CallClosedWhileWaitingException => ()
            case ex                                        => throw ex
          }
        }
      }
  }

  def clientWithMetadata[Rpcs, Request, Response](rpc: Rpc.Unary[Request, Response], service: Service[Rpcs], options: CallOptions => CallOptions)(
    using HasRpc[Rpcs, rpc.type]
  ): (Request, Metadata) => (Response, Metadata) = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    (request, requestMetadata) => {
      val responseHeaders          = new AtomicReference[Metadata]()
      val responseTrailers         = new AtomicReference[Metadata]()
      val interceptor              = MetadataUtils.newCaptureMetadataInterceptor(responseHeaders, responseTrailers)
      val interceptedChannel       = ClientInterceptors.intercept(channel, interceptor)
      val metadataAttachingChannel = ClientInterceptors.intercept(interceptedChannel, MetadataUtils.newAttachHeadersInterceptor(requestMetadata))

      try {
        val call     = metadataAttachingChannel.newCall(methodDescriptor, options(CallOptions.DEFAULT))
        val response = ClientCalls.blockingUnaryCall(call, request)
        (response, ClientBackend.mergeMetadata(responseHeaders.get(), responseTrailers.get()))
      } catch {
        case ex: StatusRuntimeException => throw ex
        case ex: Exception              => throw Status.INTERNAL.withDescription(ex.getMessage).withCause(ex).asRuntimeException()
      }
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], NoTag[Request]): (Flow[Request], Metadata) => (Response, Metadata) = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    (requestFlow, requestMetadata) => {
      val responseChannel = OxChannel.buffered[Response](1)
      val readySignal     = OxChannel.buffered[Unit](1)
      val trailersRef     = new AtomicReference[Metadata]()
      val headersRef      = new AtomicReference[Metadata]()
      val call            = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

      call.start(responseListener(responseChannel, readySignal, trailersRef, headersRef), requestMetadata)
      call.request(1)

      try {
        requestFlow.runForeach { req =>
          awaitReady(call, readySignal)
          call.sendMessage(req)
        }
        call.halfClose()
      } catch {
        case NonFatal(ex) =>
          call.cancel("Error sending requests", ex)
          throw ex
      }

      val response = Flow.fromSource(responseChannel).runLast()
      (response, ClientBackend.mergeMetadata(headersRef.get(), trailersRef.get()))
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], NoTag[Response]): (Request, Metadata) => Flow[Response] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    (request, requestMetadata) =>
      Flow.usingEmit { emit =>
        val responseChannel = OxChannel.buffered[Response](prefetch)
        val readySignal     = OxChannel.buffered[Unit](1)
        val call            = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

        call.start(responseListener(responseChannel, readySignal), requestMetadata)
        call.sendMessage(request)
        call.halfClose()
        call.request(prefetch)

        try streamingResponseFlow(responseChannel, call).runForeach(emit.apply)
        finally call.cancel("Response stream terminated", null)
      }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], NoTag[Request], NoTag[Response]): (Flow[Request], Metadata) => Flow[Response] = {
    val methodDescriptor = rpc.toMethodDescriptor(service)
    (requestFlow, requestMetadata) =>
      Flow.usingEmit { emit =>
        supervised {
          val responseChannel = OxChannel.buffered[Response](prefetch)
          val readySignal     = OxChannel.buffered[Unit](1)
          val senderError     = new AtomicReference[Throwable]()
          val call            = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

          call.start(responseListener(responseChannel, readySignal), requestMetadata)
          call.request(prefetch)

          forkDiscard {
            try {
              requestFlow.runForeach { req =>
                awaitReady(call, readySignal)
                call.sendMessage(req)
              }
              call.halfClose()
            } catch {
              case NonFatal(ex) =>
                senderError.set(ex)
                call.cancel("Error sending requests", ex)
            }
          }

          try streamingResponseFlow(responseChannel, call).runForeach(emit.apply)
          catch {
            case NonFatal(ex) =>
              val original = senderError.get()
              throw if (original != null) original else ex
          }
          senderError.get() match {
            case null | _: CallClosedWhileWaitingException => ()
            case ex                                        => throw ex
          }
        }
      }
  }
}

object OxClientBackend {

  /**
    * Creates a new Ox client backend.
    *
    * @param channel the gRPC channel used to issue calls.
    * @param prefetchN initial in-flight response window for streaming RPCs.
    */
  def apply(channel: Channel, prefetchN: Int = 16): OxClientBackend =
    new OxClientBackend(channel, prefetchN)
}
