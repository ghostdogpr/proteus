package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

import io.grpc.*
import io.grpc.stub.{ClientCalls, MetadataUtils}
import ox.channels.Channel as OxChannel
import ox.discard
import ox.flow.Flow

/**
  * A client backend that uses direct style with Ox for streaming.
  * Unary RPCs return plain values, streaming RPCs use Ox Flow.
  */
class OxClientBackend(channel: Channel) extends ClientBackend[[A] =>> A, Flow] {

  private def responseListener[T](
    responseChannel: OxChannel[T],
    readySignal: OxChannel[Unit],
    trailersRef: AtomicReference[Metadata] = null
  ): ClientCall.Listener[T] =
    new ClientCall.Listener[T] {
      override def onMessage(message: T): Unit =
        responseChannel.sendOrClosed(message).discard

      override def onClose(status: Status, trailers: Metadata): Unit = {
        if (trailersRef != null) trailersRef.set(trailers)
        if (status.isOk) responseChannel.doneOrClosed().discard
        else responseChannel.errorOrClosed(status.asException(trailers)).discard
        readySignal.doneOrClosed().discard // close channel so awaitReady exits the loop
      }

      override def onHeaders(headers: Metadata): Unit = ()

      override def onReady(): Unit =
        readySignal.sendOrClosed(()).discard
    }

  private def awaitReady(call: ClientCall[?, ?], readySignal: OxChannel[Unit]): Unit =
    while (!call.isReady)
      readySignal.receiveOrClosed() match {
        case _: ox.channels.ChannelClosed =>
          throw Status.CANCELLED.withDescription("Call closed while waiting for readiness").asRuntimeException()
        case _                            => ()
      }

  private def streamingResponseFlow[T](responseChannel: OxChannel[T], call: ClientCall[?, ?]): Flow[T] =
    Flow.fromSource(responseChannel).tap(_ => call.request(1))

  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => Response =
    request => {
      val methodDescriptor = rpc.toMethodDescriptor(service)
      val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))
      try
        ClientCalls.blockingUnaryCall(call, request)
      catch {
        case ex: StatusRuntimeException => throw ex
        case ex: Exception              => throw Status.INTERNAL.withDescription(ex.getMessage).withCause(ex).asRuntimeException()
      }
    }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Flow[Request] => Response = { requestFlow =>
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val responseChannel  = OxChannel.buffered[Response](1)
    val readySignal      = OxChannel.buffered[Unit](1)
    val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

    call.start(responseListener(responseChannel, readySignal), new Metadata())
    call.request(1)

    try {
      requestFlow.runForeach { req => awaitReady(call, readySignal); call.sendMessage(req) }
      call.halfClose()
    } catch {
      case NonFatal(ex) =>
        call.cancel("Error sending requests", ex)
        throw ex
    }

    Flow.fromSource(responseChannel).runLast()
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => Flow[Response] = { request =>
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val responseChannel  = OxChannel.buffered[Response](1)
    val readySignal      = OxChannel.buffered[Unit](1)
    val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

    call.start(responseListener(responseChannel, readySignal), new Metadata())
    call.sendMessage(request)
    call.halfClose()
    call.request(1)

    streamingResponseFlow(responseChannel, call)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Flow[Request] => Flow[Response] = { requestFlow =>
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val responseChannel  = OxChannel.buffered[Response](1)
    val readySignal      = OxChannel.buffered[Unit](1)
    val senderError      = new AtomicReference[Throwable]()
    val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

    call.start(responseListener(responseChannel, readySignal), new Metadata())
    call.request(1)

    Thread.startVirtualThread { () =>
      try {
        requestFlow.runForeach { req => awaitReady(call, readySignal); call.sendMessage(req) }
        call.halfClose()
      } catch {
        case NonFatal(ex) =>
          senderError.set(ex)
          call.cancel("Error sending requests", ex)
      }
    }.discard

    Flow.usingEmit { emit =>
      try streamingResponseFlow(responseChannel, call).runForeach(emit.apply)
      catch {
        case NonFatal(ex) =>
          val original = senderError.get()
          throw if (original != null) original else ex
      }
      val ex = senderError.get()
      if (ex != null) throw ex
    }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => (Response, Metadata) = { (request, requestMetadata) =>
    val methodDescriptor         = rpc.toMethodDescriptor(service)
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

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Flow[Request], Metadata) => (Response, Metadata) = { (requestFlow, requestMetadata) =>
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val responseChannel  = OxChannel.buffered[Response](1)
    val readySignal      = OxChannel.buffered[Unit](1)
    val trailersRef      = new AtomicReference[Metadata]()
    val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

    call.start(responseListener(responseChannel, readySignal, trailersRef), requestMetadata)
    call.request(1)

    try {
      requestFlow.runForeach { req => awaitReady(call, readySignal); call.sendMessage(req) }
      call.halfClose()
    } catch {
      case NonFatal(ex) =>
        call.cancel("Error sending requests", ex)
        throw ex
    }

    val response = Flow.fromSource(responseChannel).runLast()
    val trailers = Option(trailersRef.get()).getOrElse(new Metadata())
    (response, trailers)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => Flow[Response] = { (request, requestMetadata) =>
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val responseChannel  = OxChannel.buffered[Response](1)
    val readySignal      = OxChannel.buffered[Unit](1)
    val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

    call.start(responseListener(responseChannel, readySignal), requestMetadata)
    call.sendMessage(request)
    call.halfClose()
    call.request(1)

    streamingResponseFlow(responseChannel, call)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Flow[Request], Metadata) => Flow[Response] = { (requestFlow, requestMetadata) =>
    val methodDescriptor = rpc.toMethodDescriptor(service)
    val responseChannel  = OxChannel.buffered[Response](1)
    val readySignal      = OxChannel.buffered[Unit](1)
    val senderError      = new AtomicReference[Throwable]()
    val call             = channel.newCall(methodDescriptor, options(CallOptions.DEFAULT))

    call.start(responseListener(responseChannel, readySignal), requestMetadata)
    call.request(1)

    Thread.startVirtualThread { () =>
      try {
        requestFlow.runForeach { req => awaitReady(call, readySignal); call.sendMessage(req) }
        call.halfClose()
      } catch {
        case NonFatal(ex) =>
          senderError.set(ex)
          call.cancel("Error sending requests", ex)
      }
    }.discard

    Flow.usingEmit { emit =>
      try streamingResponseFlow(responseChannel, call).runForeach(emit.apply)
      catch {
        case NonFatal(ex) =>
          val original = senderError.get()
          throw if (original != null) original else ex
      }
      val ex = senderError.get()
      if (ex != null) throw ex
    }
  }
}
