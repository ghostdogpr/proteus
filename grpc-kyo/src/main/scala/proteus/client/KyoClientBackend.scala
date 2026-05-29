package proteus
package client

import java.util.concurrent.atomic.AtomicReference

import io.grpc.{Channel as GrpcChannel, *}
import kyo.*
import kyo.AllowUnsafe.embrace.danger

/**
  * A client backend that uses Kyo. Unary RPCs return `A < (Async & Abort[StatusException])`, streaming RPCs use [[kyo.Stream]].
  *
  * @param channel the gRPC channel used to issue calls.
  * @param prefetchN initial in-flight response window for server-streaming / bidi RPCs; also sizes the response channel buffer.
  */
class KyoClientBackend(channel: GrpcChannel, prefetchN: Int)
  extends ClientBackend[[A] =>> A < (Async & Abort[StatusException]), [A] =>> Stream[A, Async & Abort[StatusException]], Tag] {

  private val prefetch: Int = math.max(prefetchN, 1)

  private def newResponseChannel[Response](): Channel.Unsafe[Result[StatusException, Response]] =
    Channel.Unsafe.init[Result[StatusException, Response]](prefetch + 1)

  private class UnaryListener[Response](
    promise: Promise.Unsafe[(Response, Metadata, Metadata), Abort[StatusException]]
  ) extends ClientCall.Listener[Response] {
    private val headersRef = new AtomicReference[Metadata](null)
    private val messageRef = new AtomicReference[Response]()

    override def onHeaders(headers: Metadata): Unit                = headersRef.set(headers)
    override def onMessage(message: Response): Unit                = messageRef.set(message)
    override def onClose(status: Status, trailers: Metadata): Unit =
      if (status.isOk) {
        val msg = messageRef.get()
        if (msg == null) promise.completeDiscard(Result.fail(Status.INTERNAL.withDescription("No data received").asException()))
        else promise.completeDiscard(Result.succeed((msg, headersRef.get(), trailers)))
      } else promise.completeDiscard(Result.fail(new StatusException(status, trailers)))
  }

  private class StreamingListener[Response](
    responseChannel: Channel.Unsafe[Result[StatusException, Response]]
  ) extends ClientCall.Listener[Response] {
    override def onHeaders(headers: Metadata): Unit                = ()
    override def onMessage(message: Response): Unit                = { val _ = responseChannel.offer(Result.succeed(message)) }
    override def onClose(status: Status, trailers: Metadata): Unit = {
      if (!status.isOk) { val _ = responseChannel.offer(Result.fail(new StatusException(status, trailers))) }
      val _ = responseChannel.closeAwaitEmpty()
    }
  }

  final private class ClientReadySignal(call: ClientCall[?, ?]) {
    private val readyEvents = Channel.Unsafe.init[Unit](1)

    val await: Unit < Async =
      Loop.whileTrue(Sync.defer(!call.isReady)) {
        Abort.run[Closed](readyEvents.safe.take).unit
      }

    def signal(): Unit = { val _ = readyEvents.offer(()) }
  }

  private def sendRequests[Request](
    call: ClientCall[Request, ?],
    requestStream: Stream[Request, Async & Abort[StatusException]],
    readySignal: ClientReadySignal
  )(using Tag[Request]): Unit < (Async & Abort[StatusException]) =
    requestStream
      .foreach(req => readySignal.await.andThen(Sync.defer(call.sendMessage(req))))
      .andThen(Sync.defer(call.halfClose()))

  private def responseStream[Response](
    call: ClientCall[?, Response],
    responseChannel: Channel[Result[StatusException, Response]]
  )(using Tag[Response]): Stream[Response, Async & Abort[StatusException] & Scope] =
    Stream.unwrap {
      Scope.ensure(Sync.defer(call.cancel("Stream ended", null))).map { _ =>
        responseChannel.streamUntilClosed(prefetch).map(Abort.get(_)).tap(_ => Sync.defer(call.request(1)))
      }
    }

  private def unaryRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    request: Request
  ): (Response, Metadata, Metadata) < (Async & Abort[StatusException]) =
    Sync.defer {
      val call     = channel.newCall(descriptor, options)
      val promise  = Promise.Unsafe.init[(Response, Metadata, Metadata), Abort[StatusException]]()
      val listener = new UnaryListener[Response](promise)
      Abort
        .catching[Throwable] {
          call.start(listener, headers)
          call.request(1)
          call.sendMessage(request)
          call.halfClose()
        }
        .handle(Abort.run[Throwable])
        .map {
          case Result.Success(_) => Sync.ensure(Sync.defer(call.cancel("Interrupted", null)))(promise.safe.get)
          case Result.Failure(e) => Sync.defer(call.cancel("Failed to send unary request", e)).andThen(Abort.fail(ClientBackend.toStatusException(e)))
          case Result.Panic(e)   => Sync.defer(call.cancel("Failed to send unary request", e)).andThen(Abort.fail(ClientBackend.toStatusException(e)))
        }
    }

  private def clientStreamingRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    requestStream: Stream[Request, Async & Abort[StatusException]]
  )(using Tag[Request]): (Response, Metadata, Metadata) < (Async & Abort[StatusException]) =
    Sync.defer {
      val call        = channel.newCall(descriptor, options)
      val promise     = Promise.Unsafe.init[(Response, Metadata, Metadata), Abort[StatusException]]()
      val readySignal = new ClientReadySignal(call)
      val listener    = new UnaryListener[Response](promise) {
        override def onReady(): Unit = readySignal.signal()
      }
      call.start(listener, headers)
      call.request(1)

      val sender =
        sendRequests(call, requestStream, readySignal).handle(Abort.run[StatusException]).map {
          case Result.Success(_) => ()
          case Result.Failure(e) => Sync.defer { call.cancel("Error sending requests", e); promise.completeDiscard(Result.fail(e)) }
          case Result.Panic(e)   =>
            Sync.defer { call.cancel("Error sending requests", e); promise.completeDiscard(Result.fail(ClientBackend.toStatusException(e))) }
        }
      Fiber.initUnscoped(sender).map { senderFiber =>
        Sync.ensure(senderFiber.interrupt.andThen(Sync.defer(call.cancel("Interrupted", null))))(promise.safe.get)
      }
    }

  private def serverStreamingRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    request: Request
  )(using Tag[Response]): Stream[Response, Async & Abort[StatusException]] =
    Stream
      .unwrap {
        Sync.defer {
          val call            = channel.newCall(descriptor, options)
          val responseChannel = newResponseChannel[Response]()
          val listener        = new StreamingListener[Response](responseChannel)
          call.start(listener, headers)
          call.request(prefetch)
          call.sendMessage(request)
          call.halfClose()
          responseStream(call, responseChannel.safe)
        }
      }
      .handle(Scope.run)

  private def bidiRun[Request, Response](
    descriptor: MethodDescriptor[Request, Response],
    options: CallOptions,
    headers: Metadata,
    requestStream: Stream[Request, Async & Abort[StatusException]]
  )(using Tag[Request], Tag[Response]): Stream[Response, Async & Abort[StatusException]] =
    Stream
      .unwrap {
        Sync.defer {
          val call            = channel.newCall(descriptor, options)
          val responseChannel = newResponseChannel[Response]()
          val readySignal     = new ClientReadySignal(call)
          val listener        = new StreamingListener[Response](responseChannel) {
            override def onReady(): Unit = readySignal.signal()
          }
          call.start(listener, headers)
          call.request(prefetch)

          val sender =
            sendRequests(call, requestStream, readySignal).handle(Abort.run[StatusException]).map {
              case Result.Success(_) => ()
              case Result.Failure(e) => Sync.defer(call.cancel("Error sending requests", e))
              case Result.Panic(e)   => Sync.defer(call.cancel("Error sending requests", e))
            }
          Scope.acquireRelease(Fiber.initUnscoped(sender))(_.interrupt.unit).map(_ => responseStream(call, responseChannel.safe))
        }
      }
      .handle(Scope.run)

  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): Request => Response < (Async & Abort[StatusException]) = {
    val descriptor = rpc.toMethodDescriptor(service)
    req => unaryRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), req).map(_._1)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], Tag[Request]): Stream[Request, Async & Abort[StatusException]] => Response < (Async & Abort[StatusException]) = {
    val descriptor = rpc.toMethodDescriptor(service)
    reqs => clientStreamingRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), reqs).map(_._1)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], Tag[Response]): Request => Stream[Response, Async & Abort[StatusException]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    req => serverStreamingRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), req)
  }

  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(
    using HasRpc[Rpcs, rpc.type],
    Tag[Request],
    Tag[Response]
  ): Stream[Request, Async & Abort[StatusException]] => Stream[Response, Async & Abort[StatusException]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    reqs => bidiRun(descriptor, options(CallOptions.DEFAULT), new Metadata(), reqs)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type]): (Request, Metadata) => (Response, Metadata) < (Async & Abort[StatusException]) = {
    val descriptor = rpc.toMethodDescriptor(service)
    (req, md) =>
      unaryRun(descriptor, options(CallOptions.DEFAULT), md, req).map { case (resp, headers, trailers) =>
        (resp, ClientBackend.mergeMetadata(headers, trailers))
      }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(
    using HasRpc[Rpcs, rpc.type],
    Tag[Request]
  ): (Stream[Request, Async & Abort[StatusException]], Metadata) => (Response, Metadata) < (Async & Abort[StatusException]) = {
    val descriptor = rpc.toMethodDescriptor(service)
    (reqs, md) =>
      clientStreamingRun(descriptor, options(CallOptions.DEFAULT), md, reqs).map { case (resp, headers, trailers) =>
        (resp, ClientBackend.mergeMetadata(headers, trailers))
      }
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(using HasRpc[Rpcs, rpc.type], Tag[Response]): (Request, Metadata) => Stream[Response, Async & Abort[StatusException]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (req, md) => serverStreamingRun(descriptor, options(CallOptions.DEFAULT), md, req)
  }

  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs],
    options: CallOptions => CallOptions
  )(
    using HasRpc[Rpcs, rpc.type],
    Tag[Request],
    Tag[Response]
  ): (Stream[Request, Async & Abort[StatusException]], Metadata) => Stream[Response, Async & Abort[StatusException]] = {
    val descriptor = rpc.toMethodDescriptor(service)
    (reqs, md) => bidiRun(descriptor, options(CallOptions.DEFAULT), md, reqs)
  }
}

object KyoClientBackend {

  /**
    * Creates a new Kyo client backend.
    *
    * @param channel the gRPC channel used to issue calls.
    * @param prefetchN initial in-flight response window for streaming RPCs.
    */
  def apply(channel: GrpcChannel, prefetchN: Int = 16): KyoClientBackend =
    new KyoClientBackend(channel, prefetchN)
}
